# =============================================================================
# BACKTEST: Opening Range Breakout (ORB) — BTC-USD 5min
# Estrategia 1 de 6 | Datos: Massive (ex-Polygon.io)
# =============================================================================
# LOGICA:
#   - "Dia" = medianoche UTC (00:00–23:55 UTC)
#   - Rango ORB: primeras 6 velas de 5min (00:00–00:30 UTC)
#   - Long si precio rompe ORB high con volumen > promedio del rango
#   - Short si precio rompe ORB low con volumen > promedio del rango
#   - Stop: lado opuesto del rango
#   - Target: 2:1 RR (risk = ancho del rango)
#   - Max 1 trade por dia
#   - Sin trade si rango ORB < 0.1%
# FIXES APLICADOS:
#   [1] Entrada = orb_high + 0.01 (long) | orb_low - 0.01 (short)
#   [2] Misma vela toca stop Y target -> stop hit primero
#   [3] Sharpe incluye dias sin trades (P&L = 0)
# =============================================================================

# --- 1. LIBRERIAS ------------------------------------------------------------
pkgs <- c("dplyr", "lubridate", "ggplot2", "scales", "tibble", "jsonlite", "httr")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
invisible(lapply(pkgs, library, character.only = TRUE))

# --- 2. PARAMETROS -----------------------------------------------------------
POLYGON_KEY   <- "YOUR_POLYGON_KEY"
SYMBOL        <- "X:BTCUSD"
DATE_FROM     <- "2024-01-01"
DATE_TO       <- format(Sys.Date() - 1, "%Y-%m-%d")
ORB_CANDLES   <- 6             # 6 x 5min = 30 min de rango desde 00:00 UTC
RR_TARGET     <- 2.0
MIN_RANGE_PCT <- 0.001
VOL_FILTER    <- TRUE
COMMISSION_PCT <- 0.001        # 0.1% round trip (exchange fee tipico crypto)
UNITS         <- 0.1           # BTC por trade

OUTPUT_DIR <- tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) "C:/Users/solis/Downloads/classic strategies"
)

# --- 3. FETCH MASSIVE API (con paginacion) -----------------------------------
fetch_massive_intraday <- function(symbol, from, to, api_key,
                                   multiplier = 5, timespan = "minute") {
  base_url <- sprintf(
    "https://api.massive.com/v2/aggs/ticker/%s/range/%d/%s/%s/%s",
    symbol, multiplier, timespan, from, to
  )

  all_results <- list()
  next_url    <- NULL
  page        <- 1

  repeat {
    url <- if (is.null(next_url)) {
      paste0(base_url, "?adjusted=true&sort=asc&limit=50000&apiKey=", api_key)
    } else {
      paste0(next_url, "&apiKey=", api_key)
    }

    cat(sprintf("  Descargando pagina %d...\n", page))
    resp <- httr::GET(url, httr::timeout(30))

    if (httr::status_code(resp) == 429) {
      cat("  Rate limit, esperando 15s...\n")
      Sys.sleep(15)
      resp <- httr::GET(url, httr::timeout(30))
    }

    if (httr::status_code(resp) != 200) {
      stop(sprintf("Error HTTP %d: %s",
                   httr::status_code(resp),
                   httr::content(resp, "text")))
    }

    data <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"),
                               simplifyVector = FALSE)

    if (!is.null(data$results) && length(data$results) > 0) {
      all_results <- c(all_results, data$results)
    }

    if (!is.null(data$next_url) && nchar(data$next_url) > 0) {
      next_url <- sub("&apiKey=[^&]+", "", data$next_url)
      page     <- page + 1
      Sys.sleep(13)
    } else {
      break
    }
  }

  if (length(all_results) == 0) stop("Sin datos para el rango solicitado.")

  n <- length(all_results)
  cat(sprintf("  Construyendo data frame (%d velas)...\n", n))

  t_vec  <- numeric(n); o_vec <- numeric(n); h_vec <- numeric(n)
  l_vec  <- numeric(n); c_vec <- numeric(n); v_vec <- numeric(n)

  for (k in seq_len(n)) {
    r        <- all_results[[k]]
    t_vec[k] <- as.numeric(r$t) / 1000
    o_vec[k] <- as.numeric(r$o)
    h_vec[k] <- as.numeric(r$h)
    l_vec[k] <- as.numeric(r$l)
    c_vec[k] <- as.numeric(r$c)
    v_vec[k] <- as.numeric(r$v)
  }

  df <- data.frame(
    datetime = as.POSIXct(t_vec, origin = "1970-01-01", tz = "UTC"),
    open = o_vec, high = h_vec, low = l_vec, close = c_vec, volume = v_vec
  )

  df$datetime <- lubridate::with_tz(df$datetime, "America/New_York")
  df <- df[order(df$datetime), ]
  return(df)
}

cat(sprintf("Descargando datos Massive: %s | %s a %s\n", SYMBOL, DATE_FROM, DATE_TO))
raw <- fetch_massive_intraday(SYMBOL, DATE_FROM, DATE_TO, POLYGON_KEY)

cat(sprintf("Velas descargadas: %d | %s a %s\n",
            nrow(raw),
            format(min(raw$datetime), "%Y-%m-%d"),
            format(max(raw$datetime), "%Y-%m-%d")))

# --- 4. FILTRAR HORARIO NY (09:30–15:55 ET) y definir dia --------------------
raw <- raw %>%
  filter(
    format(datetime, "%H:%M") >= "09:30",
    format(datetime, "%H:%M") <= "15:55"
  ) %>%
  mutate(date = as.Date(datetime, tz = "America/New_York"))

trading_days <- sort(unique(raw$date))
cat(sprintf("Dias disponibles: %d\n", length(trading_days)))

# --- 5. BACKTEST -------------------------------------------------------------
trades <- list()

for (day in as.character(trading_days)) {
  day_data <- raw %>% filter(date == as.Date(day)) %>% arrange(datetime)

  if (nrow(day_data) < ORB_CANDLES + 1) next

  orb       <- day_data[1:ORB_CANDLES, ]
  orb_high  <- max(orb$high)
  orb_low   <- min(orb$low)
  orb_range <- orb_high - orb_low
  orb_mid   <- (orb_high + orb_low) / 2

  if (orb_range / orb_mid < MIN_RANGE_PCT) next

  avg_orb_vol <- mean(orb$volume)
  post_orb    <- day_data[(ORB_CANDLES + 1):nrow(day_data), ]
  trade_taken <- FALSE

  for (i in seq_len(nrow(post_orb))) {
    if (trade_taken) break
    candle <- post_orb[i, ]
    vol_ok <- !VOL_FILTER || (candle$volume > avg_orb_vol)

    # --- LONG ---
    if (candle$high > orb_high && vol_ok) {
      entry  <- orb_high + 0.01
      stop_p <- orb_low
      risk   <- entry - stop_p
      target <- entry + risk * RR_TARGET

      future <- if ((i + 1) <= nrow(post_orb)) post_orb[(i + 1):nrow(post_orb), ] else post_orb[0, ]
      result <- "open"; exit_price <- NA

      if (nrow(future) > 0) {
        for (j in seq_len(nrow(future))) {
          if (!is.na(future$low[j])  && future$low[j]  <= stop_p) { result <- "stop";   exit_price <- stop_p; break }
          if (!is.na(future$high[j]) && future$high[j] >= target)  { result <- "target"; exit_price <- target; break }
        }
      }
      if (result == "open") { exit_price <- tail(day_data$close, 1); result <- "eod_close" }

      commission <- entry * UNITS * COMMISSION_PCT
      pnl_usd    <- (exit_price - entry) * UNITS - commission

      trades[[length(trades) + 1]] <- tibble(
        date = as.Date(day), direction = "LONG",
        entry = entry, stop = stop_p, target = target, exit = exit_price,
        result = result, pnl_usd = pnl_usd,
        risk_usd = risk * UNITS, orb_range = orb_range
      )
      trade_taken <- TRUE
    }

    # --- SHORT ---
    if (!trade_taken && candle$low < orb_low && vol_ok) {
      entry  <- orb_low - 0.01
      stop_p <- orb_high
      risk   <- stop_p - entry
      target <- entry - risk * RR_TARGET

      future <- if ((i + 1) <= nrow(post_orb)) post_orb[(i + 1):nrow(post_orb), ] else post_orb[0, ]
      result <- "open"; exit_price <- NA

      if (nrow(future) > 0) {
        for (j in seq_len(nrow(future))) {
          if (!is.na(future$high[j]) && future$high[j] >= stop_p) { result <- "stop";   exit_price <- stop_p; break }
          if (!is.na(future$low[j])  && future$low[j]  <= target)  { result <- "target"; exit_price <- target; break }
        }
      }
      if (result == "open") { exit_price <- tail(day_data$close, 1); result <- "eod_close" }

      commission <- entry * UNITS * COMMISSION_PCT
      pnl_usd    <- (entry - exit_price) * UNITS - commission

      trades[[length(trades) + 1]] <- tibble(
        date = as.Date(day), direction = "SHORT",
        entry = entry, stop = stop_p, target = target, exit = exit_price,
        result = result, pnl_usd = pnl_usd,
        risk_usd = risk * UNITS, orb_range = orb_range
      )
      trade_taken <- TRUE
    }
  }
}

results <- bind_rows(trades)
cat(sprintf("\nTotal trades ejecutados: %d\n", nrow(results)))
if (nrow(results) == 0) stop("Sin trades. Revisar datos o parametros.")

results <- results %>% arrange(date) %>% mutate(cum_pnl = cumsum(pnl_usd))

# --- 6. METRICAS -------------------------------------------------------------
wins    <- results %>% filter(pnl_usd > 0)
losses  <- results %>% filter(pnl_usd <= 0)
n_total <- nrow(results)
n_wins  <- nrow(wins)
win_rate      <- n_wins / n_total
avg_win       <- if (n_wins > 0) mean(wins$pnl_usd) else 0
avg_loss      <- if (nrow(losses) > 0) mean(losses$pnl_usd) else 0
profit_factor <- if (sum(losses$pnl_usd) != 0) abs(sum(wins$pnl_usd) / sum(losses$pnl_usd)) else Inf
total_pnl     <- sum(results$pnl_usd)
max_dd        <- min(results$cum_pnl - cummax(results$cum_pnl))

# FIX [3]: Sharpe con todos los dias (sin trade = 0)
all_days_pnl <- tibble(date = trading_days) %>%
  left_join(results %>% group_by(date) %>% summarise(daily = sum(pnl_usd)), by = "date") %>%
  mutate(daily = if_else(is.na(daily), 0, daily))

sharpe_annual <- mean(all_days_pnl$daily) / sd(all_days_pnl$daily) * sqrt(252)

# Buy & hold BTC: precio inicio vs fin
btc_start <- raw %>% filter(date == min(results$date)) %>% slice(1) %>% pull(close)
btc_end   <- raw %>% filter(date == max(results$date)) %>% slice(n()) %>% pull(close)
bh_pnl    <- (btc_end - btc_start) * UNITS
bh_return <- (btc_end - btc_start) / btc_start * 100

result_dist <- results %>% count(result) %>% mutate(pct = n / sum(n) * 100)

cat("\n========================================\n")
cat("     RESULTADOS ORB BACKTEST — BTC-USD\n")
cat("========================================\n")
cat(sprintf("Periodo:         %s a %s\n", min(results$date), max(results$date)))
cat(sprintf("Dias evaluados:  %d | Con trade: %d (%.0f%%)\n",
            length(trading_days), n_total, n_total / length(trading_days) * 100))
cat(sprintf("Total trades:    %d\n", n_total))
cat(sprintf("Win Rate:        %.1f%%\n", win_rate * 100))
cat(sprintf("Avg Win:         $%.2f\n", avg_win))
cat(sprintf("Avg Loss:        $%.2f\n", avg_loss))
cat(sprintf("Profit Factor:   %.2f\n", profit_factor))
cat(sprintf("Total P&L ORB:   $%.2f (%.1f BTC)\n", total_pnl, UNITS))
cat(sprintf("Max Drawdown:    $%.2f\n", max_dd))
cat(sprintf("Sharpe (anual):  %.2f\n", sharpe_annual))
cat(sprintf("\nBuy & Hold BTC:  $%.2f a $%.2f | P&L: $%.2f (%.1f%%)\n",
            btc_start, btc_end, bh_pnl, bh_return))
cat(sprintf("ORB vs B&H:      %+.1f%%\n",
            (total_pnl - bh_pnl) / abs(bh_pnl) * 100))
cat("\nDistribucion de resultados:\n")
print(result_dist)
cat("========================================\n")

# --- 7. GRAFICOS -------------------------------------------------------------
out <- function(fname) file.path(OUTPUT_DIR, fname)

# 7a. Curva de equity ORB vs Buy & Hold
bh_curve <- tibble(date = trading_days) %>%
  left_join(raw %>% group_by(date) %>% slice(1) %>% select(date, close_open = close),
            by = "date") %>%
  mutate(
    btc_return = (close_open - first(close_open)) / first(close_open),
    bh_pnl_cum = btc_return * btc_start * UNITS
  )

p1 <- ggplot() +
  geom_line(data = results, aes(x = date, y = cum_pnl, color = "ORB"), linewidth = 1) +
  geom_line(data = bh_curve, aes(x = date, y = bh_pnl_cum, color = "Buy & Hold BTC"),
            linewidth = 1, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
  scale_color_manual(values = c("ORB" = "#2196F3", "Buy & Hold BTC" = "#FF9800")) +
  labs(
    title    = "ORB Backtest — BTC-USD | Curva de Equity vs Buy & Hold",
    subtitle = sprintf("Win Rate: %.1f%% | PF: %.2f | Sharpe: %.2f | Trades: %d",
                       win_rate * 100, profit_factor, sharpe_annual, n_total),
    x = "Fecha", y = "P&L Acumulado (USD)", color = ""
  ) +
  scale_y_continuous(labels = dollar_format()) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "top")

ggsave(out("orb_btc_equity_curve.png"), p1, width = 10, height = 5, dpi = 150)

# 7b. Distribucion P&L
p2 <- ggplot(results, aes(x = pnl_usd, fill = pnl_usd > 0)) +
  geom_histogram(bins = 30, color = "white", alpha = 0.85) +
  scale_fill_manual(values = c("TRUE" = "#4CAF50", "FALSE" = "#F44336"),
                    labels = c("TRUE" = "Win", "FALSE" = "Loss"), name = "") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Distribución de P&L por Trade — BTC ORB", x = "P&L (USD)", y = "Frecuencia") +
  scale_x_continuous(labels = dollar_format()) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave(out("orb_btc_pnl_dist.png"), p2, width = 8, height = 4, dpi = 150)

# 7c. Long vs Short
p3 <- results %>%
  group_by(direction) %>%
  summarise(total = sum(pnl_usd), n = n(), wr = mean(pnl_usd > 0)) %>%
  ggplot(aes(x = direction, y = total, fill = total > 0)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = sprintf("$%.0f\n(%d trades, WR %.0f%%)", total, n, wr * 100)),
            vjust = -0.3, size = 5) +
  scale_fill_manual(values = c("TRUE" = "#4CAF50", "FALSE" = "#F44336")) +
  scale_y_continuous(labels = dollar_format(), expand = expansion(mult = c(0.05, 0.25))) +
  labs(title = "P&L Total por Dirección — BTC ORB", x = "", y = "P&L (USD)") +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(face = "bold"))

ggsave(out("orb_btc_by_direction.png"), p3, width = 9, height = 6, dpi = 150)

# --- 8. EXPORTAR -------------------------------------------------------------
write.csv(results, out("orb_btc_trades.csv"), row.names = FALSE)

cat("\nArchivos generados en:", OUTPUT_DIR, "\n")
cat("  orb_btc_equity_curve.png\n  orb_btc_pnl_dist.png\n  orb_btc_by_direction.png\n  orb_btc_trades.csv\n")
cat("Done.\n")
