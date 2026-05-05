# =============================================================================
# BACKTEST: Opening Range Breakout (ORB) — SPY 5min
# Estrategia 1 de 6 | Datos: Polygon.io API
# =============================================================================
# LOGICA:
#   - Rango ORB: primeras 6 velas de 5min (09:30–10:00 ET)
#   - Long si precio rompe ORB high con volumen > promedio del rango
#   - Short si precio rompe ORB low con volumen > promedio del rango
#   - Stop: lado opuesto del rango
#   - Target: 2:1 RR (risk = ancho del rango)
#   - Max 1 trade por dia, solo en la direccion de la ruptura
#   - Sin trade si el rango ORB < 0.1%
# FIXES APLICADOS:
#   [1] Entrada = orb_high + 0.01 (long) | orb_low - 0.01 (short)
#   [2] Misma vela toca stop Y target -> stop hit primero (conservador)
#   [3] Sharpe incluye dias sin trades (P&L = 0) en el denominador
# =============================================================================

# --- 1. LIBRERIAS ------------------------------------------------------------
pkgs <- c("dplyr", "lubridate", "ggplot2", "scales", "tibble", "jsonlite", "httr")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
invisible(lapply(pkgs, library, character.only = TRUE))

# --- 2. PARAMETROS -----------------------------------------------------------
POLYGON_KEY   <- "YOUR_POLYGON_KEY"
SYMBOL        <- "SPY"
DATE_FROM     <- "2024-01-01"
DATE_TO       <- format(Sys.Date() - 1, "%Y-%m-%d")
ORB_CANDLES   <- 6             # 6 x 5min = 30 min de rango
RR_TARGET     <- 2.0
MIN_RANGE_PCT <- 0.001
VOL_FILTER    <- TRUE
COMMISSION    <- 0.02          # USD por accion, ida+vuelta
SHARES        <- 10

OUTPUT_DIR <- tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) "C:/Users/solis/Downloads/classic strategies"
)

# --- 3. FETCH POLYGON.IO (con paginacion) ------------------------------------
fetch_polygon_intraday <- function(symbol, from, to, api_key,
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
    resp <- httr::GET(url)

    if (httr::status_code(resp) == 429) {
      cat("  Rate limit alcanzado, esperando 15s...\n")
      Sys.sleep(15)
      resp <- httr::GET(url)
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

    # Paginacion: Polygon devuelve next_url si hay mas datos
    if (!is.null(data$next_url) && nchar(data$next_url) > 0) {
      next_url <- sub("&apiKey=[^&]+", "", data$next_url)
      page     <- page + 1
      Sys.sleep(13)  # free tier: 5 req/min
    } else {
      break
    }
  }

  if (length(all_results) == 0) stop("Sin datos de Polygon.io para el rango solicitado.")

  df <- do.call(rbind, lapply(all_results, function(r) {
    data.frame(
      datetime = as.POSIXct(as.numeric(r$t) / 1000, origin = "1970-01-01", tz = "UTC"),
      open     = as.numeric(r$o),
      high     = as.numeric(r$h),
      low      = as.numeric(r$l),
      close    = as.numeric(r$c),
      volume   = as.numeric(r$v),
      stringsAsFactors = FALSE
    )
  }))

  df$datetime <- lubridate::with_tz(df$datetime, "America/New_York")
  df <- df[order(df$datetime), ]
  return(df)
}

cat(sprintf("Descargando datos Massive: %s | %s a %s\n", SYMBOL, DATE_FROM, DATE_TO))
raw <- fetch_polygon_intraday(SYMBOL, DATE_FROM, DATE_TO, POLYGON_KEY)

cat(sprintf("Velas descargadas: %d | %s a %s\n",
            nrow(raw),
            format(min(raw$datetime), "%Y-%m-%d"),
            format(max(raw$datetime), "%Y-%m-%d")))

# --- 4. FILTRAR HORARIO (09:30–15:55 ET) -------------------------------------
raw <- raw %>%
  filter(
    format(datetime, "%H:%M") >= "09:30",
    format(datetime, "%H:%M") <= "15:55"
  ) %>%
  mutate(date = as.Date(datetime, tz = "America/New_York"))

trading_days <- sort(unique(raw$date))
cat(sprintf("Dias de trading: %d\n", length(trading_days)))

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
    # FIX [1]: entrada = orb_high + 0.01, no close del candle
    if (candle$high > orb_high && vol_ok) {
      entry  <- orb_high + 0.01
      stop_p <- orb_low
      risk   <- entry - stop_p
      target <- entry + risk * RR_TARGET

      future <- if ((i + 1) <= nrow(post_orb)) post_orb[(i + 1):nrow(post_orb), ] else post_orb[0, ]
      result     <- "open"
      exit_price <- NA

      if (nrow(future) > 0) {
        for (j in seq_len(nrow(future))) {
          hits_stop   <- !is.na(future$low[j])  && future$low[j]  <= stop_p
          hits_target <- !is.na(future$high[j]) && future$high[j] >= target
          # FIX [2]: misma vela toca ambos -> stop primero
          if (hits_stop) {
            result <- "stop"; exit_price <- stop_p; break
          }
          if (hits_target) {
            result <- "target"; exit_price <- target; break
          }
        }
      }

      if (result == "open") {
        exit_price <- tail(day_data$close, 1)
        result     <- "eod_close"
      }

      pnl_pts <- exit_price - entry
      pnl_usd <- pnl_pts * SHARES - COMMISSION

      trades[[length(trades) + 1]] <- tibble(
        date = as.Date(day), direction = "LONG",
        entry = entry, stop = stop_p, target = target, exit = exit_price,
        result = result, pnl_pts = pnl_pts, pnl_usd = pnl_usd,
        risk_pts = risk, orb_range = orb_range
      )
      trade_taken <- TRUE
    }

    # --- SHORT ---
    # FIX [1]: entrada = orb_low - 0.01
    if (!trade_taken && candle$low < orb_low && vol_ok) {
      entry  <- orb_low - 0.01
      stop_p <- orb_high
      risk   <- stop_p - entry
      target <- entry - risk * RR_TARGET

      future <- if ((i + 1) <= nrow(post_orb)) post_orb[(i + 1):nrow(post_orb), ] else post_orb[0, ]
      result     <- "open"
      exit_price <- NA

      if (nrow(future) > 0) {
        for (j in seq_len(nrow(future))) {
          hits_stop   <- !is.na(future$high[j]) && future$high[j] >= stop_p
          hits_target <- !is.na(future$low[j])  && future$low[j]  <= target
          # FIX [2]: stop primero si misma vela toca ambos
          if (hits_stop) {
            result <- "stop"; exit_price <- stop_p; break
          }
          if (hits_target) {
            result <- "target"; exit_price <- target; break
          }
        }
      }

      if (result == "open") {
        exit_price <- tail(day_data$close, 1)
        result     <- "eod_close"
      }

      pnl_pts <- entry - exit_price
      pnl_usd <- pnl_pts * SHARES - COMMISSION

      trades[[length(trades) + 1]] <- tibble(
        date = as.Date(day), direction = "SHORT",
        entry = entry, stop = stop_p, target = target, exit = exit_price,
        result = result, pnl_pts = pnl_pts, pnl_usd = pnl_usd,
        risk_pts = risk, orb_range = orb_range
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
win_rate <- n_wins / n_total
avg_win  <- if (n_wins > 0) mean(wins$pnl_usd) else 0
avg_loss <- if (nrow(losses) > 0) mean(losses$pnl_usd) else 0
profit_factor <- if (sum(losses$pnl_usd) != 0) abs(sum(wins$pnl_usd) / sum(losses$pnl_usd)) else Inf
total_pnl <- sum(results$pnl_usd)
max_dd    <- min(results$cum_pnl - cummax(results$cum_pnl))

# FIX [3]: Sharpe incluye todos los dias de trading (sin trade = P&L 0)
all_days_pnl <- tibble(date = trading_days) %>%
  left_join(results %>% group_by(date) %>% summarise(daily = sum(pnl_usd)),
            by = "date") %>%
  mutate(daily = if_else(is.na(daily), 0, daily))

sharpe_annual <- mean(all_days_pnl$daily) / sd(all_days_pnl$daily) * sqrt(252)

result_dist <- results %>% count(result) %>% mutate(pct = n / sum(n) * 100)

cat("\n========================================\n")
cat("       RESULTADOS ORB BACKTEST — SPY\n")
cat("========================================\n")
cat(sprintf("Periodo:         %s a %s\n", min(results$date), max(results$date)))
cat(sprintf("Dias evaluados:  %d | Con trade: %d (%.0f%%)\n",
            length(trading_days), n_total,
            n_total / length(trading_days) * 100))
cat(sprintf("Total trades:    %d\n", n_total))
cat(sprintf("Win Rate:        %.1f%%\n", win_rate * 100))
cat(sprintf("Avg Win:         $%.2f\n", avg_win))
cat(sprintf("Avg Loss:        $%.2f\n", avg_loss))
cat(sprintf("Profit Factor:   %.2f\n", profit_factor))
cat(sprintf("Total P&L:       $%.2f\n", total_pnl))
cat(sprintf("Max Drawdown:    $%.2f\n", max_dd))
cat(sprintf("Sharpe (anual):  %.2f\n", sharpe_annual))
cat("\nDistribucion de resultados:\n")
print(result_dist)
cat("========================================\n")

# --- 7. GRAFICOS -------------------------------------------------------------
out <- function(fname) file.path(OUTPUT_DIR, fname)

# 7a. Curva de equity
p1 <- ggplot(results, aes(x = date, y = cum_pnl)) +
  geom_area(fill = "#2196F3", alpha = 0.12) +
  geom_line(color = "#2196F3", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title    = "ORB Backtest — SPY | Curva de Equity",
    subtitle = sprintf("Win Rate: %.1f%% | PF: %.2f | Sharpe: %.2f | Trades: %d",
                       win_rate * 100, profit_factor, sharpe_annual, n_total),
    x = "Fecha", y = "P&L Acumulado (USD)"
  ) +
  scale_y_continuous(labels = dollar_format()) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave(out("orb_equity_curve.png"), p1, width = 10, height = 5, dpi = 150)

# 7b. Distribucion P&L por trade
p2 <- ggplot(results, aes(x = pnl_usd, fill = pnl_usd > 0)) +
  geom_histogram(bins = 30, color = "white", alpha = 0.85) +
  scale_fill_manual(values = c("TRUE" = "#4CAF50", "FALSE" = "#F44336"),
                    labels = c("TRUE" = "Win", "FALSE" = "Loss"), name = "") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Distribución de P&L por Trade", x = "P&L (USD)", y = "Frecuencia") +
  scale_x_continuous(labels = dollar_format()) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave(out("orb_pnl_dist.png"), p2, width = 8, height = 4, dpi = 150)

# 7c. P&L Long vs Short
p3 <- results %>%
  group_by(direction) %>%
  summarise(total = sum(pnl_usd), n = n(), wr = mean(pnl_usd > 0)) %>%
  ggplot(aes(x = direction, y = total, fill = total > 0)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = sprintf("$%.0f\n(%d trades, WR %.0f%%)", total, n, wr * 100)),
            vjust = -0.3, size = 4) +
  scale_fill_manual(values = c("TRUE" = "#4CAF50", "FALSE" = "#F44336")) +
  scale_y_continuous(labels = dollar_format()) +
  labs(title = "P&L Total por Dirección", x = "", y = "P&L (USD)") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave(out("orb_by_direction.png"), p3, width = 9, height = 6, dpi = 150)

# --- 8. EXPORTAR -------------------------------------------------------------
write.csv(results, out("orb_trades.csv"), row.names = FALSE)

cat("\nArchivos generados en:", OUTPUT_DIR, "\n")
cat("  orb_equity_curve.png\n  orb_pnl_dist.png\n  orb_by_direction.png\n  orb_trades.csv\n")
cat("Done.\n")
