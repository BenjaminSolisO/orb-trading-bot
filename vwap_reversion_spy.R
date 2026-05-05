# =============================================================================
# BACKTEST: VWAP Reversion — SPY 5min
# Estrategia 2 de 6 | Datos: Massive (ex-Polygon.io)
# =============================================================================
# LOGICA:
#   - VWAP calculado intraday, reset cada dia 09:30 ET
#   - Long:  close < VWAP - BAND_PCT  AND  RSI < RSI_LONG
#   - Short: close > VWAP + BAND_PCT  AND  RSI > RSI_SHORT
#   - Target: precio cruza VWAP desde el lado de entrada
#   - Stop:   STOP_MULT x desviacion desde VWAP al entrar
#   - Max 1 trade por direccion por dia (2 total)
#   - EOD close si no cierra antes
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

VWAP_BAND_PCT <- 0.002    # 0.2% de desviacion para trigger
RSI_PERIOD    <- 14
RSI_LONG      <- 45       # RSI por debajo = condicion long
RSI_SHORT     <- 55       # RSI por encima = condicion short
STOP_MULT     <- 2.0      # stop = STOP_MULT x desviacion_vwap
COMMISSION    <- 0.02     # USD por accion, round trip
SHARES        <- 10

OUTPUT_DIR <- tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) "C:/Users/solis/Downloads/classic strategies"
)

# --- 3. FETCH MASSIVE API ----------------------------------------------------
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
    if (httr::status_code(resp) != 200)
      stop(sprintf("Error HTTP %d: %s", httr::status_code(resp), httr::content(resp, "text")))

    data <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"),
                               simplifyVector = FALSE)
    if (!is.null(data$results) && length(data$results) > 0)
      all_results <- c(all_results, data$results)

    if (!is.null(data$next_url) && nchar(data$next_url) > 0) {
      next_url <- sub("&apiKey=[^&]+", "", data$next_url)
      page     <- page + 1
      Sys.sleep(13)
    } else break
  }

  if (length(all_results) == 0) stop("Sin datos.")
  n <- length(all_results)
  cat(sprintf("  Construyendo data frame (%d velas)...\n", n))

  t_vec <- numeric(n); o_vec <- numeric(n); h_vec <- numeric(n)
  l_vec <- numeric(n); c_vec <- numeric(n); v_vec <- numeric(n)
  for (k in seq_len(n)) {
    r <- all_results[[k]]
    t_vec[k] <- as.numeric(r$t) / 1000
    o_vec[k] <- as.numeric(r$o); h_vec[k] <- as.numeric(r$h)
    l_vec[k] <- as.numeric(r$l); c_vec[k] <- as.numeric(r$c)
    v_vec[k] <- as.numeric(r$v)
  }
  df <- data.frame(
    datetime = as.POSIXct(t_vec, origin = "1970-01-01", tz = "UTC"),
    open = o_vec, high = h_vec, low = l_vec, close = c_vec, volume = v_vec
  )
  df$datetime <- lubridate::with_tz(df$datetime, "America/New_York")
  df[order(df$datetime), ]
}

# --- 4. HELPERS --------------------------------------------------------------
calc_vwap <- function(high, low, close, volume) {
  typical <- (high + low + close) / 3
  cumsum(typical * volume) / cumsum(volume)
}

calc_rsi <- function(closes, n = 14) {
  m <- length(closes)
  rsi_vec <- rep(NA_real_, m)
  if (m < n + 1) return(rsi_vec)
  chg   <- diff(closes)
  gains <- pmax(chg, 0)
  losses <- pmax(-chg, 0)
  avg_g <- mean(gains[1:n])
  avg_l <- mean(losses[1:n])
  for (i in (n + 1):length(chg)) {
    avg_g <- (avg_g * (n - 1) + gains[i]) / n
    avg_l <- (avg_l * (n - 1) + losses[i]) / n
    rs <- if (avg_l == 0) Inf else avg_g / avg_l
    rsi_vec[i + 1] <- 100 - 100 / (1 + rs)
  }
  rsi_vec
}

# --- 5. DATOS ----------------------------------------------------------------
cat(sprintf("Descargando datos Massive: %s | %s a %s\n", SYMBOL, DATE_FROM, DATE_TO))
raw <- fetch_massive_intraday(SYMBOL, DATE_FROM, DATE_TO, POLYGON_KEY)

raw <- raw %>%
  filter(
    format(datetime, "%H:%M") >= "09:30",
    format(datetime, "%H:%M") <= "15:55"
  ) %>%
  mutate(date = as.Date(datetime, tz = "America/New_York"))

cat(sprintf("Velas: %d | Dias: %d\n", nrow(raw), length(unique(raw$date))))
trading_days <- sort(unique(raw$date))

# --- 6. BACKTEST -------------------------------------------------------------
trades <- list()

for (day in as.character(trading_days)) {
  day_data <- raw %>% filter(date == as.Date(day)) %>% arrange(datetime)
  if (nrow(day_data) < RSI_PERIOD + 2) next

  # Calcular VWAP y RSI para el dia
  day_data <- day_data %>%
    mutate(
      vwap = calc_vwap(high, low, close, volume),
      rsi  = calc_rsi(close, RSI_PERIOD)
    )

  long_taken  <- FALSE
  short_taken <- FALSE
  in_trade    <- FALSE
  direction   <- NA
  entry       <- NA; stop_p <- NA; vwap_entry <- NA

  for (i in seq_len(nrow(day_data))) {
    row <- day_data[i, ]
    if (is.na(row$vwap) || is.na(row$rsi)) next

    # --- Gestion de trade abierto ---
    if (in_trade) {
      hits_stop   <- if (direction == "LONG")  row$low  <= stop_p else row$high >= stop_p
      hits_target <- if (direction == "LONG")  row$high >= row$vwap else row$low <= row$vwap

      # Stop primero si misma vela toca ambos
      if (hits_stop) {
        pnl_pts <- if (direction == "LONG") stop_p - entry else entry - stop_p
        pnl_usd <- pnl_pts * SHARES - COMMISSION
        trades[[length(trades) + 1]] <- tibble(
          date = as.Date(day), direction = direction,
          entry = entry, stop = stop_p, vwap_entry = vwap_entry,
          exit = stop_p, result = "stop",
          pnl_pts = pnl_pts, pnl_usd = pnl_usd
        )
        in_trade <- FALSE; direction <- NA
        next
      }
      if (hits_target) {
        exit_p  <- row$vwap
        pnl_pts <- if (direction == "LONG") exit_p - entry else entry - exit_p
        pnl_usd <- pnl_pts * SHARES - COMMISSION
        trades[[length(trades) + 1]] <- tibble(
          date = as.Date(day), direction = direction,
          entry = entry, stop = stop_p, vwap_entry = vwap_entry,
          exit = exit_p, result = "target",
          pnl_pts = pnl_pts, pnl_usd = pnl_usd
        )
        in_trade <- FALSE; direction <- NA
        next
      }

      # EOD
      if (i == nrow(day_data)) {
        exit_p  <- row$close
        pnl_pts <- if (direction == "LONG") exit_p - entry else entry - exit_p
        pnl_usd <- pnl_pts * SHARES - COMMISSION
        trades[[length(trades) + 1]] <- tibble(
          date = as.Date(day), direction = direction,
          entry = entry, stop = stop_p, vwap_entry = vwap_entry,
          exit = exit_p, result = "eod_close",
          pnl_pts = pnl_pts, pnl_usd = pnl_usd
        )
        in_trade <- FALSE
      }
      next
    }

    # --- Buscar nueva entrada ---
    dev <- row$close - row$vwap
    band <- abs(row$vwap) * VWAP_BAND_PCT

    # LONG: precio suficientemente por debajo de VWAP + RSI confirma
    if (!long_taken && dev < -band && row$rsi < RSI_LONG) {
      entry      <- row$close
      vwap_entry <- row$vwap
      deviation  <- abs(dev)
      stop_p     <- entry - STOP_MULT * deviation
      in_trade   <- TRUE
      direction  <- "LONG"
      long_taken <- TRUE
      next
    }

    # SHORT: precio suficientemente por encima de VWAP + RSI confirma
    if (!short_taken && dev > band && row$rsi > RSI_SHORT) {
      entry       <- row$close
      vwap_entry  <- row$vwap
      deviation   <- abs(dev)
      stop_p      <- entry + STOP_MULT * deviation
      in_trade    <- TRUE
      direction   <- "SHORT"
      short_taken <- TRUE
      next
    }
  }
}

results <- bind_rows(trades)
cat(sprintf("\nTotal trades ejecutados: %d\n", nrow(results)))
if (nrow(results) == 0) stop("Sin trades. Ajustar VWAP_BAND_PCT o RSI thresholds.")

results <- results %>% arrange(date) %>% mutate(cum_pnl = cumsum(pnl_usd))

# --- 7. METRICAS -------------------------------------------------------------
wins    <- results %>% filter(pnl_usd > 0)
losses  <- results %>% filter(pnl_usd <= 0)
n_total <- nrow(results)
win_rate      <- nrow(wins) / n_total
avg_win       <- if (nrow(wins) > 0) mean(wins$pnl_usd) else 0
avg_loss      <- if (nrow(losses) > 0) mean(losses$pnl_usd) else 0
profit_factor <- if (sum(losses$pnl_usd) != 0) abs(sum(wins$pnl_usd) / sum(losses$pnl_usd)) else Inf
total_pnl     <- sum(results$pnl_usd)
max_dd        <- min(results$cum_pnl - cummax(results$cum_pnl))

all_days_pnl <- tibble(date = trading_days) %>%
  left_join(results %>% group_by(date) %>% summarise(daily = sum(pnl_usd)), by = "date") %>%
  mutate(daily = if_else(is.na(daily), 0, daily))
sharpe_annual <- mean(all_days_pnl$daily) / sd(all_days_pnl$daily) * sqrt(252)

result_dist <- results %>% count(result) %>% mutate(pct = n / sum(n) * 100)

cat("\n========================================\n")
cat("   RESULTADOS VWAP REVERSION — SPY\n")
cat("========================================\n")
cat(sprintf("Periodo:         %s a %s\n", min(results$date), max(results$date)))
cat(sprintf("Dias evaluados:  %d | Con trade: %d (%.0f%%)\n",
            length(trading_days), length(unique(results$date)),
            length(unique(results$date)) / length(trading_days) * 100))
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

# --- 8. GRAFICOS -------------------------------------------------------------
out <- function(fname) file.path(OUTPUT_DIR, fname)

p1 <- ggplot(results, aes(x = date, y = cum_pnl)) +
  geom_area(fill = "#9C27B0", alpha = 0.12) +
  geom_line(color = "#9C27B0", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title    = "VWAP Reversion — SPY | Curva de Equity",
    subtitle = sprintf("Win Rate: %.1f%% | PF: %.2f | Sharpe: %.2f | Trades: %d",
                       win_rate * 100, profit_factor, sharpe_annual, n_total),
    x = "Fecha", y = "P&L Acumulado (USD)"
  ) +
  scale_y_continuous(labels = dollar_format()) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave(out("vwap_equity_curve.png"), p1, width = 10, height = 5, dpi = 150)

p2 <- ggplot(results, aes(x = pnl_usd, fill = pnl_usd > 0)) +
  geom_histogram(bins = 30, color = "white", alpha = 0.85) +
  scale_fill_manual(values = c("TRUE" = "#4CAF50", "FALSE" = "#F44336"),
                    labels = c("TRUE" = "Win", "FALSE" = "Loss"), name = "") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Distribución de P&L por Trade — VWAP Reversion",
       x = "P&L (USD)", y = "Frecuencia") +
  scale_x_continuous(labels = dollar_format()) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave(out("vwap_pnl_dist.png"), p2, width = 8, height = 4, dpi = 150)

p3 <- results %>%
  group_by(direction) %>%
  summarise(total = sum(pnl_usd), n = n(), wr = mean(pnl_usd > 0)) %>%
  ggplot(aes(x = direction, y = total, fill = total > 0)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = sprintf("$%.0f\n(%d trades, WR %.0f%%)", total, n, wr * 100)),
            vjust = -0.3, size = 5) +
  scale_fill_manual(values = c("TRUE" = "#4CAF50", "FALSE" = "#F44336")) +
  scale_y_continuous(labels = dollar_format(), expand = expansion(mult = c(0.05, 0.25))) +
  labs(title = "P&L por Dirección — VWAP Reversion", x = "", y = "P&L (USD)") +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(face = "bold"))

ggsave(out("vwap_by_direction.png"), p3, width = 9, height = 6, dpi = 150)

# --- 9. EXPORTAR -------------------------------------------------------------
write.csv(results, out("vwap_trades.csv"), row.names = FALSE)

cat("\nArchivos generados en:", OUTPUT_DIR, "\n")
cat("  vwap_equity_curve.png\n  vwap_pnl_dist.png\n  vwap_by_direction.png\n  vwap_trades.csv\n")
cat("Done.\n")
