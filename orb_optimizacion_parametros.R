# =============================================================================
# OPTIMIZACION: Variaciones de parametros ORB — SPY 5min
# Prueba: RR ratio · velas ORB · filtro volumen · min range
# =============================================================================
pkgs <- c("dplyr", "lubridate", "ggplot2", "scales", "tibble", "jsonlite", "httr", "tidyr")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
invisible(lapply(pkgs, library, character.only = TRUE))

ALPACA_KEY    <- "YOUR_ALPACA_KEY"
ALPACA_SECRET <- "YOUR_ALPACA_SECRET"
ALPACA_BASE   <- "https://data.alpaca.markets/v2/stocks"
SYMBOL        <- "SPY"
DATE_FROM     <- "2017-01-01"
DATE_TO       <- format(Sys.Date() - 1, "%Y-%m-%d")
COMMISSION    <- 0.02
SHARES        <- 10
OUTPUT_DIR    <- "C:/Users/solis/Downloads/classic strategies"

# ── Parametros a probar ──
rr_values     <- c(1.5, 2.0, 2.5, 3.0, 3.5, 4.0)
candles_values <- c(4, 6, 8)
vol_filter_values <- c(TRUE, FALSE)
min_range_values  <- c(0.001)

# Crear grid (36 combinaciones, ~3-4 min)
param_grid <- expand.grid(
  rr          = rr_values,
  candles     = candles_values,
  vol_filter  = vol_filter_values,
  min_range   = min_range_values,
  stringsAsFactors = FALSE
)

cat(sprintf("Probando %d combinaciones de parametros...\n", nrow(param_grid)))

# ── 1. FETCH DATA (una sola vez) ──
fetch_alpaca_bars <- function(symbol, from, to, timeframe = "5Min") {
  base_url <- sprintf("%s/%s/bars", ALPACA_BASE, symbol)
  all_bars <- list(); page_token <- NULL; page <- 1
  repeat {
    url <- sprintf("%s?timeframe=%s&start=%s&end=%s&limit=10000&adjustment=all&sort=asc&feed=iex",
                   base_url, timeframe, paste0(from, "T00:00:00Z"), paste0(to, "T23:59:59Z"))
    if (!is.null(page_token)) url <- paste0(url, "&page_token=", page_token)
    cat(sprintf("  Pag %d...", page))
    resp <- httr::GET(url, httr::add_headers("APCA-API-KEY-ID" = ALPACA_KEY, "APCA-API-SECRET-KEY" = ALPACA_SECRET), httr::timeout(30))
    if (httr::status_code(resp) == 429) { cat(" RL\n"); Sys.sleep(15); next }
    if (httr::status_code(resp) != 200) stop(sprintf("Error %d", httr::status_code(resp)))
    data <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
    bars <- data$bars
    if (!is.null(bars) && length(bars) > 0) { all_bars <- c(all_bars, bars); cat(sprintf(" %d\n", length(bars))) }
    else { cat(" 0\n") }
    next_token <- data$next_page_token
    if (!is.null(next_token) && nchar(next_token) > 0) { page_token <- next_token; page <- page + 1 } else { break }
  }
  if (length(all_bars) == 0) stop("Sin datos")
  n <- length(all_bars)
  t_chr <- character(n); o_vec <- numeric(n); h_vec <- numeric(n); l_vec <- numeric(n); c_vec <- numeric(n); v_vec <- numeric(n)
  for (k in seq_len(n)) { r <- all_bars[[k]]; t_chr[k] <- r$t; o_vec[k] <- as.numeric(r$o); h_vec[k] <- as.numeric(r$h); l_vec[k] <- as.numeric(r$l); c_vec[k] <- as.numeric(r$c); v_vec[k] <- as.numeric(r$v) }
  df <- data.frame(datetime = lubridate::ymd_hms(t_chr, tz = "UTC"), open = o_vec, high = h_vec, low = l_vec, close = c_vec, volume = v_vec)
  df[order(df$datetime), ]
}

cat("Descargando SPY...\n")
raw <- fetch_alpaca_bars(SYMBOL, DATE_FROM, DATE_TO)
raw <- raw %>% mutate(datetime = lubridate::with_tz(datetime, "America/New_York")) %>%
  filter(format(datetime, "%H:%M") >= "09:30", format(datetime, "%H:%M") <= "15:55") %>%
  mutate(date = as.Date(datetime, tz = "America/New_York"))

trading_days <- sort(unique(raw$date))
cat(sprintf("%d dias de trading cargados.\n\n", length(trading_days)))

# ── 2. FUNCION DE BACKTEST ──
run_orb <- function(raw, trading_days, orb_candles, rr_target, min_range_pct, vol_filter) {
  trades <- list()
  for (day in as.character(trading_days)) {
    day_data <- raw %>% filter(date == as.Date(day)) %>% arrange(datetime)
    if (nrow(day_data) < orb_candles + 1) next
    orb <- day_data[1:orb_candles, ]; orb_high <- max(orb$high); orb_low <- min(orb$low)
    orb_range <- orb_high - orb_low; orb_mid <- (orb_high + orb_low) / 2
    if (orb_range / orb_mid < min_range_pct) next
    avg_orb_vol <- mean(orb$volume); post_orb <- day_data[(orb_candles + 1):nrow(day_data), ]; trade_taken <- FALSE
    for (i in seq_len(nrow(post_orb))) {
      if (trade_taken) break
      candle <- post_orb[i, ]; vol_ok <- !vol_filter || (candle$volume > avg_orb_vol)
      if (candle$high > orb_high && vol_ok) {
        entry <- orb_high + 0.01; stop_p <- orb_low; risk <- entry - stop_p; target <- entry + risk * rr_target
        future <- if ((i + 1) <= nrow(post_orb)) post_orb[(i + 1):nrow(post_orb), ] else post_orb[0, ]
        result <- "open"; exit_price <- NA
        if (nrow(future) > 0) for (j in seq_len(nrow(future))) {
          if (!is.na(future$low[j]) && future$low[j] <= stop_p) { result <- "stop"; exit_price <- stop_p; break }
          if (!is.na(future$high[j]) && future$high[j] >= target) { result <- "target"; exit_price <- target; break }
        }
        if (result == "open") { exit_price <- tail(day_data$close, 1); result <- "eod_close" }
        trades[[length(trades) + 1]] <- tibble(date = as.Date(day), direction = "LONG",
          pnl_usd = (exit_price - entry) * SHARES - COMMISSION, result = result)
        trade_taken <- TRUE
      }
      if (!trade_taken && candle$low < orb_low && vol_ok) {
        entry <- orb_low - 0.01; stop_p <- orb_high; risk <- stop_p - entry; target <- entry - risk * rr_target
        future <- if ((i + 1) <= nrow(post_orb)) post_orb[(i + 1):nrow(post_orb), ] else post_orb[0, ]
        result <- "open"; exit_price <- NA
        if (nrow(future) > 0) for (j in seq_len(nrow(future))) {
          if (!is.na(future$high[j]) && future$high[j] >= stop_p) { result <- "stop"; exit_price <- stop_p; break }
          if (!is.na(future$low[j]) && future$low[j] <= target) { result <- "target"; exit_price <- target; break }
        }
        if (result == "open") { exit_price <- tail(day_data$close, 1); result <- "eod_close" }
        trades[[length(trades) + 1]] <- tibble(date = as.Date(day), direction = "SHORT",
          pnl_usd = (entry - exit_price) * SHARES - COMMISSION, result = result)
        trade_taken <- TRUE
      }
    }
  }
  results <- bind_rows(trades)
  if (nrow(results) == 0) {
    return(c(trades = 0, win_rate = NA, avg_win = NA, avg_loss = NA, pf = NA, pnl = 0, max_dd = NA, sharpe = NA, target_pct = NA, stop_pct = NA, eod_pct = NA))
  }
  results <- results %>% arrange(date)
  wins   <- sum(results$pnl_usd > 0); losses <- sum(results$pnl_usd <= 0); n_t <- nrow(results)
  wr     <- wins / n_t * 100
  avg_w  <- if (wins > 0) mean(results$pnl_usd[results$pnl_usd > 0]) else 0
  avg_l  <- if (losses > 0) mean(results$pnl_usd[results$pnl_usd <= 0]) else 0
  pf     <- if (sum(results$pnl_usd[results$pnl_usd <= 0]) != 0) abs(sum(results$pnl_usd[results$pnl_usd > 0]) / sum(results$pnl_usd[results$pnl_usd <= 0])) else Inf
  pnl_total <- sum(results$pnl_usd)
  cum_pnl   <- cumsum(results$pnl_usd)
  max_dd    <- min(cum_pnl - cummax(cum_pnl))
  
  # Sharpe
  all_days <- tibble(date = trading_days) %>% left_join(results %>% group_by(date) %>% summarise(daily = sum(pnl_usd)), by = "date") %>% mutate(daily = if_else(is.na(daily), 0, daily))
  sh <- mean(all_days$daily) / sd(all_days$daily) * sqrt(252)
  
  # Distribution
  n <- n_t
  tgt_pct <- sum(results$result == "target") / n * 100
  stp_pct <- sum(results$result == "stop") / n * 100
  eod_pct <- sum(results$result == "eod_close") / n * 100
  
  c(trades = n_t, win_rate = wr, avg_win = avg_w, avg_loss = avg_l, pf = pf, pnl = pnl_total, max_dd = max_dd, sharpe = sh, target_pct = tgt_pct, stop_pct = stp_pct, eod_pct = eod_pct)
}

# ── 3. EJECUTAR GRID ──
cat("Corriendo backtests...\n")
pb <- txtProgressBar(min = 0, max = nrow(param_grid), style = 3)

results_list <- list()
for (i in 1:nrow(param_grid)) {
  p <- param_grid[i, ]
  stats <- run_orb(raw, trading_days, p$candles, p$rr, p$min_range, p$vol_filter)
  results_list[[i]] <- data.frame(
    rr = p$rr, candles = p$candles, vol_filter = p$vol_filter, min_range = p$min_range,
    trades = stats["trades"], win_rate = stats["win_rate"], avg_win = stats["avg_win"],
    avg_loss = stats["avg_loss"], pf = stats["pf"], pnl = stats["pnl"],
    max_dd = stats["max_dd"], sharpe = stats["sharpe"],
    target_pct = stats["target_pct"], stop_pct = stats["stop_pct"], eod_pct = stats["eod_pct"]
  )
  setTxtProgressBar(pb, i)
}
close(pb)
all_results <- bind_rows(results_list)

# ── 4. MOSTRAR RESULTADOS ──

# Por RR ratio (promediando sobre otros parametros)
rr_summary <- all_results %>%
  group_by(rr) %>%
  summarise(
    n_combos = n(),
    avg_pnl = mean(pnl), best_pnl = max(pnl), worst_pnl = min(pnl),
    avg_sharpe = mean(sharpe, na.rm = TRUE), best_sharpe = max(sharpe, na.rm = TRUE),
    avg_wr = mean(win_rate, na.rm = TRUE), best_wr = max(win_rate, na.rm = TRUE),
    avg_pf = mean(pf, na.rm = TRUE), best_pf = max(pf, na.rm = TRUE),
    avg_dd = mean(max_dd, na.rm = TRUE),
    avg_target = mean(target_pct, na.rm = TRUE), avg_stop = mean(stop_pct, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n\n========================================\n")
cat("   OPTIMIZACION ORB SPY — POR RR RATIO\n")
cat("   (promedio sobre velas, filtro vol, min range)\n")
cat("========================================\n")
rr_summary %>% 
  mutate(across(c(avg_pnl, avg_sharpe, avg_wr, avg_pf, avg_target, avg_stop), ~round(., 1))) %>%
  as.data.frame() %>% print(row.names = FALSE)

# Top 10 combinaciones por P&L
cat("\n--- Top 10 combinaciones por P&L ---\n")
top10 <- all_results %>% arrange(desc(pnl)) %>% head(10)
top10 %>% select(rr, candles, vol_filter, min_range, pnl, sharpe, win_rate, pf, max_dd, trades) %>%
  mutate(across(c(pnl, sharpe, win_rate, pf), ~round(., 1))) %>%
  as.data.frame() %>% print(row.names = FALSE)

# Top 10 por Sharpe
cat("\n--- Top 10 combinaciones por Sharpe ---\n")
top10_sh <- all_results %>% arrange(desc(sharpe)) %>% head(10)
top10_sh %>% select(rr, candles, vol_filter, min_range, sharpe, pnl, win_rate, pf, max_dd) %>%
  mutate(across(c(sharpe, pnl, win_rate, pf), ~round(., 1))) %>%
  as.data.frame() %>% print(row.names = FALSE)

# ── 5. Por velas ORB ──
cat("\n--- Por numero de velas ORB (promedio) ---\n")
candles_summary <- all_results %>%
  group_by(candles) %>%
  summarise(avg_pnl = mean(pnl), avg_sharpe = mean(sharpe, na.rm = TRUE), avg_wr = mean(win_rate, na.rm = TRUE), avg_pf = mean(pf, na.rm = TRUE), .groups = "drop")
print(as.data.frame(candles_summary %>% mutate(across(where(is.numeric), ~round(., 1)))), row.names = FALSE)

# ── 6. Por filtro de volumen ──
cat("\n--- Por filtro de volumen ---\n")
vol_summary <- all_results %>%
  group_by(vol_filter) %>%
  summarise(avg_pnl = mean(pnl), avg_sharpe = mean(sharpe, na.rm = TRUE), avg_wr = mean(win_rate, na.rm = TRUE), avg_pf = mean(pf, na.rm = TRUE), .groups = "drop")
print(as.data.frame(vol_summary %>% mutate(across(where(is.numeric), ~round(., 1)))), row.names = FALSE)

# ── 7. GRAFICOS ──
out <- function(fname) file.path(OUTPUT_DIR, fname)

# P&L vs RR
p1 <- ggplot(rr_summary, aes(x = factor(rr))) +
  geom_col(aes(y = avg_pnl), fill = "#2196F3", alpha = 0.7, width = 0.6) +
  geom_point(aes(y = best_pnl), color = "#4CAF50", size = 3) +
  geom_point(aes(y = worst_pnl), color = "#F44336", size = 3) +
  geom_text(aes(y = avg_pnl, label = sprintf("$%.0f", avg_pnl)), vjust = -0.8, size = 3.5) +
  labs(title = "P&L Promedio por RR Ratio", subtitle = "Verde = mejor por RR | Rojo = peor por RR",
       x = "RR Ratio", y = "P&L (USD)") +
  scale_y_continuous(labels = dollar_format()) + theme_minimal(base_size = 13)
ggsave(out("opt_rr_vs_pnl.png"), p1, width = 8, height = 4.5, dpi = 150)

# Sharpe vs RR
p2 <- ggplot(rr_summary, aes(x = factor(rr))) +
  geom_col(aes(y = avg_sharpe), fill = "#7B1FA2", alpha = 0.7, width = 0.6) +
  geom_text(aes(y = avg_sharpe, label = sprintf("%.1f", avg_sharpe)), vjust = -0.8, size = 3.5) +
  labs(title = "Sharpe Promedio por RR Ratio", x = "RR Ratio", y = "Sharpe (anual)") +
  theme_minimal(base_size = 13)
ggsave(out("opt_rr_vs_sharpe.png"), p2, width = 8, height = 4.5, dpi = 150)

# Heatmap: RR × Candles → P&L
heat_data <- all_results %>%
  group_by(rr, candles) %>%
  summarise(pnl = mean(pnl), .groups = "drop")

p3 <- ggplot(heat_data, aes(x = factor(rr), y = factor(candles), fill = pnl)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = sprintf("$%.0f", pnl)), size = 4.5, fontface = "bold") +
  scale_fill_gradient2(low = "#E53935", mid = "#FFF9C4", high = "#43A047", midpoint = median(heat_data$pnl)) +
  labs(title = "P&L Promedio: RR Ratio vs Velas ORB", x = "RR Ratio", y = "Velas ORB") +
  theme_minimal(base_size = 14) + theme(plot.title = element_text(face = "bold"), legend.position = "none")
ggsave(out("opt_heatmap_rr_candles.png"), p3, width = 8, height = 5, dpi = 150)

# Win Rate y Stop Rate vs RR
p4 <- rr_summary %>%
  select(rr, avg_wr, avg_stop, avg_target) %>%
  pivot_longer(-rr, names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = factor(rr), y = value, color = metric, group = metric)) +
  geom_line(linewidth = 1.2) + geom_point(size = 3) +
  scale_color_manual(values = c("avg_wr" = "#4CAF50", "avg_stop" = "#F44336", "avg_target" = "#2196F3"),
                     labels = c("avg_wr" = "Win Rate", "avg_stop" = "Stop Rate", "avg_target" = "Target Rate")) +
  labs(title = "Win/Stop/Target Rate vs RR Ratio", x = "RR Ratio", y = "%", color = "") +
  theme_minimal(base_size = 13) + theme(legend.position = "bottom")
ggsave(out("opt_rr_vs_rates.png"), p4, width = 8, height = 4.5, dpi = 150)

cat("\nGraficos guardados en:", OUTPUT_DIR, "\n")
cat("  opt_rr_vs_pnl.png\n  opt_rr_vs_sharpe.png\n  opt_heatmap_rr_candles.png\n  opt_rr_vs_rates.png\n")
cat("\nDone.\n")
