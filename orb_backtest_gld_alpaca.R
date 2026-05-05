# =============================================================================
# BACKTEST: Opening Range Breakout (ORB) — GLD 5min
# Datos: Alpaca Markets API (IEX feed)
# =============================================================================
# Oro via ETF — commodity con gaps del overnight global
# =============================================================================

pkgs <- c("dplyr", "lubridate", "ggplot2", "scales", "tibble", "jsonlite", "httr")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
invisible(lapply(pkgs, library, character.only = TRUE))

ALPACA_KEY    <- "YOUR_ALPACA_KEY"
ALPACA_SECRET <- "YOUR_ALPACA_SECRET"
ALPACA_BASE   <- "https://data.alpaca.markets/v2/stocks"

SYMBOL        <- "GLD"
DATE_FROM     <- "2017-01-01"
DATE_TO       <- format(Sys.Date() - 1, "%Y-%m-%d")
ORB_CANDLES   <- 6
RR_TARGET     <- 2.0
MIN_RANGE_PCT <- 0.001
VOL_FILTER    <- TRUE
COMMISSION    <- 0.02
SHARES        <- 10

OUTPUT_DIR <- tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) "C:/Users/solis/Downloads/classic strategies"
)

fetch_alpaca_bars <- function(symbol, from, to, timeframe = "5Min") {
  base_url <- sprintf("%s/%s/bars", ALPACA_BASE, symbol)
  all_bars <- list(); page_token <- NULL; page <- 1
  repeat {
    url <- sprintf("%s?timeframe=%s&start=%s&end=%s&limit=10000&adjustment=all&sort=asc&feed=iex",
                   base_url, timeframe, paste0(from, "T00:00:00Z"), paste0(to, "T23:59:59Z"))
    if (!is.null(page_token)) url <- paste0(url, "&page_token=", page_token)
    cat(sprintf("  Pagina %d...", page))
    resp <- httr::GET(url, httr::add_headers(
      "APCA-API-KEY-ID" = ALPACA_KEY, "APCA-API-SECRET-KEY" = ALPACA_SECRET), httr::timeout(30))
    if (httr::status_code(resp) == 429) { cat(" rate limit, esperando 15s...\n"); Sys.sleep(15); next }
    if (httr::status_code(resp) != 200) stop(sprintf("Error HTTP %d", httr::status_code(resp)))
    data <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
    bars <- data$bars
    if (!is.null(bars) && length(bars) > 0) { all_bars <- c(all_bars, bars); cat(sprintf(" %d (total: %d)\n", length(bars), length(all_bars))) }
    else { cat(" 0\n") }
    next_token <- data$next_page_token
    if (!is.null(next_token) && nchar(next_token) > 0) { page_token <- next_token; page <- page + 1 } else { break }
  }
  if (length(all_bars) == 0) stop("Sin datos.")
  n <- length(all_bars); cat(sprintf("  Construyendo data frame (%d velas)...\n", n))
  t_chr <- character(n); o_vec <- numeric(n); h_vec <- numeric(n); l_vec <- numeric(n); c_vec <- numeric(n); v_vec <- numeric(n)
  for (k in seq_len(n)) { r <- all_bars[[k]]; t_chr[k] <- r$t; o_vec[k] <- as.numeric(r$o); h_vec[k] <- as.numeric(r$h); l_vec[k] <- as.numeric(r$l); c_vec[k] <- as.numeric(r$c); v_vec[k] <- as.numeric(r$v) }
  df <- data.frame(datetime = lubridate::ymd_hms(t_chr, tz = "UTC"), open = o_vec, high = h_vec, low = l_vec, close = c_vec, volume = v_vec, stringsAsFactors = FALSE)
  df[order(df$datetime), ]
}

cat(sprintf("Descargando: %s | %s – %s\n", SYMBOL, DATE_FROM, DATE_TO))
raw <- fetch_alpaca_bars(SYMBOL, DATE_FROM, DATE_TO)
cat(sprintf("Velas: %d | %s – %s\n", nrow(raw), format(min(raw$datetime), "%Y-%m-%d"), format(max(raw$datetime), "%Y-%m-%d")))

raw <- raw %>% mutate(datetime = lubridate::with_tz(datetime, "America/New_York")) %>%
  filter(format(datetime, "%H:%M") >= "09:30", format(datetime, "%H:%M") <= "15:55") %>%
  mutate(date = as.Date(datetime, tz = "America/New_York"))
trading_days <- sort(unique(raw$date))
cat(sprintf("Dias trading: %d\n", length(trading_days)))

trades <- list()
for (day in as.character(trading_days)) {
  day_data <- raw %>% filter(date == as.Date(day)) %>% arrange(datetime)
  if (nrow(day_data) < ORB_CANDLES + 1) next
  orb <- day_data[1:ORB_CANDLES, ]; orb_high <- max(orb$high); orb_low <- min(orb$low)
  orb_range <- orb_high - orb_low; orb_mid <- (orb_high + orb_low) / 2
  if (orb_range / orb_mid < MIN_RANGE_PCT) next
  avg_orb_vol <- mean(orb$volume); post_orb <- day_data[(ORB_CANDLES + 1):nrow(day_data), ]; trade_taken <- FALSE
  for (i in seq_len(nrow(post_orb))) {
    if (trade_taken) break
    candle <- post_orb[i, ]; vol_ok <- !VOL_FILTER || (candle$volume > avg_orb_vol)
    if (candle$high > orb_high && vol_ok) {
      entry <- orb_high + 0.01; stop_p <- orb_low; risk <- entry - stop_p; target <- entry + risk * RR_TARGET
      future <- if ((i + 1) <= nrow(post_orb)) post_orb[(i + 1):nrow(post_orb), ] else post_orb[0, ]; result <- "open"; exit_price <- NA
      if (nrow(future) > 0) for (j in seq_len(nrow(future))) {
        if (!is.na(future$low[j]) && future$low[j] <= stop_p) { result <- "stop"; exit_price <- stop_p; break }
        if (!is.na(future$high[j]) && future$high[j] >= target) { result <- "target"; exit_price <- target; break }
      }
      if (result == "open") { exit_price <- tail(day_data$close, 1); result <- "eod_close" }
      trades[[length(trades) + 1]] <- tibble(date = as.Date(day), direction = "LONG",
        entry = entry, stop = stop_p, target = target, exit = exit_price,
        result = result, pnl_pts = exit_price - entry, pnl_usd = (exit_price - entry) * SHARES - COMMISSION, risk_pts = risk, orb_range = orb_range)
      trade_taken <- TRUE
    }
    if (!trade_taken && candle$low < orb_low && vol_ok) {
      entry <- orb_low - 0.01; stop_p <- orb_high; risk <- stop_p - entry; target <- entry - risk * RR_TARGET
      future <- if ((i + 1) <= nrow(post_orb)) post_orb[(i + 1):nrow(post_orb), ] else post_orb[0, ]; result <- "open"; exit_price <- NA
      if (nrow(future) > 0) for (j in seq_len(nrow(future))) {
        if (!is.na(future$high[j]) && future$high[j] >= stop_p) { result <- "stop"; exit_price <- stop_p; break }
        if (!is.na(future$low[j]) && future$low[j] <= target) { result <- "target"; exit_price <- target; break }
      }
      if (result == "open") { exit_price <- tail(day_data$close, 1); result <- "eod_close" }
      trades[[length(trades) + 1]] <- tibble(date = as.Date(day), direction = "SHORT",
        entry = entry, stop = stop_p, target = target, exit = exit_price,
        result = result, pnl_pts = entry - exit_price, pnl_usd = (entry - exit_price) * SHARES - COMMISSION, risk_pts = risk, orb_range = orb_range)
      trade_taken <- TRUE
    }
  }
}

results <- bind_rows(trades); cat(sprintf("\nTrades: %d\n", nrow(results)))
if (nrow(results) == 0) stop("Sin trades.")
results <- results %>% arrange(date) %>% mutate(cum_pnl = cumsum(pnl_usd))
wins <- results %>% filter(pnl_usd > 0); losses <- results %>% filter(pnl_usd <= 0)
n_total <- nrow(results); n_wins <- nrow(wins); win_rate <- n_wins / n_total
avg_win <- if (n_wins > 0) mean(wins$pnl_usd) else 0; avg_loss <- if (nrow(losses) > 0) mean(losses$pnl_usd) else 0
profit_factor <- if (sum(losses$pnl_usd) != 0) abs(sum(wins$pnl_usd) / sum(losses$pnl_usd)) else Inf
total_pnl <- sum(results$pnl_usd); max_dd <- min(results$cum_pnl - cummax(results$cum_pnl))
all_days_pnl <- tibble(date = trading_days) %>% left_join(results %>% group_by(date) %>% summarise(daily = sum(pnl_usd)), by = "date") %>% mutate(daily = if_else(is.na(daily), 0, daily))
sharpe_annual <- mean(all_days_pnl$daily) / sd(all_days_pnl$daily) * sqrt(252)
result_dist <- results %>% count(result) %>% mutate(pct = n / sum(n) * 100)
yearly <- results %>% mutate(year = year(date)) %>% group_by(year) %>% summarise(trades = n(), win_rate = mean(pnl_usd > 0) * 100, pnl = sum(pnl_usd), sharpe = if (n() > 1) mean(pnl_usd) / sd(pnl_usd) * sqrt(252) else NA, max_dd = min(cumsum(pnl_usd) - cummax(cumsum(pnl_usd))), .groups = "drop")

cat("\n========================================\n       RESULTADOS ORB — GLD\n========================================\n")
cat(sprintf("Periodo: %s – %s\n", min(results$date), max(results$date)))
cat(sprintf("Dias: %d | Con trade: %d (%.0f%%)\nTrades: %d\nWR: %.1f%%\nAvg Win: $%.2f | Avg Loss: $%.2f\nPF: %.2f\nP&L: $%.2f\nMaxDD: $%.2f\nSharpe: %.2f\n",
            length(trading_days), n_total, n_total/length(trading_days)*100, n_total,
            win_rate*100, avg_win, avg_loss, profit_factor, total_pnl, max_dd, sharpe_annual))
cat("\nDistribucion:\n"); print(result_dist)
cat("\n--- Por año ---\n"); print(as.data.frame(yearly), row.names = FALSE)

gld_start <- raw %>% filter(date == min(raw$date)) %>% slice(1) %>% pull(close)
gld_end <- raw %>% filter(date == max(raw$date)) %>% slice_tail(n = 1) %>% pull(close)
bh_return <- (gld_end - gld_start) / gld_start * 100; bh_pnl <- (gld_end - gld_start) * SHARES
cat(sprintf("\nB&H: $%.2f -> $%.2f | Ret: %.1f%% | P&L: $%.2f | ORB vs B&H: +$%.2f\n", gld_start, gld_end, bh_return, bh_pnl, total_pnl - bh_pnl))
cat("========================================\n")

out <- function(fname) file.path(OUTPUT_DIR, fname)
p1 <- ggplot(results, aes(x = date, y = cum_pnl)) + geom_area(fill = "#FFD700", alpha = 0.2) + geom_line(color = "#B8860B", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "ORB Backtest — GLD | Curva de Equity", subtitle = sprintf("WR: %.1f%% | PF: %.2f | Sharpe: %.2f | Trades: %d", win_rate*100, profit_factor, sharpe_annual, n_total),
       x = "Fecha", y = "P&L Acumulado (USD)") + scale_y_continuous(labels = dollar_format()) + theme_minimal(base_size = 13) + theme(plot.title = element_text(face = "bold"))
ggsave(out("orb_gld_equity_curve.png"), p1, width = 12, height = 5, dpi = 150)

p3 <- results %>% group_by(direction) %>% summarise(total = sum(pnl_usd), n = n(), wr = mean(pnl_usd > 0)) %>%
  ggplot(aes(x = direction, y = total, fill = total > 0)) + geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = sprintf("$%.0f\n(%d trades, WR %.0f%%)", total, n, wr * 100)), vjust = -0.3, size = 4) +
  scale_fill_manual(values = c("TRUE" = "#4CAF50", "FALSE" = "#F44336")) + scale_y_continuous(labels = dollar_format()) +
  labs(title = "P&L por Direccion — GLD", x = "", y = "P&L (USD)") + theme_minimal(base_size = 13) + theme(plot.title = element_text(face = "bold"))
ggsave(out("orb_gld_by_direction.png"), p3, width = 9, height = 6, dpi = 150)

write.csv(results, out("orb_gld_trades.csv"), row.names = FALSE)
cat("Archivos: orb_gld_equity_curve.png, orb_gld_by_direction.png, orb_gld_trades.csv\nDone.\n")
