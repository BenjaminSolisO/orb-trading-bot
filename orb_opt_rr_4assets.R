# =============================================================================
# OPTIMIZACION RR Ratio — QQQ + IWM + GLD (SPY ya fue)
# =============================================================================
pkgs <- c("dplyr", "lubridate", "ggplot2", "scales", "tibble", "jsonlite", "httr", "tidyr")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
invisible(lapply(pkgs, library, character.only = TRUE))

ALPACA_KEY    <- "YOUR_ALPACA_KEY"
ALPACA_SECRET <- "YOUR_ALPACA_SECRET"
ALPACA_BASE   <- "https://data.alpaca.markets/v2/stocks"
OUTPUT_DIR    <- "C:/Users/solis/Downloads/classic strategies"

SYMBOLS       <- c("QQQ", "IWM", "GLD")
ORB_CANDLES   <- 6; MIN_RANGE_PCT <- 0.001; VOL_FILTER <- TRUE
COMMISSION    <- 0.02; SHARES <- 10
rr_values     <- c(1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 5.0)

fetch_alpaca_bars <- function(symbol, from, to, timeframe = "5Min") {
  base_url <- sprintf("%s/%s/bars", ALPACA_BASE, symbol)
  all_bars <- list(); page_token <- NULL; page <- 1
  repeat {
    url <- sprintf("%s?timeframe=%s&start=%s&end=%s&limit=10000&adjustment=all&sort=asc&feed=iex",
                   base_url, timeframe, paste0(from, "T00:00:00Z"), paste0(to, "T23:59:59Z"))
    if (!is.null(page_token)) url <- paste0(url, "&page_token=", page_token)
    resp <- httr::GET(url, httr::add_headers("APCA-API-KEY-ID" = ALPACA_KEY, "APCA-API-SECRET-KEY" = ALPACA_SECRET), httr::timeout(30))
    if (httr::status_code(resp) == 429) { Sys.sleep(15); next }
    if (httr::status_code(resp) != 200) stop(sprintf("Error %d", httr::status_code(resp)))
    data <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
    bars <- data$bars
    if (!is.null(bars) && length(bars) > 0) { all_bars <- c(all_bars, bars) }
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

all_symbol_results <- list()

for (sym in SYMBOLS) {
  cat(sprintf("\n========== %s ==========\n", sym))
  cat("Descargando...\n")
  raw <- fetch_alpaca_bars(sym, "2017-01-01", format(Sys.Date() - 1, "%Y-%m-%d"))
  raw <- raw %>% mutate(datetime = lubridate::with_tz(datetime, "America/New_York")) %>%
    filter(format(datetime, "%H:%M") >= "09:30", format(datetime, "%H:%M") <= "15:55") %>%
    mutate(date = as.Date(datetime, tz = "America/New_York"))
  trading_days <- sort(unique(raw$date))
  cat(sprintf("%d dias.\n", length(trading_days)))
  
  cat("Encontrando entradas...\n")
  entries <- list()
  for (day in as.character(trading_days)) {
    day_data <- raw %>% filter(date == as.Date(day)) %>% arrange(datetime)
    if (nrow(day_data) < ORB_CANDLES + 1) next
    orb <- day_data[1:ORB_CANDLES, ]; orb_high <- max(orb$high); orb_low <- min(orb$low)
    orb_range <- orb_high - orb_low; orb_mid <- (orb_high + orb_low) / 2
    if (orb_range / orb_mid < MIN_RANGE_PCT) next
    avg_orb_vol <- mean(orb$volume); post_orb <- day_data[(ORB_CANDLES + 1):nrow(day_data), ]
    trade_taken <- FALSE
    for (i in seq_len(nrow(post_orb))) {
      if (trade_taken) break
      candle <- post_orb[i, ]; vol_ok <- !VOL_FILTER || (candle$volume > avg_orb_vol)
      if (candle$high > orb_high && vol_ok) {
        entries[[length(entries) + 1]] <- list(date = day, direction = "LONG",
          entry = orb_high + 0.01, orb_low = orb_low, orb_high = orb_high,
          post_start = i + 1, day_data = day_data)
        trade_taken <- TRUE
      } else if (!trade_taken && candle$low < orb_low && vol_ok) {
        entries[[length(entries) + 1]] <- list(date = day, direction = "SHORT",
          entry = orb_low - 0.01, orb_low = orb_low, orb_high = orb_high,
          post_start = i + 1, day_data = day_data)
        trade_taken <- TRUE
      }
    }
  }
  cat(sprintf("%d entradas.\n", length(entries)))
  
  cat("Evaluando RR...\n")
  rr_results <- list()
  for (rr in rr_values) {
    pnl_vec <- numeric(length(entries))
    result_vec <- character(length(entries))
    for (k in seq_along(entries)) {
      e <- entries[[k]]
      stop_p <- if (e$direction == "LONG") e$orb_low else e$orb_high
      risk <- abs(e$entry - stop_p)
      target <- if (e$direction == "LONG") e$entry + risk * rr else e$entry - risk * rr
      
      day_data <- e$day_data
      post_orb <- day_data[(ORB_CANDLES + 1):nrow(day_data), ]
      future <- if (e$post_start <= nrow(post_orb)) post_orb[e$post_start:nrow(post_orb), ] else post_orb[0, ]
      
      res <- "open"; exit_price <- NA
      if (nrow(future) > 0) {
        for (j in seq_len(nrow(future))) {
          if (e$direction == "LONG") {
            if (!is.na(future$low[j]) && future$low[j] <= stop_p) { res <- "stop"; exit_price <- stop_p; break }
            if (!is.na(future$high[j]) && future$high[j] >= target) { res <- "target"; exit_price <- target; break }
          } else {
            if (!is.na(future$high[j]) && future$high[j] >= stop_p) { res <- "stop"; exit_price <- stop_p; break }
            if (!is.na(future$low[j]) && future$low[j] <= target) { res <- "target"; exit_price <- target; break }
          }
        }
      }
      if (res == "open") { exit_price <- tail(day_data$close, 1); res <- "eod_close" }
      
      pnl_vec[k] <- if (e$direction == "LONG") (exit_price - e$entry) * SHARES - COMMISSION
                    else (e$entry - exit_price) * SHARES - COMMISSION
      result_vec[k] <- res
    }
    
    wins <- sum(pnl_vec > 0); n_t <- length(pnl_vec)
    avg_w <- if (wins > 0) mean(pnl_vec[pnl_vec > 0]) else 0
    avg_l <- if (n_t - wins > 0) mean(pnl_vec[pnl_vec <= 0]) else 0
    total_pnl <- sum(pnl_vec); pf <- if (sum(pnl_vec[pnl_vec <= 0]) != 0) abs(sum(pnl_vec[pnl_vec > 0]) / sum(pnl_vec[pnl_vec <= 0])) else Inf
    
    daily <- data.frame(date = as.Date(sapply(entries, `[[`, "date")), pnl = pnl_vec) %>%
      group_by(date) %>% summarise(daily = sum(pnl), .groups = "drop")
    all_days <- data.frame(date = trading_days) %>% left_join(daily, by = "date") %>% mutate(daily = ifelse(is.na(daily), 0, daily))
    sh <- if (sd(all_days$daily) > 0) mean(all_days$daily) / sd(all_days$daily) * sqrt(252) else NA
    cum_pnl <- cumsum(pnl_vec); max_dd <- min(cum_pnl - cummax(cum_pnl))
    
    rr_results[[as.character(rr)]] <- data.frame(
      symbol = sym, rr = rr, trades = n_t, win_rate = wins/n_t*100,
      pf = pf, pnl = total_pnl, max_dd = max_dd, sharpe = sh,
      target_pct = sum(result_vec == "target")/n_t*100,
      stop_pct = sum(result_vec == "stop")/n_t*100,
      eod_pct = sum(result_vec == "eod_close")/n_t*100,
      stringsAsFactors = FALSE
    )
  }
  all_symbol_results[[sym]] <- bind_rows(rr_results)
}

all_data <- bind_rows(all_symbol_results)

# ── SPY results (from earlier run) ──
spy_rr <- data.frame(
  symbol = "SPY",
  rr = c(1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 5.0),
  trades = 1412,
  win_rate = c(62.8, 56.9, 54.8, 54.1, 53.7, 53.5, 53.3, 53.2),
  pf = c(1.8, 1.8, 1.9, 1.9, 1.9, 2.0, 2.0, 2.0),
  pnl = c(6643, 7444, 8031, 8553, 8811, 9011, 9003, 9161),
  max_dd = c(-183, -183, -208, -208, -208, -208, -208, -208),
  sharpe = c(3.94, 3.70, 3.79, 3.81, 3.79, 3.72, 3.70, 3.59),
  target_pct = c(49.2, 33.8, 22.9, 15.0, 10.6, 6.8, 4.8, 2.3),
  stop_pct = c(28.8, 32.7, 34.2, 34.6, 34.9, 35.1, 35.3, 35.3),
  eod_pct = c(22.0, 33.5, 42.9, 50.4, 54.5, 58.1, 59.9, 62.3)
)

all_data <- bind_rows(all_data, spy_rr)

# ── Mostrar todo ──
cat("\n========================================\n")
cat("   OPTIMIZACION RR — SPY + QQQ + IWM + GLD\n")
cat("========================================\n")

for (sym in c("SPY", "QQQ", "IWM", "GLD")) {
  cat(sprintf("\n--- %s ---\n", sym))
  s <- all_data %>% filter(symbol == sym) %>% arrange(rr)
  s %>% select(rr, pnl, max_dd, sharpe, win_rate, target_pct, stop_pct) %>%
    mutate(pnl = round(pnl, 0), sharpe = round(sharpe, 2), win_rate = round(win_rate, 1),
           target_pct = round(target_pct, 1), stop_pct = round(stop_pct, 1)) %>%
    as.data.frame() %>% print(row.names = FALSE)
}

# ── Best por activo ──
cat("\n--- Mejor P&L por activo ---\n")
best <- all_data %>% group_by(symbol) %>% slice_max(pnl, n = 1) %>% arrange(desc(pnl))
best %>% select(symbol, rr, pnl, max_dd, sharpe, win_rate) %>%
  mutate(pnl = round(pnl, 0), sharpe = round(sharpe, 2), win_rate = round(win_rate, 1)) %>%
  as.data.frame() %>% print(row.names = FALSE)

# ── Grafico comparativo ──
out <- function(fname) file.path(OUTPUT_DIR, fname)

colors <- c("SPY" = "#2196F3", "QQQ" = "#FF9800", "IWM" = "#7C4DFF", "GLD" = "#FFD700")

p1 <- ggplot(all_data, aes(x = rr, y = pnl, color = symbol)) +
  geom_line(linewidth = 1.2) + geom_point(size = 3) +
  scale_color_manual(values = colors) +
  labs(title = "P&L Total vs RR Ratio — 4 activos",
       subtitle = "Entradas identicas, solo varia el target",
       x = "RR Ratio", y = "P&L Total (USD)", color = "") +
  scale_y_continuous(labels = dollar_format()) +
  theme_minimal(base_size = 14) + theme(legend.position = "bottom")
ggsave(out("opt_rr_4assets_pnl.png"), p1, width = 10, height = 5.5, dpi = 150)

p2 <- ggplot(all_data, aes(x = rr, y = sharpe, color = symbol)) +
  geom_line(linewidth = 1.2) + geom_point(size = 3) +
  scale_color_manual(values = colors) +
  labs(title = "Sharpe Ratio vs RR Ratio — 4 activos",
       x = "RR Ratio", y = "Sharpe (anual)", color = "") +
  theme_minimal(base_size = 14) + theme(legend.position = "bottom")
ggsave(out("opt_rr_4assets_sharpe.png"), p2, width = 10, height = 5.5, dpi = 150)

p3 <- ggplot(all_data, aes(x = rr, y = max_dd, color = symbol)) +
  geom_line(linewidth = 1.2) + geom_point(size = 3) +
  scale_color_manual(values = colors) +
  labs(title = "Max Drawdown vs RR Ratio — 4 activos",
       x = "RR Ratio", y = "Max Drawdown (USD)", color = "") +
  scale_y_continuous(labels = dollar_format()) +
  theme_minimal(base_size = 14) + theme(legend.position = "bottom")
ggsave(out("opt_rr_4assets_dd.png"), p3, width = 10, height = 5.5, dpi = 150)

cat("\nGraficos: opt_rr_4assets_pnl.png, opt_rr_4assets_sharpe.png, opt_rr_4assets_dd.png\n")
cat("Done.\n")
