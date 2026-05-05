# =============================================================================
# OPTIMIZACION ORB: Variacion de RR Ratio (manteniendo todo lo demas fijo)
# =============================================================================
pkgs <- c("dplyr", "lubridate", "ggplot2", "scales", "tibble", "jsonlite", "httr")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
invisible(lapply(pkgs, library, character.only = TRUE))

ALPACA_KEY    <- "YOUR_ALPACA_KEY"
ALPACA_SECRET <- "YOUR_ALPACA_SECRET"
ALPACA_BASE   <- "https://data.alpaca.markets/v2/stocks"
OUTPUT_DIR    <- "C:/Users/solis/Downloads/classic strategies"

# ── FETCH (una sola vez) ──
cat("Descargando SPY...\n")
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

raw <- fetch_alpaca_bars("SPY", "2017-01-01", format(Sys.Date() - 1, "%Y-%m-%d"))
raw <- raw %>% mutate(datetime = lubridate::with_tz(datetime, "America/New_York")) %>%
  filter(format(datetime, "%H:%M") >= "09:30", format(datetime, "%H:%M") <= "15:55") %>%
  mutate(date = as.Date(datetime, tz = "America/New_York"))
trading_days <- sort(unique(raw$date))
cat(sprintf("%d dias cargados.\n\n", length(trading_days)))

# ── Backtest con entradas precomputadas para ser rapido ──
# Paso 1: encontrar todas las entradas (no dependen del RR)
# Paso 2: para cada RR, evaluar donde sale (stop/target/EOD)

ORB_CANDLES   <- 6
MIN_RANGE_PCT <- 0.001
VOL_FILTER    <- TRUE
COMMISSION    <- 0.02
SHARES        <- 10

cat("Paso 1: Encontrando entradas (candles=6, vol=T, min_range=0.001)...\n")
entries <- list()  # cada elemento: list(date, direction, entry, orb_low, orb_high)
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
cat(sprintf("  %d entradas encontradas.\n\n", length(entries)))

# Paso 2: evaluar cada RR
rr_values <- c(1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 5.0)
cat("Paso 2: Evaluando RR ratios...\n")

results_by_rr <- list()
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
  losses <- sum(pnl_vec <= 0)
  wr <- wins / n_t * 100
  avg_w <- if (wins > 0) mean(pnl_vec[pnl_vec > 0]) else 0
  avg_l <- if (losses > 0) mean(pnl_vec[pnl_vec <= 0]) else 0
  pf <- if (sum(pnl_vec[pnl_vec <= 0]) != 0) abs(sum(pnl_vec[pnl_vec > 0]) / sum(pnl_vec[pnl_vec <= 0])) else Inf
  total_pnl <- sum(pnl_vec)
  
  # Daily P&L para Sharpe
  daily <- data.frame(date = as.Date(sapply(entries, `[[`, "date")), pnl = pnl_vec) %>%
    group_by(date) %>% summarise(daily = sum(pnl), .groups = "drop")
  all_days <- data.frame(date = trading_days) %>% left_join(daily, by = "date") %>% mutate(daily = ifelse(is.na(daily), 0, daily))
  sh <- if (sd(all_days$daily) > 0) mean(all_days$daily) / sd(all_days$daily) * sqrt(252) else NA
  
  cum_pnl <- cumsum(pnl_vec)
  max_dd <- min(cum_pnl - cummax(cum_pnl))
  
  tgt_pct <- sum(result_vec == "target") / n_t * 100
  stp_pct <- sum(result_vec == "stop") / n_t * 100
  eod_pct <- sum(result_vec == "eod_close") / n_t * 100
  
  results_by_rr[[as.character(rr)]] <- data.frame(
    rr = rr, trades = n_t, win_rate = wr, avg_win = avg_w, avg_loss = avg_l,
    pf = pf, pnl = total_pnl, max_dd = max_dd, sharpe = sh,
    target_pct = tgt_pct, stop_pct = stp_pct, eod_pct = eod_pct
  )
}

all_rr <- bind_rows(results_by_rr)

cat("\n========================================\n")
cat("   OPTIMIZACION: RR Ratio (candles=6, vol=T)\n")
cat("========================================\n\n")

# ── Tabla ──
display <- all_rr %>% select(rr, trades, win_rate, pf, pnl, max_dd, sharpe, target_pct, stop_pct, eod_pct)
display %>% mutate(across(c(win_rate, pf, sharpe, target_pct, stop_pct, eod_pct), ~round(., 1)),
                   across(c(pnl, max_dd), ~round(., 0)),
                   sharpe = round(sharpe, 2)) %>%
  as.data.frame() %>% print(row.names = FALSE)

# ── Best ──
best_pnl <- all_rr %>% slice_max(pnl, n = 1)
best_sharpe <- all_rr %>% slice_max(sharpe, n = 1)
cat(sprintf("\nMejor P&L:  RR=%.1f -> $%.0f (Sharpe=%.2f, WR=%.1f%%)\n", best_pnl$rr, best_pnl$pnl, best_pnl$sharpe, best_pnl$win_rate))
cat(sprintf("Mejor Sharpe: RR=%.1f -> %.2f (P&L=$%.0f, WR=%.1f%%)\n", best_sharpe$rr, best_sharpe$sharpe, best_sharpe$pnl, best_sharpe$win_rate))

# ── Graficos ──
out <- function(fname) file.path(OUTPUT_DIR, fname)

p1 <- ggplot(all_rr, aes(x = factor(rr))) +
  geom_col(aes(y = pnl, fill = pnl == max(pnl)), width = 0.6) +
  scale_fill_manual(values = c("TRUE" = "#FF9800", "FALSE" = "#2196F3"), guide = "none") +
  geom_text(aes(y = pnl, label = sprintf("$%.0f", pnl)), vjust = -0.5, size = 4) +
  labs(title = "P&L Total por RR Ratio", subtitle = sprintf("Naranja = mejor ($%.0f con RR=%.1f)", best_pnl$pnl, best_pnl$rr),
       x = "RR Ratio", y = "P&L Total (USD)") +
  scale_y_continuous(labels = dollar_format()) + theme_minimal(base_size = 14)
ggsave(out("opt_rr_pnl.png"), p1, width = 9, height = 5, dpi = 150)

p2 <- all_rr %>%
  select(rr, win_rate, sharpe) %>%
  pivot_longer(-rr, names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = factor(rr), y = value, color = metric, group = metric)) +
  geom_line(linewidth = 1.5) + geom_point(size = 4) +
  scale_color_manual(values = c("win_rate" = "#4CAF50", "sharpe" = "#7B1FA2"),
                     labels = c("win_rate" = "Win Rate %", "sharpe" = "Sharpe")) +
  labs(title = "Win Rate y Sharpe vs RR Ratio", x = "RR Ratio", y = "", color = "") +
  theme_minimal(base_size = 14) + theme(legend.position = "bottom")
ggsave(out("opt_rr_sharpe_wr.png"), p2, width = 9, height = 5, dpi = 150)

p3 <- all_rr %>%
  select(rr, target_pct, stop_pct, eod_pct) %>%
  pivot_longer(-rr, names_to = "resultado", values_to = "pct") %>%
  ggplot(aes(x = factor(rr), y = pct, fill = resultado)) +
  geom_col(position = "fill", width = 0.6) +
  scale_fill_manual(values = c("target_pct" = "#4CAF50", "stop_pct" = "#F44336", "eod_pct" = "#FF9800"),
                    labels = c("target_pct" = "Target", "stop_pct" = "Stop", "eod_pct" = "EOD")) +
  labs(title = "Distribucion de Resultados por RR Ratio", subtitle = "A mayor RR, menos targets y mas EOD/stops",
       x = "RR Ratio", y = "% de trades", fill = "") +
  theme_minimal(base_size = 14)
ggsave(out("opt_rr_distribution.png"), p3, width = 9, height = 5, dpi = 150)

cat("\nGraficos: opt_rr_pnl.png, opt_rr_sharpe_wr.png, opt_rr_distribution.png\n")
cat("Done.\n")
