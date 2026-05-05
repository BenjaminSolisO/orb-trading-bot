# =============================================================================
# PORTFOLIO: ORB 4-asset — QQQ + SPY + IWM + GLD (5min)
# Datos: Alpaca Markets API (IEX feed)
# =============================================================================
# Descarga una vez, ejecuta ORB en los 4, combina, calcula correlacion
# =============================================================================

pkgs <- c("dplyr", "lubridate", "ggplot2", "scales", "tibble", "jsonlite", "httr", "tidyr")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
invisible(lapply(pkgs, library, character.only = TRUE))

ALPACA_KEY    <- "YOUR_ALPACA_KEY"
ALPACA_SECRET <- "YOUR_ALPACA_SECRET"
ALPACA_BASE   <- "https://data.alpaca.markets/v2/stocks"

SYMBOLS       <- c("SPY", "QQQ", "IWM", "GLD")
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

cat("=== ORB Portfolio: QQQ + SPY + IWM + GLD ===\n")
cat(sprintf("Periodo: %s a %s | 10 acciones por activo\n\n", DATE_FROM, DATE_TO))

# --- 1. FETCH DATA Para todos los simbolos ------------------------------------
fetch_alpaca_bars <- function(symbol, from, to, timeframe = "5Min") {
  base_url <- sprintf("%s/%s/bars", ALPACA_BASE, symbol)
  all_bars <- list(); page_token <- NULL; page <- 1
  repeat {
    url <- sprintf("%s?timeframe=%s&start=%s&end=%s&limit=10000&adjustment=all&sort=asc&feed=iex",
                   base_url, timeframe, paste0(from, "T00:00:00Z"), paste0(to, "T23:59:59Z"))
    if (!is.null(page_token)) url <- paste0(url, "&page_token=", page_token)
    cat(sprintf("    Pag %d...", page))
    resp <- httr::GET(url, httr::add_headers("APCA-API-KEY-ID" = ALPACA_KEY, "APCA-API-SECRET-KEY" = ALPACA_SECRET), httr::timeout(30))
    if (httr::status_code(resp) == 429) { cat(" RL\n"); Sys.sleep(15); next }
    if (httr::status_code(resp) != 200) stop(sprintf("Error HTTP %d", httr::status_code(resp)))
    data <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
    bars <- data$bars
    if (!is.null(bars) && length(bars) > 0) { all_bars <- c(all_bars, bars); cat(sprintf(" %d\n", length(bars))) }
    else { cat(" 0\n") }
    next_token <- data$next_page_token
    if (!is.null(next_token) && nchar(next_token) > 0) { page_token <- next_token; page <- page + 1 } else { break }
  }
  if (length(all_bars) == 0) stop(sprintf("%s sin datos.", symbol))
  n <- length(all_bars)
  t_chr <- character(n); o_vec <- numeric(n); h_vec <- numeric(n); l_vec <- numeric(n); c_vec <- numeric(n); v_vec <- numeric(n)
  for (k in seq_len(n)) { r <- all_bars[[k]]; t_chr[k] <- r$t; o_vec[k] <- as.numeric(r$o); h_vec[k] <- as.numeric(r$h); l_vec[k] <- as.numeric(r$l); c_vec[k] <- as.numeric(r$c); v_vec[k] <- as.numeric(r$v) }
  df <- data.frame(datetime = lubridate::ymd_hms(t_chr, tz = "UTC"), open = o_vec, high = h_vec, low = l_vec, close = c_vec, volume = v_vec, stringsAsFactors = FALSE)
  df[order(df$datetime), ]
}

# Data por simbolo
raw_data <- list()
for (sym in SYMBOLS) {
  cat(sprintf("\nDescargando %s...\n", sym))
  raw <- fetch_alpaca_bars(sym, DATE_FROM, DATE_TO)
  raw <- raw %>% mutate(datetime = lubridate::with_tz(datetime, "America/New_York")) %>%
    filter(format(datetime, "%H:%M") >= "09:30", format(datetime, "%H:%M") <= "15:55") %>%
    mutate(date = as.Date(datetime, tz = "America/New_York"))
  cat(sprintf("  %s: %d velas | %s – %s | %d dias\n", sym, nrow(raw),
              format(min(raw$datetime), "%Y-%m-%d"), format(max(raw$datetime), "%Y-%m-%d"),
              length(unique(raw$date))))
  raw_data[[sym]] <- raw
}

# --- 2. RUN ORB en cada simbolo ------------------------------------------------
run_orb <- function(raw, symbol_name) {
  trading_days <- sort(unique(raw$date))
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
        trades[[length(trades) + 1]] <- tibble(symbol = symbol_name, date = as.Date(day), direction = "LONG",
          entry = entry, exit = exit_price, result = result, pnl_usd = (exit_price - entry) * SHARES - COMMISSION, orb_range = orb_range)
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
        trades[[length(trades) + 1]] <- tibble(symbol = symbol_name, date = as.Date(day), direction = "SHORT",
          entry = entry, exit = exit_price, result = result, pnl_usd = (entry - exit_price) * SHARES - COMMISSION, orb_range = orb_range)
        trade_taken <- TRUE
      }
    }
  }
  list(trades = bind_rows(trades), trading_days = trading_days)
}

all_trades <- list()
all_days_list <- list()
individual_stats <- list()

for (sym in SYMBOLS) {
  cat(sprintf("Backtest ORB %s...\n", sym))
  orb_result <- run_orb(raw_data[[sym]], sym)
  trades_df <- orb_result$trades
  
  # Stats individuales
  w <- sum(trades_df$pnl_usd > 0); l <- sum(trades_df$pnl_usd <= 0)
  wr <- w / nrow(trades_df)
  pf <- if (sum(trades_df$pnl_usd[trades_df$pnl_usd <= 0]) != 0) 
    abs(sum(trades_df$pnl_usd[trades_df$pnl_usd > 0]) / sum(trades_df$pnl_usd[trades_df$pnl_usd <= 0])) else Inf
  total <- sum(trades_df$pnl_usd)
  
  individual_stats[[sym]] <- c(trades = nrow(trades_df), wr = wr * 100, pf = pf, pnl = total)
  
  # Daily P&L
  daily <- tibble(date = orb_result$trading_days, symbol = sym) %>%
    left_join(trades_df %>% group_by(date) %>% summarise(pnl = sum(pnl_usd)), by = "date") %>%
    mutate(pnl = if_else(is.na(pnl), 0, pnl)) %>%
    select(date, symbol, pnl)
  
  all_days_list[[sym]] <- daily
  all_trades[[sym]] <- trades_df
  cat(sprintf("  %s: %d trades | WR %.1f%% | PF %.2f | P&L $%.0f\n", sym, nrow(trades_df), wr*100, pf, total))
}

# --- 3. PORTFOLIO COMBINADO ---------------------------------------------------
all_daily <- bind_rows(all_days_list)

portfolio_daily <- all_daily %>%
  group_by(date) %>%
  summarise(daily_pnl = sum(pnl), n_active = sum(pnl != 0), .groups = "drop") %>%
  arrange(date) %>%
  mutate(cum_pnl = cumsum(daily_pnl))

all_trades_combined <- bind_rows(all_trades)

# Wide format para correlacion
daily_wide <- all_daily %>%
  pivot_wider(names_from = symbol, values_from = pnl, values_fill = 0)

# Correlacion entre estrategias
cor_matrix <- cor(daily_wide[, SYMBOLS])
cat("\n--- Correlacion diaria entre estrategias ---\n")
print(round(cor_matrix, 3))

# --- 4. METRICAS --------------------------------------------------------------
n_days <- nrow(portfolio_daily)
n_active_days <- sum(portfolio_daily$n_active > 0)
total_pnl <- sum(portfolio_daily$daily_pnl)
total_trades <- nrow(all_trades_combined)
max_dd <- min(portfolio_daily$cum_pnl - cummax(portfolio_daily$cum_pnl))
daily_return <- mean(portfolio_daily$daily_pnl)
daily_sd <- sd(portfolio_daily$daily_pnl)
sharpe <- daily_return / daily_sd * sqrt(252)

wins <- portfolio_daily %>% filter(daily_pnl > 0)
losses <- portfolio_daily %>% filter(daily_pnl <= 0)
daily_wr <- nrow(wins) / n_days * 100
pf_daily <- if (sum(losses$daily_pnl) != 0) abs(sum(wins$daily_pnl) / sum(losses$daily_pnl)) else Inf

cat(sprintf("\n========================================\n"))
cat(sprintf("    PORTFOLIO ORB 4-ASSET: QQQ + SPY + IWM + GLD\n"))
cat(sprintf("========================================\n"))
cat(sprintf("Periodo:        %s a %s\n", min(portfolio_daily$date), max(portfolio_daily$date)))
cat(sprintf("Dias totales:   %d | Dias con trades: %d (%.0f%%)\n", n_days, n_active_days, n_active_days/n_days*100))
cat(sprintf("Trades totales: %d (promedio %.1f/dia)\n", total_trades, total_trades/n_active_days))

cat(sprintf("\n--- Individual ---\n"))
cat(sprintf("  %-4s  %6s  %5s  %4s  %8s\n", "Sym", "Trades", "WR%", "PF", "P&L"))
for (sym in SYMBOLS) {
  s <- individual_stats[[sym]]
  cat(sprintf("  %-4s  %6.0f  %5.1f  %4.2f  %+8.0f\n", sym, s["trades"], s["wr"], s["pf"], s["pnl"]))
}

cat(sprintf("\n--- Portfolio Combinado ---\n"))
cat(sprintf("P&L Total:      $%.0f\n", total_pnl))
cat(sprintf("Dias positivos: %.0f%% (%d/%d)\n", daily_wr, nrow(wins), n_days))
cat(sprintf("Profit Factor:  %.2f\n", pf_daily))
cat(sprintf("Max Drawdown:   $%.0f\n", max_dd))
cat(sprintf("Sharpe anual:   %.2f\n", sharpe))
cat(sprintf("Retorno diario promedio: $%.2f (+- $%.2f)\n", daily_return, daily_sd))

# --- Yearly breakdown ---
yearly <- portfolio_daily %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(
    pnl      = sum(daily_pnl),
    days     = n(),
    active   = sum(n_active > 0),
    daily_avg = mean(daily_pnl),
    daily_sd  = sd(daily_pnl),
    sharpe   = daily_avg / daily_sd * sqrt(252),
    max_dd   = min(cumsum(daily_pnl) - cummax(cumsum(daily_pnl))),
    .groups  = "drop"
  )

cat("\n--- Por año ---\n")
print(as.data.frame(yearly %>% select(year, pnl, active, sharpe, max_dd) %>% mutate(pnl = round(pnl, 0), sharpe = round(sharpe, 2), max_dd = round(max_dd, 0))), row.names = FALSE)

# --- 5. GRAFICOS -------------------------------------------------------------
out <- function(fname) file.path(OUTPUT_DIR, fname)

# 5a. Equity curve
p1 <- ggplot(portfolio_daily, aes(x = date, y = cum_pnl)) +
  geom_area(fill = "#4A148C", alpha = 0.12) +
  geom_line(color = "#7B1FA2", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Portfolio ORB — QQQ + SPY + IWM + GLD",
       subtitle = sprintf("Sharpe: %.2f | P&L: $%.0f | TRADES: %d | MaxDD: $%.0f | %s – %s",
                          sharpe, total_pnl, total_trades, max_dd, min(portfolio_daily$date), max(portfolio_daily$date)),
       x = "Fecha", y = "P&L Acumulado (USD)") +
  scale_y_continuous(labels = dollar_format()) +
  theme_minimal(base_size = 13) + theme(plot.title = element_text(face = "bold"))
ggsave(out("portfolio_orb_4_equity.png"), p1, width = 12, height = 5, dpi = 150)

# 5b. Contribucion por activo (stacked area)
cum_by_symbol <- all_daily %>%
  group_by(symbol) %>%
  arrange(date) %>%
  mutate(cum = cumsum(pnl)) %>%
  ungroup()

p2 <- ggplot(cum_by_symbol, aes(x = date, y = cum, fill = symbol)) +
  geom_area(alpha = 0.85, position = "identity") +
  scale_fill_manual(values = c("SPY" = "#2196F3", "QQQ" = "#FF9800", "IWM" = "#7C4DFF", "GLD" = "#FFD700")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Contribucion por Activo — Equity Acumulada",
       x = "Fecha", y = "P&L Acumulado (USD)", fill = "") +
  scale_y_continuous(labels = dollar_format()) +
  theme_minimal(base_size = 13) + theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
ggsave(out("portfolio_orb_4_stacked.png"), p2, width = 12, height = 5, dpi = 150)

# 5c. P&L Mensual (barras) + Equity (linea)
monthly <- portfolio_daily %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(pnl = sum(daily_pnl), .groups = "drop") %>%
  mutate(cum_pnl = cumsum(pnl))

p3 <- ggplot(monthly, aes(x = month, y = pnl, fill = pnl >= 0)) +
  geom_col(width = 20) +
  geom_line(aes(x = month, y = cum_pnl / 5, group = 1), color = "#4A148C", linewidth = 0.8, inherit.aes = FALSE) +
  scale_fill_manual(values = c("TRUE" = "#4CAF50", "FALSE" = "#F44336"), guide = "none") +
  scale_y_continuous(labels = dollar_format(), name = "P&L Mensual (USD)",
                     sec.axis = sec_axis(~ . * 5, name = "Acumulado (USD)", labels = dollar_format())) +
  labs(title = "P&L Mensual — Portfolio ORB 4-asset", x = "") +
  theme_minimal(base_size = 13) + theme(plot.title = element_text(face = "bold"), axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(out("portfolio_orb_4_monthly.png"), p3, width = 12, height = 5, dpi = 150)

# 5d. Correlacion heatmap
cor_long <- as.data.frame(as.table(cor_matrix))
names(cor_long) <- c("Asset1", "Asset2", "Correlation")

p4 <- ggplot(cor_long, aes(x = Asset1, y = Asset2, fill = Correlation)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = sprintf("%.2f", Correlation)), size = 5, color = "white", fontface = "bold") +
  scale_fill_gradient2(low = "#E53935", mid = "white", high = "#43A047", midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Correlacion Diaria entre Estrategias ORB", x = "", y = "") +
  theme_minimal(base_size = 14) + theme(plot.title = element_text(face = "bold"), legend.position = "none")
ggsave(out("portfolio_orb_4_corr.png"), p4, width = 6, height = 5, dpi = 150)

# 5e. P&L Anual comparativo por activo
yearly_by_symbol <- all_daily %>%
  mutate(year = year(date)) %>%
  group_by(symbol, year) %>%
  summarise(pnl = sum(pnl), .groups = "drop")

p5 <- ggplot(yearly_by_symbol, aes(x = factor(year), y = pnl, fill = symbol)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("SPY" = "#2196F3", "QQQ" = "#FF9800", "IWM" = "#7C4DFF", "GLD" = "#FFD700")) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  scale_y_continuous(labels = dollar_format()) +
  labs(title = "P&L Anual por Activo — ORB", x = "", y = "P&L (USD)", fill = "") +
  theme_minimal(base_size = 13) + theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
ggsave(out("portfolio_orb_4_yearly_bars.png"), p5, width = 10, height = 5, dpi = 150)

# 5f. Daily P&L distribution
p6 <- ggplot(portfolio_daily, aes(x = daily_pnl)) +
  geom_histogram(bins = 50, fill = "#7B1FA2", color = "white", alpha = 0.85) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = daily_return, color = "#4CAF50", linewidth = 1.2) +
  labs(title = "Distribucion de P&L Diario — Portfolio ORB",
       subtitle = sprintf("Media: $%.2f | Mediana: $%.2f | Dias > 0: %.0f%%",
                          daily_return, median(portfolio_daily$daily_pnl), daily_wr),
       x = "P&L Diario (USD)", y = "Frecuencia") +
  scale_x_continuous(labels = dollar_format()) +
  theme_minimal(base_size = 13) + theme(plot.title = element_text(face = "bold"))
ggsave(out("portfolio_orb_4_daily_dist.png"), p6, width = 10, height = 5, dpi = 150)

# --- 6. EXPORTAR -------------------------------------------------------------
write.csv(all_trades_combined, out("portfolio_orb_4_trades.csv"), row.names = FALSE)
write.csv(portfolio_daily, out("portfolio_orb_4_daily.csv"), row.names = FALSE)

max_pnl_date <- portfolio_daily$date[which.max(portfolio_daily$cum_pnl)]
cat(sprintf("\nMax equity: $%.0f (%s)\n", max(portfolio_daily$cum_pnl), max_pnl_date))
cat(sprintf("Max daily gain: $%.0f | Max daily loss: $%.0f\n", max(portfolio_daily$daily_pnl), min(portfolio_daily$daily_pnl)))

cat("\nArchivos generados:\n")
cat("  portfolio_orb_4_equity.png\n  portfolio_orb_4_stacked.png\n  portfolio_orb_4_monthly.png\n")
cat("  portfolio_orb_4_corr.png\n  portfolio_orb_4_yearly_bars.png\n  portfolio_orb_4_daily_dist.png\n")
cat("  portfolio_orb_4_trades.csv\n  portfolio_orb_4_daily.csv\n")
cat("Done.\n")
