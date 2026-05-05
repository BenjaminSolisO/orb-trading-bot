# =============================================================================
# BACKTEST: Gap and Go — SPY 5min
# Estrategia 3 de 6 | Datos: Massive (ex-Polygon.io)
# =============================================================================
# LOGICA:
#   - Gap Up:   open > prev_close * (1 + GAP_PCT)
#   - Gap Down: open < prev_close * (1 - GAP_PCT)
#   - Confirmacion: primera vela 5min cierra en direccion del gap
#   - Entry:  close de vela de confirmacion
#   - Stop:   low (long) / high (short) de vela de confirmacion
#   - Target: 2:1 RR
#   - Max 1 trade por dia
#   - Sin trade si stop es menor a MIN_STOP_PCT del entry
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

GAP_PCT       <- 0.003    # gap minimo 0.3% para trigger
RR_TARGET     <- 2.0
MIN_STOP_PCT  <- 0.001    # stop minimo 0.1% — evita stops irracionales
COMMISSION    <- 0.02
SHARES        <- 10

OUTPUT_DIR <- tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) "C:/Users/solis/Downloads/classic strategies"
)

# --- 3. FETCH ----------------------------------------------------------------
fetch_massive <- function(symbol, from, to, api_key,
                          multiplier = 5, timespan = "minute") {
  base_url <- sprintf(
    "https://api.massive.com/v2/aggs/ticker/%s/range/%d/%s/%s/%s",
    symbol, multiplier, timespan, from, to
  )
  all_results <- list(); next_url <- NULL; page <- 1
  repeat {
    url <- if (is.null(next_url))
      paste0(base_url, "?adjusted=true&sort=asc&limit=50000&apiKey=", api_key)
    else paste0(next_url, "&apiKey=", api_key)
    cat(sprintf("  Pagina %d...\n", page))
    resp <- httr::GET(url, httr::timeout(30))
    while (httr::status_code(resp) == 429) {
      cat("  Rate limit, esperando 20s...\n"); Sys.sleep(20)
      resp <- httr::GET(url, httr::timeout(30))
    }
    if (httr::status_code(resp) != 200)
      stop(sprintf("HTTP %d: %s", httr::status_code(resp), httr::content(resp, "text")))
    data <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"),
                               simplifyVector = FALSE)
    if (!is.null(data$results)) all_results <- c(all_results, data$results)
    if (!is.null(data$next_url) && nchar(data$next_url) > 0) {
      next_url <- sub("&apiKey=[^&]+", "", data$next_url); page <- page+1; Sys.sleep(13)
    } else break
  }
  n <- length(all_results)
  cat(sprintf("  Construyendo df (%d velas)...\n", n))
  t_v<-numeric(n); o_v<-numeric(n); h_v<-numeric(n)
  l_v<-numeric(n); c_v<-numeric(n); v_v<-numeric(n)
  for (k in seq_len(n)) {
    r<-all_results[[k]]
    t_v[k]<-as.numeric(r$t)/1000; o_v[k]<-as.numeric(r$o)
    h_v[k]<-as.numeric(r$h);      l_v[k]<-as.numeric(r$l)
    c_v[k]<-as.numeric(r$c);      v_v[k]<-as.numeric(r$v)
  }
  df <- data.frame(
    datetime=as.POSIXct(t_v, origin="1970-01-01", tz="UTC"),
    open=o_v, high=h_v, low=l_v, close=c_v, volume=v_v
  )
  df$datetime <- lubridate::with_tz(df$datetime, "America/New_York")
  df[order(df$datetime), ]
}

# --- 4. DATOS ----------------------------------------------------------------
cat(sprintf("Descargando %s | %s a %s\n", SYMBOL, DATE_FROM, DATE_TO))
raw <- fetch_massive(SYMBOL, DATE_FROM, DATE_TO, POLYGON_KEY)

raw <- raw %>%
  filter(format(datetime,"%H:%M") >= "09:30",
         format(datetime,"%H:%M") <= "15:55") %>%
  mutate(date = as.Date(datetime, tz="America/New_York"))

trading_days <- sort(unique(raw$date))
cat(sprintf("Velas: %d | Dias: %d\n", nrow(raw), length(trading_days)))

# Prev close por dia
daily_close <- raw %>%
  group_by(date) %>%
  summarise(prev_close = last(close), .groups="drop") %>%
  mutate(next_date = lead(date), prev_close_fwd = prev_close) %>%
  select(date = next_date, prev_close = prev_close_fwd) %>%
  filter(!is.na(date))

# --- 5. BACKTEST -------------------------------------------------------------
trades <- list()

for (day in as.character(trading_days)) {
  d <- as.Date(day)
  pc_row <- daily_close %>% filter(date == d)
  if (nrow(pc_row) == 0) next
  prev_close <- pc_row$prev_close

  dd <- raw %>% filter(date == d) %>% arrange(datetime)
  if (nrow(dd) < 2) next

  # Primera vela = 09:30
  first <- dd[1, ]
  day_open <- first$open
  gap_pct  <- (day_open - prev_close) / prev_close

  # Clasificar gap
  is_gap_up   <- gap_pct >  GAP_PCT
  is_gap_down <- gap_pct < -GAP_PCT
  if (!is_gap_up && !is_gap_down) next

  # Confirmacion: primera vela cierra en direccion del gap
  confirmed_long  <- is_gap_up   && first$close > first$open
  confirmed_short <- is_gap_down && first$close < first$open
  if (!confirmed_long && !confirmed_short) next

  direction <- if (confirmed_long) "LONG" else "SHORT"
  entry     <- first$close
  stop_p    <- if (direction == "LONG") first$low else first$high
  risk      <- abs(entry - stop_p)

  # Filtro: stop demasiado pequeno
  if (risk / entry < MIN_STOP_PCT) next

  target <- if (direction == "LONG") entry + risk * RR_TARGET
            else                      entry - risk * RR_TARGET

  # Simular en velas post-primera
  post   <- dd[2:nrow(dd), ]
  result <- "open"; exit_price <- NA

  for (j in seq_len(nrow(post))) {
    hits_stop   <- if (direction=="LONG") post$low[j]  <= stop_p else post$high[j] >= stop_p
    hits_target <- if (direction=="LONG") post$high[j] >= target  else post$low[j]  <= target
    # Stop primero si misma vela toca ambos
    if (hits_stop)   { result <- "stop";   exit_price <- stop_p; break }
    if (hits_target) { result <- "target"; exit_price <- target; break }
  }

  if (result == "open") {
    exit_price <- tail(dd$close, 1)
    result     <- "eod_close"
  }

  pnl_pts <- if (direction=="LONG") exit_price - entry else entry - exit_price
  pnl_usd <- pnl_pts * SHARES - COMMISSION

  trades[[length(trades)+1]] <- tibble(
    date      = d,
    direction = direction,
    gap_pct   = round(gap_pct * 100, 3),
    prev_close = prev_close,
    day_open  = day_open,
    entry     = entry,
    stop      = stop_p,
    target    = target,
    exit      = exit_price,
    result    = result,
    risk_pts  = risk,
    pnl_pts   = pnl_pts,
    pnl_usd   = pnl_usd
  )
}

results <- bind_rows(trades)
cat(sprintf("\nTotal trades: %d\n", nrow(results)))
if (nrow(results) == 0) stop("Sin trades. Reducir GAP_PCT.")

results <- results %>% arrange(date) %>% mutate(cum_pnl = cumsum(pnl_usd))

# --- 6. METRICAS -------------------------------------------------------------
wins    <- results %>% filter(pnl_usd > 0)
losses  <- results %>% filter(pnl_usd <= 0)
n_total <- nrow(results)
win_rate      <- nrow(wins) / n_total
avg_win       <- if (nrow(wins)   > 0) mean(wins$pnl_usd)   else 0
avg_loss      <- if (nrow(losses) > 0) mean(losses$pnl_usd) else 0
profit_factor <- if (sum(losses$pnl_usd) != 0) abs(sum(wins$pnl_usd)/sum(losses$pnl_usd)) else Inf
total_pnl     <- sum(results$pnl_usd)
max_dd        <- min(results$cum_pnl - cummax(results$cum_pnl))

all_days <- tibble(date = trading_days) %>%
  left_join(results %>% group_by(date) %>% summarise(daily=sum(pnl_usd)), by="date") %>%
  mutate(daily = if_else(is.na(daily), 0, daily))
sharpe <- mean(all_days$daily) / sd(all_days$daily) * sqrt(252)

result_dist <- results %>% count(result) %>% mutate(pct = n/sum(n)*100)
gap_stats   <- results %>%
  mutate(gap_dir = if_else(direction=="LONG","Gap Up","Gap Down")) %>%
  group_by(gap_dir) %>%
  summarise(n=n(), total=sum(pnl_usd), wr=mean(pnl_usd>0)*100,
            avg_gap=mean(abs(gap_pct)), .groups="drop")

cat("\n========================================\n")
cat("     RESULTADOS GAP AND GO — SPY\n")
cat("========================================\n")
cat(sprintf("Periodo:         %s a %s\n", min(results$date), max(results$date)))
cat(sprintf("Dias evaluados:  %d | Con gap: %d (%.0f%%)\n",
            length(trading_days), n_total, n_total/length(trading_days)*100))
cat(sprintf("Total trades:    %d\n", n_total))
cat(sprintf("Win Rate:        %.1f%%\n", win_rate*100))
cat(sprintf("Avg Win:         $%.2f\n", avg_win))
cat(sprintf("Avg Loss:        $%.2f\n", avg_loss))
cat(sprintf("Profit Factor:   %.2f\n", profit_factor))
cat(sprintf("Total P&L:       $%.2f\n", total_pnl))
cat(sprintf("Max Drawdown:    $%.2f\n", max_dd))
cat(sprintf("Sharpe (anual):  %.2f\n", sharpe))
cat("\nPor direccion:\n"); print(gap_stats)
cat("\nDistribucion resultados:\n"); print(result_dist)
cat("========================================\n")

# --- 7. GRAFICOS -------------------------------------------------------------
out <- function(f) file.path(OUTPUT_DIR, f)

p1 <- ggplot(results, aes(x=date, y=cum_pnl)) +
  geom_area(fill="#FF5722", alpha=0.12) +
  geom_line(color="#FF5722", linewidth=1) +
  geom_hline(yintercept=0, linetype="dashed", color="gray50") +
  scale_y_continuous(labels=dollar_format()) +
  labs(
    title    = "Gap and Go — SPY | Curva de Equity",
    subtitle = sprintf("Win Rate: %.1f%% | PF: %.2f | Sharpe: %.2f | Trades: %d",
                       win_rate*100, profit_factor, sharpe, n_total),
    x="Fecha", y="P&L Acumulado (USD)"
  ) +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(face="bold"))

ggsave(out("gap_equity_curve.png"), p1, width=10, height=5, dpi=150)

p2 <- ggplot(results, aes(x=pnl_usd, fill=pnl_usd>0)) +
  geom_histogram(bins=25, color="white", alpha=0.85) +
  scale_fill_manual(values=c("TRUE"="#4CAF50","FALSE"="#F44336"),
                    labels=c("TRUE"="Win","FALSE"="Loss"), name="") +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_x_continuous(labels=dollar_format()) +
  labs(title="Distribución P&L — Gap and Go", x="P&L (USD)", y="Frecuencia") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(face="bold"))

ggsave(out("gap_pnl_dist.png"), p2, width=8, height=4, dpi=150)

p3 <- ggplot(results, aes(x=abs(gap_pct), y=pnl_usd, color=pnl_usd>0)) +
  geom_point(alpha=0.6, size=2) +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_color_manual(values=c("TRUE"="#4CAF50","FALSE"="#F44336"),
                     labels=c("TRUE"="Win","FALSE"="Loss"), name="") +
  scale_y_continuous(labels=dollar_format()) +
  labs(title="Tamaño del Gap vs P&L", x="Gap % (absoluto)", y="P&L (USD)") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(face="bold"))

ggsave(out("gap_size_vs_pnl.png"), p3, width=8, height=4, dpi=150)

write.csv(results, out("gap_trades.csv"), row.names=FALSE)
cat("\nArchivos: gap_equity_curve.png | gap_pnl_dist.png | gap_size_vs_pnl.png | gap_trades.csv\n")
cat("Done.\n")
