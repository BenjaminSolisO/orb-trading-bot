# =============================================================================
# PORTFOLIO: ORB + VWAP Reversion (ADX filter) — SPY 5min
# Combina ambas estrategias con capital igual por lado
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

# ORB
ORB_CANDLES   <- 6
RR_TARGET     <- 2.0
MIN_RANGE_PCT <- 0.001
VOL_FILTER    <- TRUE
SHARES        <- 10
COMMISSION    <- 0.02

# VWAP V3
VWAP_BAND_PCT <- 0.002
RSI_PERIOD    <- 14
RSI_LONG      <- 45
RSI_SHORT     <- 55
STOP_MULT     <- 2.0
ADX_MAX       <- 25

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
      next_url <- sub("&apiKey=[^&]+", "", data$next_url); page <- page + 1; Sys.sleep(13)
    } else break
  }
  n <- length(all_results)
  cat(sprintf("  Construyendo df (%d velas)...\n", n))
  t_v <- numeric(n); o_v <- numeric(n); h_v <- numeric(n)
  l_v <- numeric(n); c_v <- numeric(n); v_v <- numeric(n)
  for (k in seq_len(n)) {
    r <- all_results[[k]]
    t_v[k] <- as.numeric(r$t)/1000; o_v[k] <- as.numeric(r$o)
    h_v[k] <- as.numeric(r$h);      l_v[k] <- as.numeric(r$l)
    c_v[k] <- as.numeric(r$c);      v_v[k] <- as.numeric(r$v)
  }
  df <- data.frame(
    datetime = as.POSIXct(t_v, origin = "1970-01-01", tz = "UTC"),
    open = o_v, high = h_v, low = l_v, close = c_v, volume = v_v
  )
  df$datetime <- lubridate::with_tz(df$datetime, "America/New_York")
  df[order(df$datetime), ]
}

# --- 4. HELPERS --------------------------------------------------------------
calc_vwap <- function(high, low, close, volume) {
  tp <- (high + low + close) / 3
  cumsum(tp * volume) / cumsum(volume)
}

calc_rsi <- function(closes, n = 14) {
  m <- length(closes); rsi_vec <- rep(NA_real_, m)
  if (m < n + 1) return(rsi_vec)
  chg <- diff(closes); g <- pmax(chg, 0); l <- pmax(-chg, 0)
  ag <- mean(g[1:n]); al <- mean(l[1:n])
  for (i in (n+1):length(chg)) {
    ag <- (ag*(n-1) + g[i])/n; al <- (al*(n-1) + l[i])/n
    rsi_vec[i+1] <- 100 - 100/(1 + if (al==0) Inf else ag/al)
  }
  rsi_vec
}

calc_adx <- function(high, low, close, n = 14) {
  m <- length(close); adx_vec <- rep(NA_real_, m)
  if (m < 2*n+2) return(adx_vec)
  tr <- numeric(m); pdm <- numeric(m); ndm <- numeric(m)
  for (i in 2:m) {
    tr[i]  <- max(high[i]-low[i], abs(high[i]-close[i-1]), abs(low[i]-close[i-1]))
    up <- high[i]-high[i-1]; dn <- low[i-1]-low[i]
    pdm[i] <- if (up>dn && up>0) up else 0
    ndm[i] <- if (dn>up && dn>0) dn else 0
  }
  atr_s <- sum(tr[2:(n+1)]); ap_s <- sum(pdm[2:(n+1)]); an_s <- sum(ndm[2:(n+1)])
  dx_v <- rep(NA_real_, m)
  for (i in (n+1):m) {
    if (i > n+1) { atr_s <- atr_s - atr_s/n + tr[i]; ap_s <- ap_s - ap_s/n + pdm[i]; an_s <- an_s - an_s/n + ndm[i] }
    pdi <- 100*ap_s/atr_s; ndi <- 100*an_s/atr_s; den <- pdi+ndi
    dx_v[i] <- if (den==0) 0 else 100*abs(pdi-ndi)/den
  }
  fv <- which(!is.na(dx_v))[1]
  if (is.na(fv) || fv+n > m) return(adx_vec)
  adx_s <- mean(dx_v[fv:(fv+n-1)], na.rm = TRUE)
  for (i in (fv+n):m) { adx_s <- (adx_s*(n-1)+dx_v[i])/n; adx_vec[i] <- adx_s }
  adx_vec
}

metrics <- function(results, trading_days, label) {
  wins <- results %>% filter(pnl_usd > 0)
  losses <- results %>% filter(pnl_usd <= 0)
  n_total <- nrow(results)
  wr <- nrow(wins)/n_total
  pf <- if (sum(losses$pnl_usd)!=0) abs(sum(wins$pnl_usd)/sum(losses$pnl_usd)) else Inf
  tot <- sum(results$pnl_usd)
  mdd <- min(results$cum_pnl - cummax(results$cum_pnl))
  adp <- tibble(date = trading_days) %>%
    left_join(results %>% group_by(date) %>% summarise(d=sum(pnl_usd)), by="date") %>%
    mutate(d = if_else(is.na(d), 0, d))
  sh <- mean(adp$d)/sd(adp$d)*sqrt(252)
  cat(sprintf("\n--- %s ---\n", label))
  cat(sprintf("Trades: %d | WR: %.1f%% | PF: %.2f | P&L: $%.2f | MDD: $%.2f | Sharpe: %.2f\n",
              n_total, wr*100, pf, tot, mdd, sh))
  list(wr=wr, pf=pf, tot=tot, mdd=mdd, sharpe=sh, n=n_total)
}

# --- 5. DATOS ----------------------------------------------------------------
cat(sprintf("Descargando %s | %s a %s\n", SYMBOL, DATE_FROM, DATE_TO))
raw <- fetch_massive(SYMBOL, DATE_FROM, DATE_TO, POLYGON_KEY)

raw <- raw %>%
  filter(format(datetime,"%H:%M") >= "09:30",
         format(datetime,"%H:%M") <= "15:55") %>%
  mutate(date = as.Date(datetime, tz="America/New_York"))

trading_days <- sort(unique(raw$date))
cat(sprintf("Velas: %d | Dias: %d\n", nrow(raw), length(trading_days)))

# --- 6. BACKTEST ORB ---------------------------------------------------------
cat("\nEjecutando ORB...\n")
orb_trades <- list()

for (day in as.character(trading_days)) {
  dd <- raw %>% filter(date==as.Date(day)) %>% arrange(datetime)
  if (nrow(dd) < ORB_CANDLES+1) next
  orb <- dd[1:ORB_CANDLES,]
  orb_high <- max(orb$high); orb_low <- min(orb$low)
  orb_range <- orb_high - orb_low; orb_mid <- (orb_high+orb_low)/2
  if (orb_range/orb_mid < MIN_RANGE_PCT) next
  avg_vol <- mean(orb$volume)
  post <- dd[(ORB_CANDLES+1):nrow(dd),]
  taken <- FALSE

  for (i in seq_len(nrow(post))) {
    if (taken) break
    cn <- post[i,]; vol_ok <- !VOL_FILTER || cn$volume > avg_vol

    if (cn$high > orb_high && vol_ok) {
      entry <- orb_high+0.01; stp <- orb_low; risk <- entry-stp; tgt <- entry+risk*RR_TARGET
      fut <- if ((i+1)<=nrow(post)) post[(i+1):nrow(post),] else post[0,]
      res <- "open"; ex <- NA
      for (j in seq_len(nrow(fut))) {
        if (!is.na(fut$low[j]) && fut$low[j]<=stp)  { res<-"stop";   ex<-stp; break }
        if (!is.na(fut$high[j])&& fut$high[j]>=tgt) { res<-"target"; ex<-tgt; break }
      }
      if (res=="open") { ex <- tail(dd$close,1); res <- "eod_close" }
      orb_trades[[length(orb_trades)+1]] <- tibble(
        date=as.Date(day), strategy="ORB", direction="LONG",
        result=res, pnl_usd=(ex-entry)*SHARES-COMMISSION)
      taken <- TRUE
    }
    if (!taken && cn$low < orb_low && vol_ok) {
      entry <- orb_low-0.01; stp <- orb_high; risk <- stp-entry; tgt <- entry-risk*RR_TARGET
      fut <- if ((i+1)<=nrow(post)) post[(i+1):nrow(post),] else post[0,]
      res <- "open"; ex <- NA
      for (j in seq_len(nrow(fut))) {
        if (!is.na(fut$high[j])&& fut$high[j]>=stp) { res<-"stop";   ex<-stp; break }
        if (!is.na(fut$low[j]) && fut$low[j]<=tgt)  { res<-"target"; ex<-tgt; break }
      }
      if (res=="open") { ex <- tail(dd$close,1); res <- "eod_close" }
      orb_trades[[length(orb_trades)+1]] <- tibble(
        date=as.Date(day), strategy="ORB", direction="SHORT",
        result=res, pnl_usd=(entry-ex)*SHARES-COMMISSION)
      taken <- TRUE
    }
  }
}

# --- 7. BACKTEST VWAP V3 (ADX filter) ----------------------------------------
cat("Ejecutando VWAP+ADX...\n")
vwap_trades <- list()

for (day in as.character(trading_days)) {
  dd <- raw %>% filter(date==as.Date(day)) %>% arrange(datetime)
  if (nrow(dd) < RSI_PERIOD+2) next
  dd <- dd %>% mutate(
    vwap = calc_vwap(high, low, close, volume),
    rsi  = calc_rsi(close, RSI_PERIOD),
    adx  = calc_adx(high, low, close, RSI_PERIOD)
  )
  long_taken <- FALSE; short_taken <- FALSE
  in_trade <- FALSE; direction <- NA; entry <- NA; stp <- NA

  for (i in seq_len(nrow(dd))) {
    row <- dd[i,]
    if (is.na(row$vwap)||is.na(row$rsi)||is.na(row$adx)) next
    if (row$adx >= ADX_MAX) next

    if (in_trade) {
      hs <- if(direction=="LONG") row$low<=stp else row$high>=stp
      ht <- if(direction=="LONG") row$high>=row$vwap else row$low<=row$vwap
      if (hs) {
        pnl <- if(direction=="LONG") (stp-entry)*SHARES-COMMISSION else (entry-stp)*SHARES-COMMISSION
        vwap_trades[[length(vwap_trades)+1]] <- tibble(date=as.Date(day), strategy="VWAP", direction=direction, result="stop", pnl_usd=pnl)
        in_trade <- FALSE; direction <- NA; next
      }
      if (ht) {
        ex <- row$vwap
        pnl <- if(direction=="LONG") (ex-entry)*SHARES-COMMISSION else (entry-ex)*SHARES-COMMISSION
        vwap_trades[[length(vwap_trades)+1]] <- tibble(date=as.Date(day), strategy="VWAP", direction=direction, result="target", pnl_usd=pnl)
        in_trade <- FALSE; direction <- NA; next
      }
      if (i==nrow(dd)) {
        ex <- row$close
        pnl <- if(direction=="LONG") (ex-entry)*SHARES-COMMISSION else (entry-ex)*SHARES-COMMISSION
        vwap_trades[[length(vwap_trades)+1]] <- tibble(date=as.Date(day), strategy="VWAP", direction=direction, result="eod_close", pnl_usd=pnl)
        in_trade <- FALSE
      }
      next
    }
    dev <- row$close - row$vwap; band <- abs(row$vwap)*VWAP_BAND_PCT
    if (!long_taken && dev < -band && row$rsi < RSI_LONG) {
      entry <- row$close; stp <- entry - STOP_MULT*abs(dev)
      in_trade <- TRUE; direction <- "LONG"; long_taken <- TRUE; next
    }
    if (!short_taken && dev > band && row$rsi > RSI_SHORT) {
      entry <- row$close; stp <- entry + STOP_MULT*abs(dev)
      in_trade <- TRUE; direction <- "SHORT"; short_taken <- TRUE; next
    }
  }
}

# --- 8. COMBINAR -------------------------------------------------------------
orb_df  <- bind_rows(orb_trades)  %>% arrange(date) %>% mutate(cum_pnl = cumsum(pnl_usd))
vwap_df <- bind_rows(vwap_trades) %>% arrange(date) %>% mutate(cum_pnl = cumsum(pnl_usd))

combined_daily <- tibble(date = trading_days) %>%
  left_join(orb_df  %>% group_by(date) %>% summarise(orb_pnl  = sum(pnl_usd)), by="date") %>%
  left_join(vwap_df %>% group_by(date) %>% summarise(vwap_pnl = sum(pnl_usd)), by="date") %>%
  mutate(
    orb_pnl  = if_else(is.na(orb_pnl),  0, orb_pnl),
    vwap_pnl = if_else(is.na(vwap_pnl), 0, vwap_pnl),
    total_pnl = orb_pnl + vwap_pnl,
    cum_orb   = cumsum(orb_pnl),
    cum_vwap  = cumsum(vwap_pnl),
    cum_total = cumsum(total_pnl)
  )

# --- 9. METRICAS -------------------------------------------------------------
cat("\n========================================\n")
cat("     PORTFOLIO: ORB + VWAP ADX — SPY\n")
cat("========================================\n")

m_orb  <- metrics(orb_df,  trading_days, "ORB solo")
m_vwap <- metrics(vwap_df, trading_days, "VWAP+ADX solo")

# Portfolio combinado
sh_port <- mean(combined_daily$total_pnl)/sd(combined_daily$total_pnl)*sqrt(252)
mdd_port <- min(combined_daily$cum_total - cummax(combined_daily$cum_total))
cat(sprintf("\n--- Portfolio combinado ---\n"))
cat(sprintf("P&L total:   $%.2f\n", sum(combined_daily$total_pnl)))
cat(sprintf("MDD:         $%.2f\n", mdd_port))
cat(sprintf("Sharpe:      %.2f\n", sh_port))
cat(sprintf("Correlacion diaria ORB vs VWAP: %.2f\n",
            cor(combined_daily$orb_pnl, combined_daily$vwap_pnl)))
cat("========================================\n")

# --- 10. GRAFICOS ------------------------------------------------------------
out <- function(f) file.path(OUTPUT_DIR, f)

# Curva de equity: 3 lineas
plot_data <- combined_daily %>%
  select(date, cum_orb, cum_vwap, cum_total) %>%
  tidyr::pivot_longer(-date, names_to="estrategia", values_to="pnl") %>%
  mutate(estrategia = recode(estrategia,
    cum_orb="ORB", cum_vwap="VWAP+ADX", cum_total="Portfolio"))

if (!requireNamespace("tidyr", quietly=TRUE)) install.packages("tidyr")
library(tidyr)

plot_data <- combined_daily %>%
  select(date, cum_orb, cum_vwap, cum_total) %>%
  pivot_longer(-date, names_to="estrategia", values_to="pnl") %>%
  mutate(estrategia = recode(estrategia,
    cum_orb="ORB", cum_vwap="VWAP+ADX", cum_total="Portfolio"))

p1 <- ggplot(plot_data, aes(x=date, y=pnl, color=estrategia, linewidth=estrategia)) +
  geom_line() +
  geom_hline(yintercept=0, linetype="dashed", color="gray50") +
  scale_color_manual(values=c("ORB"="#2196F3","VWAP+ADX"="#9C27B0","Portfolio"="#FF5722")) +
  scale_linewidth_manual(values=c("ORB"=0.8,"VWAP+ADX"=0.8,"Portfolio"=1.4)) +
  scale_y_continuous(labels=dollar_format()) +
  labs(
    title    = "Portfolio ORB + VWAP ADX — SPY | Curva de Equity",
    subtitle = sprintf("Portfolio P&L: $%.0f | Sharpe: %.2f | MDD: $%.0f",
                       sum(combined_daily$total_pnl), sh_port, mdd_port),
    x="Fecha", y="P&L Acumulado (USD)", color="", linewidth=""
  ) +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(face="bold"), legend.position="top")

ggsave(out("portfolio_equity_curve.png"), p1, width=11, height=5, dpi=150)

# P&L diario combinado — barras
p2 <- ggplot(combined_daily, aes(x=date, y=total_pnl, fill=total_pnl>0)) +
  geom_col(width=1) +
  scale_fill_manual(values=c("TRUE"="#4CAF50","FALSE"="#F44336"), guide="none") +
  scale_y_continuous(labels=dollar_format()) +
  geom_hline(yintercept=0, color="gray30") +
  labs(title="P&L Diario Portfolio", x="Fecha", y="P&L (USD)") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(face="bold"))

ggsave(out("portfolio_daily_pnl.png"), p2, width=11, height=4, dpi=150)

write.csv(combined_daily, out("portfolio_daily.csv"), row.names=FALSE)
cat("\nArchivos: portfolio_equity_curve.png | portfolio_daily_pnl.png | portfolio_daily.csv\n")
cat("Done.\n")
