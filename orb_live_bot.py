#!/usr/bin/env python3
"""
ORB Live Trading Bot — Alpaca Paper
====================================
Opening Range Breakout en SPY, QQQ, IWM, GLD (5min)
RR 2.5:1 · 1 trade/dia/activo · EOD close 15:50 ET

Instalacion: pip install alpaca-py
Ejecucion:   python orb_live_bot.py
             Ctrl+C para detener
"""

import os, sys, time, signal, csv, logging, math
from datetime import datetime, timedelta, timezone
from pathlib import Path
from typing import Optional

# ── Timezone ET (Python 3.9+ built-in) ──
try:
    from zoneinfo import ZoneInfo
    ET = ZoneInfo("America/New_York")
except ImportError:
    ET = timezone(timedelta(hours=-5))

# ── Alpaca SDK ──
from alpaca.trading.client import TradingClient
from alpaca.trading.requests import MarketOrderRequest, StopLossRequest, TakeProfitRequest
from alpaca.trading.enums import OrderSide, TimeInForce
from alpaca.data.historical import StockHistoricalDataClient
from alpaca.data.requests import StockBarsRequest
from alpaca.data.timeframe import TimeFrame, TimeFrameUnit

# =============================================================================
# CONFIGURACION (variables de entorno o valores directos)
# =============================================================================
ALPACA_KEY      = os.environ.get("ALPACA_KEY", "")
ALPACA_SECRET   = os.environ.get("ALPACA_SECRET", "")
PAPER           = os.environ.get("ALPACA_PAPER", "true").lower() == "true"

SYMBOLS         = ["SPY", "QQQ", "IWM", "GLD"]
CAPITAL_PER     = int(os.environ.get("CAPITAL_PER", "1000"))

ORB_CANDLES     = 6
RR_RATIO        = float(os.environ.get("RR_RATIO", "2.5"))
MIN_RANGE_PCT   = 0.001
VOLUME_FILTER   = True

FRAME_5MIN      = TimeFrame(5, TimeFrameUnit.Minute)
TRADE_LOG       = Path("orb_live_trades.csv")

# =============================================================================
# LOGGING
# =============================================================================
class _ETFormatter(logging.Formatter):
    def formatTime(self, record, datefmt=None):
        return datetime.fromtimestamp(record.created, ET).strftime("%H:%M:%S ET")

_handler = logging.StreamHandler()
_handler.setFormatter(_ETFormatter("%(asctime)s [%(levelname)s] %(message)s"))
logging.basicConfig(level=logging.INFO, handlers=[_handler])
log = logging.getLogger("ORB")

# =============================================================================
# CLIENTS
# =============================================================================
trade_client = TradingClient(ALPACA_KEY, ALPACA_SECRET, paper=PAPER)
data_client  = StockHistoricalDataClient(ALPACA_KEY, ALPACA_SECRET)

# =============================================================================
# HOLIDAY CHECK (via Alpaca Clock API) — cached 60s to avoid rate limits
# =============================================================================
_mkt_cache: dict = {"v": False, "t": 0.0}

def market_is_open_now() -> bool:
    if time.monotonic() - _mkt_cache["t"] < 60:
        return _mkt_cache["v"]
    try:
        _mkt_cache["v"] = trade_client.get_clock().is_open
    except Exception:
        _mkt_cache["v"] = False
    _mkt_cache["t"] = time.monotonic()
    return _mkt_cache["v"]

# =============================================================================
# STATE MACHINE
# =============================================================================
class State:
    WAITING  = 0
    COLLECT  = 1
    MONITOR  = 2
    TRADING  = 3
    DONE     = 4

# Per-symbol state
state = {}
for sym in SYMBOLS:
    state[sym] = {"s": State.WAITING, "bars": [], "orb_h": 0, "orb_l": 0,
                  "avg_vol": 0, "entry": 0, "stop": 0, "target": 0, "dir": "",
                  "monitor_bars": []}

run_bot = True

def on_signal(sig, frame):
    global run_bot
    log.info("Senal de salida. Cerrando...")
    run_bot = False

signal.signal(signal.SIGINT, on_signal)
signal.signal(signal.SIGTERM, on_signal)

# =============================================================================
# HELPERS
# =============================================================================
def now_et():
    return datetime.now(ET)

def market_is_open() -> bool:
    n = now_et()
    if n.weekday() >= 5:
        return False
    minutes = n.hour * 60 + n.minute
    if not (565 <= minutes < 960):
        return False
    # Final check: Alpaca says market is open?
    return market_is_open_now()

def orb_phase() -> str:
    n = now_et()
    t = n.hour * 60 + n.minute
    if t < 565:   return "pre"
    if t <= 600:  return "orb"
    if t <= 950:  return "mon"
    return "eod"

def has_open_position(sym: str) -> bool:
    try:
        pos = trade_client.get_open_position(sym)
        return pos is not None and float(pos.qty) != 0
    except Exception as e:
        if "40410000" not in str(e):
            log.warning(f"  {sym} position check error: {e}")
        return False

def fetch_bar(symbol: str) -> Optional[dict]:
    """Get latest completed 5-min bar, validated for today."""
    try:
        req = StockBarsRequest(
            symbol_or_symbols=symbol,
            timeframe=FRAME_5MIN,
            limit=5,
            start=(now_et() - timedelta(minutes=15)).isoformat()
        )
        bars = data_client.get_stock_bars(req)
        if not bars or not bars.data or symbol not in bars.data:
            return None

        today = now_et().date()
        for b in reversed(bars.data[symbol]):
            bar_et = b.timestamp.astimezone(ET)
            bar_date = bar_et.date()
            bar_minutes = bar_et.hour * 60 + bar_et.minute
            if bar_date == today and bar_minutes >= 565 and bar_minutes <= 955:
                return {
                    "t": b.timestamp,
                    "o": float(b.open), "h": float(b.high),
                    "l": float(b.low), "c": float(b.close),
                    "v": float(b.volume)
                }
    except Exception as e:
        log.debug(f"{symbol} bar error: {e}")
    return None

def current_price(symbol: str) -> float:
    bar = fetch_bar(symbol)
    return bar["c"] if bar else 0

def seed_orb_if_late(sym: str):
    """If bot starts/restarts past ORB window, replay historical ORB bars."""
    try:
        today = now_et().date()
        orb_start = datetime(today.year, today.month, today.day, 9, 30, tzinfo=ET)
        orb_end   = datetime(today.year, today.month, today.day, 10, 1, tzinfo=ET)
        req = StockBarsRequest(
            symbol_or_symbols=sym,
            timeframe=FRAME_5MIN,
            start=orb_start.isoformat(),
            end=orb_end.isoformat(),
        )
        bars = data_client.get_stock_bars(req)
        if not bars or not bars.data or sym not in bars.data:
            state[sym]["s"] = State.DONE
            log.warning(f"  {sym}: no ORB bars in history, skipping today")
            return
        orb_bars = [
            {"t": b.timestamp, "o": float(b.open), "h": float(b.high),
             "l": float(b.low),  "c": float(b.close), "v": float(b.volume)}
            for b in bars.data[sym]
        ]
        if len(orb_bars) < ORB_CANDLES:
            state[sym]["s"] = State.DONE
            log.warning(f"  {sym}: only {len(orb_bars)} ORB bars available, skipping today")
            return
        for b in orb_bars[:ORB_CANDLES]:
            process_bar(sym, b)
        log.info(f"  {sym}: ORB seeded from history | H={state[sym]['orb_h']:.2f} L={state[sym]['orb_l']:.2f}")
    except Exception as e:
        log.warning(f"  {sym}: ORB seed error: {e}")
        state[sym]["s"] = State.DONE

def shares_for(price: float) -> int:
    return max(1, math.floor(CAPITAL_PER / price)) if price > 0 else 1

def submit_trade(sym: str, direction: str, entry: float, stop: float, target: float, shares: float):
    """Submit market entry with bracket (SL + TP)."""
    side = OrderSide.BUY if direction == "LONG" else OrderSide.SELL

    sl_price = round(stop, 2)
    tp_price = round(target, 2)

    # Validate: SL must be on the "losing" side, TP on the "winning" side
    if direction == "LONG":
        if sl_price >= entry - 0.01:
            sl_price = round(entry - (entry - stop) if entry > stop else entry - 0.10, 2)
        if tp_price <= entry + 0.01:
            tp_price = round(entry + (target - entry) if target > entry else entry + 0.10, 2)
    else:
        if sl_price <= entry + 0.01:
            sl_price = round(entry + (stop - entry) if stop > entry else entry + 0.10, 2)
        if tp_price >= entry - 0.01:
            tp_price = round(entry - (entry - target) if target < entry else entry - 0.10, 2)

    try:
        order = trade_client.submit_order(
            order_data=MarketOrderRequest(
                symbol=sym, qty=shares, side=side, time_in_force=TimeInForce.DAY,
                stop_loss=StopLossRequest(stop_price=sl_price),
                take_profit=TakeProfitRequest(limit_price=tp_price)
            )
        )
        log.info(f"  {sym} {direction}: E=${entry:.2f} SL=${sl_price:.2f} TP=${tp_price:.2f} | ID={order.id}")

        # Log trade
        log_trade({
            "date": now_et().strftime("%Y-%m-%d"),
            "symbol": sym, "direction": direction,
            "entry": entry, "stop": sl_price, "target": tp_price,
            "shares": shares, "order_id": order.id
        })
        return order.id
    except Exception as e:
        log.error(f"  {sym} order error: {e}")
        return None

def log_trade(data: dict):
    """Append trade to CSV."""
    file_exists = TRADE_LOG.exists()
    try:
        with open(TRADE_LOG, "a", newline="") as f:
            writer = csv.DictWriter(f, fieldnames=data.keys())
            if not file_exists:
                writer.writeheader()
            writer.writerow(data)
    except Exception as e:
        log.warning(f"Trade log error: {e}")

def account_summary():
    try:
        a = trade_client.get_account()
        log.info(f"  Account | Equity=${float(a.equity):.2f} | Cash=${float(a.cash):.2f} | BP=${float(a.buying_power):.2f}")
    except Exception as e:
        log.warning(f"  account error: {e}")

# =============================================================================
# DAY LOGIC
# =============================================================================
def process_bar(sym: str, bar: dict):
    """Handle a new 5-min bar for one symbol."""
    s = state[sym]

    # ── COLLECT ──
    if s["s"] == State.WAITING:
        s["s"] = State.COLLECT
        s["bars"] = [bar]
        log.info(f"  {sym}: ORB bar 1/6 | O={bar['o']:.2f} H={bar['h']:.2f} L={bar['l']:.2f} C={bar['c']:.2f}")
        return

    if s["s"] == State.COLLECT:
        s["bars"].append(bar)
        n_bars = len(s["bars"])
        log.info(f"  {sym}: ORB bar {n_bars}/6 | H={bar['h']:.2f} L={bar['l']:.2f} C={bar['c']:.2f}")
        if n_bars >= ORB_CANDLES:
            bars = s["bars"]
            orb_h = max(b["h"] for b in bars)
            orb_l = min(b["l"] for b in bars)
            rng = orb_h - orb_l
            mid = (orb_h + orb_l) / 2
            avg_vol = sum(b["v"] for b in bars) / ORB_CANDLES

            if rng / mid < MIN_RANGE_PCT:
                log.info(f"  {sym}: ORB rango muy chico ({rng:.2f}, {rng/mid*100:.2f}%), sin trade")
                s["s"] = State.DONE
                return

            s["orb_h"] = orb_h; s["orb_l"] = orb_l; s["avg_vol"] = avg_vol
            s["s"] = State.MONITOR
            log.info(f"  {sym}: ORB LISTO | H=${orb_h:.2f} L=${orb_l:.2f} R=${rng:.2f} Vol={avg_vol:.0f}")
        return

    # ── MONITOR ──
    if s["s"] == State.MONITOR:
        orb_h = s["orb_h"]; orb_l = s["orb_l"]; avg_vol = s["avg_vol"]
        vol_ok = not VOLUME_FILTER or bar["v"] > avg_vol

        if bar["h"] > orb_h:
            if vol_ok:
                entry = orb_h + 0.01
                stop = orb_l
                risk = entry - stop
                target = entry + risk * RR_RATIO
                px = current_price(sym) or entry
                sh = shares_for(px)

                s["entry"] = entry; s["stop"] = stop; s["target"] = target
                s["dir"] = "LONG"; s["s"] = State.TRADING
                log.info(f"  {sym} >>> LONG BREAKOUT <<< Entry=${entry:.2f} Stop=${stop:.2f} Target=${target:.2f} Shares={sh:.4f}")
                submit_trade(sym, "LONG", px, stop, target, sh)
            else:
                log.info(f"  {sym}: LONG setup bloqueado por volumen | bar_vol={bar['v']:.0f} < avg={avg_vol:.0f}")
            return

        if bar["l"] < orb_l:
            if vol_ok:
                entry = orb_l - 0.01
                stop = orb_h
                risk = stop - entry
                target = entry - risk * RR_RATIO
                px = current_price(sym) or entry
                sh = shares_for(px)

                s["entry"] = entry; s["stop"] = stop; s["target"] = target
                s["dir"] = "SHORT"; s["s"] = State.TRADING
                log.info(f"  {sym} >>> SHORT BREAKOUT <<< Entry=${entry:.2f} Stop=${stop:.2f} Target=${target:.2f} Shares={sh:.4f}")
                submit_trade(sym, "SHORT", px, stop, target, sh)
            else:
                log.info(f"  {sym}: SHORT setup bloqueado por volumen | bar_vol={bar['v']:.0f} < avg={avg_vol:.0f}")
            return

        # No breakout — log bar + distance to ORB levels
        bar_et = bar["t"].astimezone(ET)
        s["monitor_bars"].append(bar)
        dist_h = orb_h - bar["h"]
        dist_l = bar["l"] - orb_l
        recent = s["monitor_bars"][-5:]
        hist = " | ".join(
            f"{b['t'].astimezone(ET).strftime('%H:%M')} H={b['h']:.2f} L={b['l']:.2f} C={b['c']:.2f}"
            for b in recent
        )
        log.info(
            f"  {sym}: sin breakout {bar_et.strftime('%H:%M')} | "
            f"H={bar['h']:.2f}(Δ{dist_h:+.2f} vs ORB_H) "
            f"L={bar['l']:.2f}(Δ{dist_l:+.2f} vs ORB_L) "
            f"C={bar['c']:.2f} Vol={bar['v']:.0f}"
        )
        log.info(f"  {sym}: histórico reciente → {hist}")

def reset_day():
    global state
    for sym in SYMBOLS:
        state[sym] = {"s": State.WAITING, "bars": [], "orb_h": 0, "orb_l": 0,
                      "avg_vol": 0, "entry": 0, "stop": 0, "target": 0, "dir": "",
                      "monitor_bars": []}

# =============================================================================
# MAIN LOOP
# =============================================================================
def main():
    global run_bot

    log.info("=" * 55)
    log.info(" ORB LIVE BOT — Alpaca Paper Trading")
    log.info(f" Symbols: {' '.join(SYMBOLS)} | Capital/asset: ${CAPITAL_PER}")
    log.info(f" RR: {RR_RATIO}:1 | ORB: {ORB_CANDLES} x 5min = 30min")
    log.info(f" EOD close: 15:50 ET | Holidays: auto-detected")
    log.info(" Ctrl+C to stop")
    log.info("=" * 55)

    if not ALPACA_KEY or not ALPACA_SECRET:
        log.critical("ALPACA_KEY / ALPACA_SECRET no definidas. Saliendo.")
        sys.exit(1)

    account_summary()

    last_day = None
    eod_done = False
    seen_bars = {s: None for s in SYMBOLS}

    while run_bot:
        try:
            n = now_et()
            today = n.date()

            # ── New day ──
            if today != last_day:
                last_day = today
                eod_done = False
                reset_day()
                seen_bars = {s: None for s in SYMBOLS}
                log.info(f"\n=== {today} ({n.strftime('%A')}) ===\n")
                account_summary()
                # Late start: ORB window already passed — seed from history
                if orb_phase() in ("mon", "eod"):
                    log.info("Late start detected — seeding ORB from history...")
                    for sym in SYMBOLS:
                        seed_orb_if_late(sym)

            # ── Skip non-trading days ──
            if n.weekday() >= 5 or not market_is_open():
                n_et = now_et()
                orb_open = n_et.replace(hour=9, minute=25, second=0, microsecond=0)
                if n_et >= orb_open:
                    orb_open += timedelta(days=1)
                # Cap at 5 min so bot wakes by 9:30 even when orb_open overshoots to tomorrow
                sleep_sec = min((orb_open - n_et).total_seconds(), 300)
                if sleep_sec > 0:
                    time.sleep(sleep_sec)
                continue

            phase = orb_phase()

            # ── EOD: close positions, then sleep until tomorrow ──
            if phase == "eod" and not eod_done:
                log.info("=== EOD 15:50 — Closing all positions ===")
                try:
                    trade_client.close_all_positions(cancel_orders=True)
                    log.info("  Done.")
                except Exception as e:
                    log.warning(f"  EOD close error: {e}")
                account_summary()
                eod_done = True

                # Sleep until 6:00 AM next day
                tomorrow = (n + timedelta(days=1)).replace(hour=6, minute=0, second=0, microsecond=0)
                sleep_sec = (tomorrow - n).total_seconds()
                if sleep_sec > 0:
                    log.info(f"  Sleeping {sleep_sec/3600:.1f}h until tomorrow...")
                    time.sleep(sleep_sec)
                continue

            # ── Trading: poll bars every 5 seconds ──
            if phase in ("orb", "mon"):
                if phase == "mon":
                    for sym in SYMBOLS:
                        st = state[sym]["s"]
                        if st == State.WAITING:
                            log.info(f"  {sym}: missed ORB window, seeding from history...")
                            seed_orb_if_late(sym)
                        elif st == State.COLLECT:
                            # Bot restarted mid-ORB — partial bars, must re-seed clean
                            log.info(f"  {sym}: incomplete ORB ({len(state[sym]['bars'])} bars), re-seeding...")
                            state[sym]["s"] = State.WAITING
                            state[sym]["bars"] = []
                            seed_orb_if_late(sym)

                for sym in SYMBOLS:
                    s = state[sym]

                    # Skip if already traded or done
                    if s["s"] == State.DONE:
                        continue

                    # Skip if trading — Alpaca handles the bracket
                    if s["s"] == State.TRADING:
                        continue

                    # Skip if already have a position for this symbol (safety check)
                    if s["s"] == State.WAITING and has_open_position(sym):
                        log.warning(f"  {sym}: Posicion abierta detectada, saltando hoy")
                        s["s"] = State.DONE
                        continue

                    bar = fetch_bar(sym)
                    if not bar:
                        continue

                    bar_time = bar["t"]
                    if seen_bars[sym] == bar_time:
                        continue  # same bar, skip

                    seen_bars[sym] = bar_time
                    process_bar(sym, bar)

            time.sleep(5)

        except KeyboardInterrupt:
            break
        except Exception as e:
            log.error(f"Loop error: {e}", exc_info=True)
            time.sleep(30)

    # ── Clean shutdown ──
    log.info("Bot stopped.")
    log.info(f"Trades logged to: {TRADE_LOG}")

if __name__ == "__main__":
    main()
