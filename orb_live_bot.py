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

import os, sys, time, signal, csv, logging
from datetime import datetime, date, timedelta, timezone
from pathlib import Path
from typing import Optional, Dict, Any

# ── Timezone ET (Python 3.9+ built-in) ──
try:
    from zoneinfo import ZoneInfo
    ET = ZoneInfo("America/New_York")
except ImportError:
    ET = timezone(timedelta(hours=-5))

# ── Alpaca SDK ──
from alpaca.trading.client import TradingClient
from alpaca.trading.requests import MarketOrderRequest, StopLossRequest, TakeProfitRequest
from alpaca.trading.enums import OrderSide, OrderType, TimeInForce, QueryOrderStatus
from alpaca.data.historical import StockHistoricalDataClient
from alpaca.data.requests import StockBarsRequest, StockLatestBarRequest
from alpaca.data.timeframe import TimeFrame, TimeFrameUnit

# =============================================================================
# CONFIGURACION (variables de entorno o valores directos)
# =============================================================================
ALPACA_KEY      = os.environ.get("ALPACA_KEY", "")
ALPACA_SECRET   = os.environ.get("ALPACA_SECRET", "")
PAPER           = os.environ.get("ALPACA_PAPER", "true").lower() == "true"

SYMBOLS         = ["SPY", "QQQ", "IWM", "GLD"]
CAPITAL_PER     = int(os.environ.get("CAPITAL_PER", "250"))

ORB_CANDLES     = 6
RR_RATIO        = float(os.environ.get("RR_RATIO", "2.5"))
MIN_RANGE_PCT   = 0.001
VOLUME_FILTER   = True

FRAME_5MIN      = TimeFrame(5, TimeFrameUnit.Minute)
TRADE_LOG       = Path("C:/Users/solis/Downloads/classic strategies/orb_live_trades.csv")

# =============================================================================
# LOGGING
# =============================================================================
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    datefmt="%H:%M:%S"
)
log = logging.getLogger("ORB")

# =============================================================================
# CLIENTS
# =============================================================================
trade_client = TradingClient(ALPACA_KEY, ALPACA_SECRET, paper=PAPER)
data_client  = StockHistoricalDataClient(ALPACA_KEY, ALPACA_SECRET)

# =============================================================================
# HOLIDAY CHECK (via Alpaca Clock API)
# =============================================================================
def market_is_open_now() -> bool:
    """Check if US stock market is open right now using Alpaca Clock API."""
    try:
        clock = trade_client.get_clock()
        return clock.is_open
    except Exception:
        # Fallback: assume closed if API fails
        return False

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
                  "avg_vol": 0, "entry": 0, "stop": 0, "target": 0, "dir": ""}

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
    t = now_et().hour * 60 + now_et().minute
    if t < 565:   return "pre"
    if t <= 600:  return "orb"
    if t <= 950:  return "mon"
    return "eod"

def has_open_position(sym: str) -> bool:
    """Check if there's already an open position for this symbol."""
    try:
        pos = trade_client.get_open_position(sym)
        return pos is not None and float(pos.qty) != 0
    except:
        return False

def fetch_bar(symbol: str) -> Optional[dict]:
    """Get latest completed 5-min bar, validated for today."""
    try:
        req = StockBarsRequest(
            symbol_or_symbols=symbol,
            timeframe=FRAME_5MIN,
            limit=5,
            start=(now_et() - timedelta(hours=7)).isoformat()
        )
        bars = data_client.get_stock_bars(req)
        if not bars or not bars.data:
            return None
        
        today = now_et().date()
        # Walk backwards to find the most recent bar from today
        for b in reversed(bars.data):
            bar_date = b.timestamp.date() if hasattr(b.timestamp, 'date') else b.timestamp
            if isinstance(bar_date, datetime):
                bar_date = bar_date.date()
            # Reject bars from other days or pre-market
            bar_hour = b.timestamp.hour if hasattr(b.timestamp, 'hour') else b.timestamp.hour
            bar_min  = b.timestamp.minute if hasattr(b.timestamp, 'minute') else b.timestamp.minute
            bar_minutes = bar_hour * 60 + bar_min
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

def shares_for(price: float) -> float:
    return round(CAPITAL_PER / price, 6) if price > 0 else 0

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
                stop_loss=StopLossRequest(stop_price=str(sl_price)),
                take_profit=TakeProfitRequest(limit_price=str(tp_price))
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
    except:
        pass

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

        if bar["h"] > orb_h and vol_ok:
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
            return

        if bar["l"] < orb_l and vol_ok:
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
            return

def reset_day():
    global state
    for sym in SYMBOLS:
        state[sym] = {"s": State.WAITING, "bars": [], "orb_h": 0, "orb_l": 0,
                      "avg_vol": 0, "entry": 0, "stop": 0, "target": 0, "dir": ""}

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

            # ── Skip non-trading days ──
            if n.weekday() >= 5 or not market_is_open_now():
                next_check = (n + timedelta(days=1)).replace(hour=9, minute=0, second=0, microsecond=0)
                sleep_sec = min((next_check - n).total_seconds(), 3600)
                if sleep_sec > 0:
                    time.sleep(sleep_sec)
                continue

            phase = orb_phase()

            # ── Pre-open: exponential sleep approaching 9:25 ──
            if phase == "pre":
                remain = 565 - (n.hour * 60 + n.minute)
                sleep_time = 60 if remain > 10 else (5 if remain > 2 else 1)
                time.sleep(sleep_time)
                continue

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
