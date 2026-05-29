#!/usr/bin/env python3
"""
BTC MA Crossover Bot — Alpaca Paper
=====================================
EMA(9) x EMA(21) on 15-min bars. Long-only. 24/7.
Buy on golden cross, close on death cross.
"""

import os, sys, time, signal, logging
from datetime import datetime, timedelta, timezone

try:
    from zoneinfo import ZoneInfo
    ET = ZoneInfo("America/New_York")
except ImportError:
    ET = timezone(timedelta(hours=-5))

from alpaca.trading.client import TradingClient
from alpaca.trading.requests import MarketOrderRequest
from alpaca.trading.enums import OrderSide, TimeInForce
from alpaca.data.historical.crypto import CryptoHistoricalDataClient
from alpaca.data.requests import CryptoBarsRequest
from alpaca.data.timeframe import TimeFrame, TimeFrameUnit

# =============================================================================
# CONFIG
# =============================================================================
ALPACA_KEY    = os.environ.get("ALPACA_KEY", "")
ALPACA_SECRET = os.environ.get("ALPACA_SECRET", "")
PAPER         = os.environ.get("ALPACA_PAPER", "true").lower() == "true"

SYMBOL_DATA   = "BTC/USD"
SYMBOL_TRADE  = "BTCUSD"
CAPITAL       = float(os.environ.get("CAPITAL_BTC", "500"))
FAST_PERIOD   = int(os.environ.get("FAST_EMA", "9"))
SLOW_PERIOD   = int(os.environ.get("SLOW_EMA", "21"))
TIMEFRAME     = TimeFrame(15, TimeFrameUnit.Minute)
POLL_SEC      = 60

# =============================================================================
# LOGGING
# =============================================================================
class _ETFormatter(logging.Formatter):
    def formatTime(self, record, datefmt=None):
        return datetime.fromtimestamp(record.created, ET).strftime("%H:%M:%S ET")

_h = logging.StreamHandler()
_h.setFormatter(_ETFormatter("%(asctime)s [%(levelname)s] %(message)s"))
logging.basicConfig(level=logging.INFO, handlers=[_h])
log = logging.getLogger("BTC_MA")

# =============================================================================
# CLIENTS
# =============================================================================
trade_client = TradingClient(ALPACA_KEY, ALPACA_SECRET, paper=PAPER)
data_client  = CryptoHistoricalDataClient(ALPACA_KEY, ALPACA_SECRET)

run_bot = True

def on_signal(sig, frame):
    global run_bot
    log.info("Señal de salida. Cerrando...")
    run_bot = False

signal.signal(signal.SIGINT, on_signal)
signal.signal(signal.SIGTERM, on_signal)

# =============================================================================
# HELPERS
# =============================================================================
def now_et():
    return datetime.now(ET)

def ema(prices: list, period: int) -> float:
    if not prices:
        return 0.0
    if len(prices) < period:
        return sum(prices) / len(prices)
    k = 2.0 / (period + 1)
    val = prices[0]
    for p in prices[1:]:
        val = p * k + val * (1 - k)
    return val

def fetch_bars(n: int) -> list:
    """Fetch last n completed 15-min bars (drops current incomplete bar)."""
    try:
        req = CryptoBarsRequest(
            symbol_or_symbols=SYMBOL_DATA,
            timeframe=TIMEFRAME,
            limit=n + 2,
        )
        resp = data_client.get_crypto_bars(req)
        if not resp or not resp.data or SYMBOL_DATA not in resp.data:
            return []
        raw = resp.data[SYMBOL_DATA]
        bars = [
            {"t": b.timestamp, "c": float(b.close), "v": float(b.volume)}
            for b in raw
        ]
        return bars[:-1]  # drop potentially incomplete last bar
    except Exception as e:
        log.warning(f"fetch_bars error: {e}")
        return []

def has_position() -> bool:
    try:
        pos = trade_client.get_open_position(SYMBOL_TRADE)
        return pos is not None and float(pos.qty) > 0
    except Exception as e:
        if "40410000" not in str(e):
            log.warning(f"position check error: {e}")
        return False

def buy(price: float):
    qty = round(CAPITAL / price, 6)
    if qty <= 0:
        log.warning("qty=0, skip buy")
        return
    try:
        order = trade_client.submit_order(
            MarketOrderRequest(
                symbol=SYMBOL_TRADE,
                qty=qty,
                side=OrderSide.BUY,
                time_in_force=TimeInForce.GTC,
            )
        )
        log.info(f"  BUY {qty} BTC @ ~${price:,.2f} | order={order.id}")
    except Exception as e:
        log.error(f"  buy error: {e}")

def sell():
    try:
        trade_client.close_position(SYMBOL_TRADE)
        log.info("  SELL — position closed")
    except Exception as e:
        if "40410000" not in str(e):
            log.error(f"  sell error: {e}")

def account_summary():
    try:
        a = trade_client.get_account()
        log.info(f"  Account | Equity=${float(a.equity):,.2f} | Cash=${float(a.cash):,.2f}")
    except Exception as e:
        log.warning(f"  account error: {e}")

# =============================================================================
# MAIN LOOP
# =============================================================================
def main():
    global run_bot

    log.info("=" * 50)
    log.info(" BTC MA CROSSOVER — Alpaca Paper Trading")
    log.info(f" {SYMBOL_DATA} | Capital: ${CAPITAL} | EMA({FAST_PERIOD}) x EMA({SLOW_PERIOD})")
    log.info(f" Timeframe: 15min | Poll: {POLL_SEC}s | 24/7")
    log.info("=" * 50)

    if not ALPACA_KEY or not ALPACA_SECRET:
        log.critical("ALPACA_KEY / ALPACA_SECRET missing. Exit.")
        sys.exit(1)

    account_summary()

    in_position = has_position()
    log.info(f"  Initial state: {'LONG' if in_position else 'FLAT'}")

    last_bar_time = None

    while run_bot:
        try:
            bars = fetch_bars(SLOW_PERIOD + 5)

            if len(bars) < SLOW_PERIOD + 1:
                log.debug(f"Not enough bars ({len(bars)}), waiting...")
                time.sleep(POLL_SEC)
                continue

            latest = bars[-1]
            if latest["t"] == last_bar_time:
                time.sleep(POLL_SEC)
                continue

            last_bar_time = latest["t"]
            closes = [b["c"] for b in bars]

            fast_now  = ema(closes,       FAST_PERIOD)
            fast_prev = ema(closes[:-1],  FAST_PERIOD)
            slow_now  = ema(closes,       SLOW_PERIOD)
            slow_prev = ema(closes[:-1],  SLOW_PERIOD)

            cross_up   = fast_prev <= slow_prev and fast_now > slow_now
            cross_down = fast_prev >= slow_prev and fast_now < slow_now

            bar_et = latest["t"].astimezone(ET)
            signal_label = "↑ GOLDEN CROSS" if cross_up else "↓ DEATH CROSS" if cross_down else "—"
            log.info(
                f"  {bar_et.strftime('%m/%d %H:%M')} | "
                f"C=${latest['c']:,.2f} | "
                f"EMA{FAST_PERIOD}={fast_now:.2f} EMA{SLOW_PERIOD}={slow_now:.2f} | "
                f"{signal_label} | "
                f"{'LONG' if in_position else 'FLAT'}"
            )

            if cross_up and not in_position:
                log.info("  >>> BUY <<<")
                buy(latest["c"])
                in_position = True

            elif cross_down and in_position:
                log.info("  >>> SELL <<<")
                sell()
                in_position = False

            time.sleep(POLL_SEC)

        except KeyboardInterrupt:
            break
        except Exception as e:
            log.error(f"Loop error: {e}", exc_info=True)
            time.sleep(30)

    log.info("Bot stopped.")

if __name__ == "__main__":
    main()
