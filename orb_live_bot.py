import os, time, math
from datetime import datetime, timedelta
from zoneinfo import ZoneInfo

from alpaca.trading.client import TradingClient
from alpaca.trading.requests import MarketOrderRequest, StopLossRequest, TakeProfitRequest
from alpaca.trading.enums import OrderSide, TimeInForce
from alpaca.data.historical import StockHistoricalDataClient
from alpaca.data.requests import StockBarsRequest
from alpaca.data.timeframe import TimeFrame, TimeFrameUnit
from alpaca.data.enums import DataFeed

ET = ZoneInfo("America/New_York")

API_KEY    = os.environ.get("ALPACA_KEY", "")
API_SECRET = os.environ.get("ALPACA_SECRET", "")

trade = TradingClient(API_KEY, API_SECRET, paper=True)
data  = StockHistoricalDataClient(API_KEY, API_SECRET)

SYMBOLS = ["SPY", "QQQ", "IWM", "GLD"]
CAPITAL = 250
RR      = 2.5

def now_et():
    return datetime.now(ET)

def get_bars(symbol, start, end=None, limit=None):
    try:
        kwargs = {
            "symbol_or_symbols": symbol,
            "timeframe": TimeFrame(5, TimeFrameUnit.Minute),
            "feed": DataFeed.IEX,
            "start": start.isoformat(),
        }
        if end:   kwargs["end"]   = end.isoformat()
        if limit: kwargs["limit"] = limit
        resp = data.get_stock_bars(StockBarsRequest(**kwargs))
        if not resp or not resp.data or symbol not in resp.data:
            return []
        return [{"t": b.timestamp, "h": float(b.high), "l": float(b.low),
                 "c": float(b.close), "v": float(b.volume)} for b in resp.data[symbol]]
    except Exception as e:
        print(f"[ERROR] get_bars {symbol}: {e}")
        return []

def has_position(symbol):
    try:
        return float(trade.get_open_position(symbol).qty) != 0
    except:
        return False

orb      = {s: {"h": None, "l": None, "avg_vol": 0, "traded": False} for s in SYMBOLS}
last_day = None
eod_done = False

print(f"ORB BOT — {' '.join(SYMBOLS)} | Capital=${CAPITAL} | RR={RR}")

while True:
    try:
        n     = now_et()
        today = n.date()
        t     = n.hour * 60 + n.minute

        # New day reset
        if today != last_day:
            last_day = today
            eod_done = False
            orb = {s: {"h": None, "l": None, "avg_vol": 0, "traded": False} for s in SYMBOLS}
            print(f"\n=== {today} ({n.strftime('%A')}) ===")

        # Weekend or pre-market (before 9:30)
        if n.weekday() >= 5 or t < 570:
            time.sleep(60)
            continue

        # ORB window 9:30–10:00: wait for all 6 bars to complete
        if t < 601:
            print(f"[{n.strftime('%H:%M')}] ORB window open, waiting...")
            time.sleep(30)
            continue

        # EOD 15:50: close all
        if t >= 950 and not eod_done:
            try:
                trade.close_all_positions(cancel_orders=True)
                print("EOD: closed all positions")
            except Exception as e:
                print(f"EOD error: {e}")
            eod_done = True

        if eod_done:
            time.sleep(60)
            continue

        # Build ORB from history if not done yet
        for sym in SYMBOLS:
            if orb[sym]["h"] is None:
                orb_start = n.replace(hour=9,  minute=30, second=0, microsecond=0)
                orb_end   = n.replace(hour=10, minute=1,  second=0, microsecond=0)
                bars = get_bars(sym, orb_start, orb_end)
                if len(bars) >= 6:
                    b6 = bars[:6]
                    orb[sym]["h"]       = max(b["h"] for b in b6)
                    orb[sym]["l"]       = min(b["l"] for b in b6)
                    orb[sym]["avg_vol"] = sum(b["v"] for b in b6) / 6
                    print(f"{sym}: ORB H={orb[sym]['h']:.2f} L={orb[sym]['l']:.2f} AvgVol={orb[sym]['avg_vol']:.0f}")
                else:
                    print(f"{sym}: only {len(bars)} ORB bars — skipping today")
                    orb[sym]["h"] = 0  # mark attempted

        # Monitor breakouts
        bar_start = (n - timedelta(minutes=15)).replace(second=0, microsecond=0)
        for sym in SYMBOLS:
            if not orb[sym]["h"] or orb[sym]["traded"] or has_position(sym):
                continue

            bars = get_bars(sym, bar_start, limit=5)
            if len(bars) < 2:
                continue
            bar    = bars[-2]  # last complete bar
            orb_h  = orb[sym]["h"]
            orb_l  = orb[sym]["l"]
            vol_ok = bar["v"] > orb[sym]["avg_vol"]

            if bar["h"] > orb_h and vol_ok:
                entry  = round(orb_h + 0.01, 2)
                stop   = round(orb_l, 2)
                target = round(entry + (entry - stop) * RR, 2)
                qty    = max(1, math.floor(CAPITAL / entry))
                trade.submit_order(MarketOrderRequest(
                    symbol=sym, qty=qty, side=OrderSide.BUY, time_in_force=TimeInForce.DAY,
                    stop_loss=StopLossRequest(stop_price=stop),
                    take_profit=TakeProfitRequest(limit_price=target)
                ))
                orb[sym]["traded"] = True
                print(f">>> {sym} LONG  E={entry} SL={stop} TP={target} qty={qty} <<<")

            elif bar["l"] < orb_l and vol_ok:
                entry  = round(orb_l - 0.01, 2)
                stop   = round(orb_h, 2)
                target = round(entry - (stop - entry) * RR, 2)
                qty    = max(1, math.floor(CAPITAL / entry))
                trade.submit_order(MarketOrderRequest(
                    symbol=sym, qty=qty, side=OrderSide.SELL, time_in_force=TimeInForce.DAY,
                    stop_loss=StopLossRequest(stop_price=stop),
                    take_profit=TakeProfitRequest(limit_price=target)
                ))
                orb[sym]["traded"] = True
                print(f">>> {sym} SHORT E={entry} SL={stop} TP={target} qty={qty} <<<")

            else:
                print(f"{sym}: no breakout | H={bar['h']:.2f} L={bar['l']:.2f} | ORB_H={orb_h:.2f} ORB_L={orb_l:.2f} vol={'OK' if vol_ok else 'LOW'}")

        time.sleep(30)

    except Exception as e:
        print(f"[ERROR] {e}")
        time.sleep(30)
