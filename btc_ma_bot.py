import os, time
import pandas as pd
from alpaca.trading.client import TradingClient
from alpaca.trading.requests import MarketOrderRequest
from alpaca.trading.enums import OrderSide, TimeInForce
from alpaca.data.historical.crypto import CryptoHistoricalDataClient
from alpaca.data.requests import CryptoBarsRequest
from alpaca.data.timeframe import TimeFrame, TimeFrameUnit

API_KEY    = os.environ.get("ALPACA_KEY", "")
API_SECRET = os.environ.get("ALPACA_SECRET", "")

trade = TradingClient(API_KEY, API_SECRET, paper=True)
data  = CryptoHistoricalDataClient(API_KEY, API_SECRET)

SYMBOL = "BTC/USD"
QTY    = 0.001

def get_signal():
    req  = CryptoBarsRequest(symbol_or_symbols=SYMBOL, timeframe=TimeFrame(1, TimeFrameUnit.Minute), limit=25)
    bars = data.get_crypto_bars(req).data[SYMBOL]
    df   = pd.DataFrame([{"close": float(b.close)} for b in bars])
    df["sma_fast"] = df["close"].rolling(5).mean()
    df["sma_slow"] = df["close"].rolling(20).mean()
    if df["sma_fast"].iloc[-2] > df["sma_slow"].iloc[-2]:
        return "buy"
    elif df["sma_fast"].iloc[-2] < df["sma_slow"].iloc[-2]:
        return "sell"
    return "hold"

def has_position():
    try:
        return float(trade.get_open_position("BTCUSD").qty) > 0
    except:
        return False

while True:
    signal = get_signal()
    in_pos = has_position()
    print(f"Signal: {signal} | Position: {'LONG' if in_pos else 'FLAT'}")

    if signal == "buy" and not in_pos:
        trade.submit_order(MarketOrderRequest(symbol="BTCUSD", qty=QTY, side=OrderSide.BUY, time_in_force=TimeInForce.GTC))
        print("BUY")
    elif signal == "sell" and in_pos:
        trade.close_position("BTCUSD")
        print("SELL")

    time.sleep(60)
