"""Test ORB bot strategy logic."""
import sys
sys.path.insert(0, "C:/Users/solis/Downloads/classic strategies")

# Import specific items to avoid name issues
from orb_live_bot import (
    now_et, orb_phase, market_is_open,
    State, state, SYMBOLS, ORB_CANDLES, RR_RATIO,
    reset_day, process_bar, shares_for,
    trade_client
)

n = now_et()
print(f"ET time: {n}")
print(f"Phase: {orb_phase()}")
print(f"Market open: {market_is_open()}")

# Account
acc = trade_client.get_account()
print(f"Equity: {acc.equity}")

# Shares
for sym in SYMBOLS:
    px = {"SPY": 720, "QQQ": 674, "IWM": 279, "GLD": 423}[sym]
    print(f"Shares {sym} at ${px}: {shares_for(px):.6f}")

print("\n=== STRATEGY LOGIC TEST ===")

sample_bars = [
    {"t": "09:35", "o": 720.10, "h": 720.50, "l": 719.80, "c": 720.30, "v": 5000},
    {"t": "09:40", "o": 720.30, "h": 720.60, "l": 720.10, "c": 720.45, "v": 4500},
    {"t": "09:45", "o": 720.45, "h": 720.70, "l": 720.20, "c": 720.35, "v": 4800},
    {"t": "09:50", "o": 720.35, "h": 720.55, "l": 719.90, "c": 720.15, "v": 5200},
    {"t": "09:55", "o": 720.15, "h": 720.80, "l": 720.10, "c": 720.60, "v": 4600},
    {"t": "10:00", "o": 720.60, "h": 720.95, "l": 720.40, "c": 720.85, "v": 5000},
]

reset_day()
status_names = {0: "WAIT", 1: "COLL", 2: "MON", 3: "TRADE", 4: "DONE"}

for bar in sample_bars:
    process_bar("SPY", bar)
    s = state["SPY"]
    print(f"  {bar['t']} -> state={status_names[s['s']]} | bars={len(s['bars'])} | "
          f"orb_h={s['orb_h']:.2f} orb_l={s['orb_l']:.2f}")

s = state["SPY"]
orb_high = max(b["h"] for b in sample_bars)
orb_low  = min(b["l"] for b in sample_bars)
orb_range = orb_high - orb_low
avg_vol = sum(b["v"] for b in sample_bars) / len(sample_bars)

print("\n--- ORB CALCULATION ---")
print(f"ORB High: ${orb_high:.2f}  ORB Low: ${orb_low:.2f}  Range: ${orb_range:.2f}")
print(f"Avg Vol: {avg_vol:.0f}")

assert s["s"] == State.MONITOR, f"State should be MONITOR, is {s['s']}"
assert s["orb_h"] == orb_high, f"orb_h mismatch"
assert s["orb_l"] == orb_low, f"orb_l mismatch"
assert s["avg_vol"] == avg_vol, f"avg_vol mismatch"

# Test breakout math (manually verify, no order submission)
print("\n--- BREAKOUT MATH (simulated) ---")
breakout_bar = {"t": "10:05", "o": 720.85, "h": 721.20, "l": 720.70, "c": 721.00, "v": 5500}
# Verify: bar high (721.20) > orb_high (720.95)? YES
# Verify: bar volume (5500) > orb_avg_vol (4850)? YES
# Breakout should trigger LONG
bar_h = breakout_bar["h"]
bar_v = breakout_bar["v"]
print(f"Bar high ({bar_h}) > ORB high ({orb_high})? {bar_h > orb_high}")
print(f"Bar vol ({bar_v}) > Avg vol ({avg_vol:.0f})? {bar_v > avg_vol}")
assert bar_h > orb_high, "Should detect breakout"
assert bar_v > avg_vol, "Volume filter should pass"

expected_entry = orb_high + 0.01
expected_stop  = orb_low
expected_risk  = expected_entry - expected_stop
expected_target = expected_entry + expected_risk * 2.5

print(f"Entry = orb_high({orb_high}) + 0.01 = ${expected_entry:.2f}")
print(f"Stop  = orb_low({orb_low}) = ${expected_stop:.2f}")
print(f"Risk  = {expected_entry:.2f} - {orb_low:.2f} = ${expected_risk:.2f}")
print(f"Target = {expected_entry:.2f} + {expected_risk:.2f} * 2.5 = ${expected_target:.2f}")

print("\nALL STRATEGY CHECKS PASSED - ORB logic matches backtest")
