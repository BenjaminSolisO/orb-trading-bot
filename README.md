# orb-trading-bot

Live trading bot: Opening Range Breakout (ORB) en SPY, QQQ, IWM, GLD.
Paper trading con Alpaca Markets.

## Quick Start

```bash
pip install alpaca-py
set ALPACA_KEY=your_key
set ALPACA_SECRET=your_secret
set ALPACA_PAPER=true
python orb_live_bot.py
```

## Railway Deploy

1. Conecta este repo en Railway
2. Variables de entorno:
   - `ALPACA_KEY` — tu API Key de Alpaca
   - `ALPACA_SECRET` — tu Secret Key de Alpaca
   - `ALPACA_PAPER` — `true` para paper trading
   - `CAPITAL_PER` — capital por activo (default: 250)
   - `RR_RATIO` — risk:reward ratio (default: 2.5)
3. Start command: `python orb_live_bot.py`
4. Deploy

## Estrategia

- **ORB**: Opening Range Breakout, primeras 6 velas de 5min (09:30-10:00 ET)
- **Entrada**: Ruptura del high/low del ORB con filtro de volumen
- **Salida**: Stop en lado opuesto del ORB, Take Profit a 2.5:1 RR, o cierre EOD 15:50 ET
- **Máximo 1 trade por activo por día**

## Backtest (R scripts)

Correr en RStudio o Rscript:

```r
source("orb_backtest_spy_alpaca.R")   # ORB en SPY (~6 años IEX)
source("orb_opt_rr_ratio.R")          # Optimizacion RR ratio
source("portfolio_orb_4_assets.R")    # Portfolio 4 activos
```

Resultados (2020-2026, SPY/QQQ/IWM/GLD):
- Portfolio 4-asset: +$27,379 | Sharpe 5.41 | 5,480 trades
- Mejor individual: QQQ +$10,586 | Sharpe 4.34 | WR 60.5%
