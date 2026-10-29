# Portfolio risk monitor

Monitor de riesgo para una cartera indexada de fondos y un plan de pensiones ante un posible pinchazo del ciclo de inversión en IA. El contexto es de inflación persistente y tipos del 4–5 %.

Descarga datos de mercado (Yahoo Finance) y macro/crédito (FRED) con `quantmod`. A partir de ellos clasifica regímenes macro, calcula un semáforo de alertas, hace stress tests por escenario e históricos y calcula los traspasos de rebalanceo.

> Estado a 29 oct 2026. Pipeline probado de principio a fin con datos reales (Yahoo y FRED) y con datos sintéticos (`Rscript main.R synthetic`). FRED solo sirve los 3 últimos años de los spreads ICE BofA: los regímenes usan el diferencial Baa-10a (`BAA10Y`) y la caché de `data/raw` acumula histórico entre ejecuciones.
>
> Los pesos de este README y de `cfg/portfolio.example.yaml` son **de ejemplo**, no una cartera real.

---

## 1. Resumen del análisis (oct 2026)

**Situación**

- La inversión en IA ha pasado de financiarse con caja a financiarse con deuda: 266.000 M USD en bonos ligados a la IA en 2026 (JPMorgan).
- El eslabón débil son los neoclouds: CoreWeave tiene un CDS a 5 años por encima de 1.000 pb.
- El crédito privado vive su primer test serio, con reembolsos limitados al 5 % en Blue Owl, Cliffwater y KKR.
- La inflación repunta (3,8 % en la zona euro y 3,7 % en EE. UU.) con Ormuz cerrado y el Brent en torno a 100 USD.
- Los bancos centrales están subiendo tipos: BCE al 2,50 % y Fed al 3,75–4,00 %, con el 10 años de EE. UU. en torno al 5,3 %.
- El CAPE del S&P 500 está en 41. La prima de riesgo frente al tipo real es prácticamente nula.

**Paralelos históricos:** telecos y fibra 1999–2001 (sobreinversión financiada con deuda y financiación de proveedores a clientes) más 1973–74 (shock petrolero con valoraciones concentradas). Diferencia clave con 2000 y 2008: con la inflación alta no hay margen para recortes rápidos de tipos.

**Escenarios a 12–24 meses** (juicio propio, no previsiones). Rentabilidad de la cartera de ejemplo sin el fondo de emergencia:

| Escenario | Prob. | Antes | Objetivo | Plan pensiones | Conjunto objetivo |
| --- | --- | --- | --- | --- | --- |
| A. Desinflado ordenado del ciclo IA | 40 % | −8,8 % | −5,6 % | +0,1 % | −4,2 % |
| B. Estanflación + evento de crédito | 25 % | −20,8 % | −14,5 % | −6,2 % | −12,5 % |
| C. Crash deflacionario | 20 % | −28,9 % | −20,9 % | +0,9 % | −15,5 % |
| D. Aterrizaje suave | 15 % | +8,9 % | +7,2 % | +2,2 % | +5,9 % |

**Lógica de la cartera de ejemplo**

- El monetario (Groupama Trésorerie) se divide en tres bolsillos: fondo de emergencia, reserva para la entrada de una vivienda y renta fija del ahorro general.
- El bolsillo de la entrada lleva un 45 % en bolsa que baja 10 puntos cada año. El ahorro general lleva un 80 % en bolsa.
- Traspasos en dos tramos: MSCI World pasa del 30 % al 22 % y Emergentes del 15 % al 8 %. Todo va al monetario.
- No se añaden fondos nuevos ni duración. Un plan de pensiones tipo cartera permanente (bolsa, oro, bonos largos y liquidez) ya cubre el escenario C. El hueco común de ambas carteras es el escenario B, y lo cubre mejor el monetario.

| Fondo | Antes | Objetivo |
| --- | --- | --- |
| Groupama Trésorerie IC | 35,00 % | 50,00 % |
| Fidelity MSCI World | 30,00 % | 22,00 % |
| Vanguard European Stock | 20,00 % | 20,00 % |
| Fidelity MSCI Emerging Markets | 15,00 % | 8,00 % |

---

## 2. Uso

```bash
cp cfg/portfolio.example.yaml cfg/portfolio.yaml   # tus pesos (git-ignored)
cp cfg/positions.example.yaml cfg/positions.yaml   # tus acciones y valores (git-ignored)
Rscript main.R              # datos reales (Yahoo + FRED)
Rscript main.R synthetic    # demo offline con datos simulados
```

Dependencias (en `DESCRIPTION`): `data.table`, `zoo`, `xts`, `quantmod`, `yaml`, `ggplot2`, `scales`, `httr2`, `curl` y `rugarch`; `testthat` para los tests.

Para obtener los traspasos en euros, rellena `values_eur` en `cfg/portfolio.yaml`. Si no, el cálculo se hace en puntos porcentuales.

**Posiciones seguidas** (`cfg/positions.yaml`): cada valor lleva su ticker de Yahoo, su divisa, la retención sobre dividendos y una lista de compras (`date`, `shares`, `price`, `fees`); una venta es una compra con títulos negativos. El monitor convierte todo a euros con los tipos de Yahoo, ajusta las compras por splits y calcula la plusvalía latente y realizada (coste medio) y los dividendos cobrados (títulos en cartera el día anterior a la fecha ex-dividendo, netos de retención).

### Informe en Telegram

`report.R` genera un resumen y lo envía a Telegram:

```bash
Rscript report.R daily --dry-run     # resumen diario, guardado en output/report_daily.html sin enviar
Rscript report.R weekly --dry-run    # informe semanal con gráficos
```

- **Resumen diario:** semáforo y cambios desde el último informe, cartera de fondos, posiciones y dividendos nuevos.
- **Informe semanal:** añade el régimen, los escenarios, el rebalanceo, el detalle de cada posición y los gráficos.

Sin credenciales, o con `--dry-run`, no se envía nada.

**Envío automático con GitHub Actions** (`.github/workflows/report.yml`): laborables a las 18:30 y domingos a las 10:00, hora de Madrid. Para activarlo:

1. Crea un bot con [@BotFather](https://t.me/BotFather) y guarda el token. Escríbele un mensaje al bot y obtén tu `chat_id` en `https://api.telegram.org/bot<TOKEN>/getUpdates`.
2. Guarda los secretos en el repo (con `gh` o en *Settings → Secrets and variables → Actions*):

   ```bash
   gh secret set TELEGRAM_BOT_TOKEN
   gh secret set TELEGRAM_CHAT_ID
   gh secret set PORTFOLIO_YAML_B64 --body "$(base64 -w0 cfg/portfolio.yaml)"
   gh secret set POSITIONS_YAML_B64 --body "$(base64 -w0 cfg/positions.yaml)"
   gh secret set CACHE_PASSPHRASE --body "$(openssl rand -base64 32)"
   ```

3. Lanza una prueba en *Actions → Telegram report → Run workflow*.

**Privacidad en un repo público:**
- Los datos personales solo entran como secretos y nunca se escriben en el log, que muestra las series de las posiciones como `private series`.
- No se suben artefactos.
- La caché de datos se guarda en dos partes: las series de mercado en claro y las de tus posiciones cifradas con `CACHE_PASSPHRASE`.
- Si cambias `cfg/portfolio.yaml` o `cfg/positions.yaml`, vuelve a guardar su secreto.

### Estructura

```
cfg/config.yaml              series, umbrales del semáforo, escenarios, ventanas históricas
cfg/portfolio.example.yaml   pesos actuales/objetivo, bolsillos, reglas, plan de pensiones
src/00_utils.R               utilidades (mensualización m_y, drawdown, percentiles)
src/01_download.R            quantmod: Yahoo + FRED con caché CSV en data/raw
src/02_synthetic.R           datos simulados con crisis 2000, 2008, 2020, 2022 y 2026
src/03_regimes.R             regímenes inflación × crédito, rendimiento y transiciones
src/04_stress.R              escenarios, rejilla por % de bolsa, stress histórico
src/05_signals.R             semáforo de alertas y lectura heurística de escenario
src/06_rebalance.R           traspasos al objetivo, senda de la entrada, router de aportaciones
src/07_plots.R               gráficos ggplot2
src/08_positions.R           posiciones con fecha de compra: precios, splits, divisas y dividendos
src/09_forecast.R            bootstrap por régimen, logit de alerta temprana, CAPM y GARCH
src/10_report.R              textos del informe de Telegram (HTML)
src/11_telegram.R            envío a Telegram con httr2 (o modo de prueba)
src/12_pipeline.R            run_pipeline(): el pipeline completo, usado por main.R y report.R
report.R                     informe diario o semanal: Rscript report.R daily|weekly [--dry-run]
main.R                       punto de entrada
```

### Salidas (`output/`)

| Archivo | Contenido |
| --- | --- |
| `signals.csv`, `signals.png` | semáforo: valor, cambio 1m/3m, percentil histórico, estado |
| `scenario_hint.csv` | escenario más coherente con las señales (heurístico) |
| `scenarios.csv`, `scenarios.png` | pérdidas por escenario: actual, objetivo, plan y conjunto |
| `equity_share_grid.csv` | pérdida por escenario según el % en bolsa (control de la tolerancia −15 %) |
| `historical_stress.csv` | comportamiento de cada proxy en 2000-02, 2007-09, 2020 y 2022 |
| `bootstrap.csv`, `bootstrap_fan.csv`, `bootstrap.png` | bootstrap por régimen: P(caída > 15 %) a 6, 12 y 24 meses y abanico de rentabilidad por cartera y bolsillo |
| `early_warning.csv`, `early_warning_coefs.csv`, `early_warning_series.csv` | logit de alerta temprana: probabilidad actual, coeficientes y validación fuera de muestra (AUC, Brier) |
| `capm.csv` | CAPM a 36 meses frente al MSCI World: alfa, beta, R² y volatilidad de fondos y posiciones |
| `garch.csv`, `garch_vol.csv`, `garch.png` | GJR-GARCH(1,1) con t de Student: volatilidad condicional, su percentil, VaR y ES a 1 y 10 días |
| `positions_summary.csv`, `positions_history.csv`, `dividends.csv`, `positions.png` | posiciones seguidas: valor, coste, plusvalía latente y realizada, dividendos, variación del día, del mes y del año |
| `portfolio_history.csv`, `portfolio.png` | evolución simulada de la cartera actual y objetivo (proxies, rebalanceo trimestral por defecto: `rebalance: frequency_months` en `cfg/portfolio.yaml`) y caída desde máximo |
| `regimes_monthly.csv`, `regimes.png` | régimen mensual inflación × crédito |
| `regime_performance.csv`, `regime_transitions.csv` | rentabilidad condicionada y matriz de transición |
| `rebalance_plan.csv`, `glide_path.csv` | traspasos por tramo y senda del bolsillo de entrada |

### Proxies (verificar)

| Fondo | Proxy diario |
| --- | --- |
| Fidelity MSCI World | `IWDA.AS` |
| Vanguard European Stock | `VEUR.AS` |
| Fidelity MSCI EM | `EMIM.AS` |
| Groupama Trésorerie | €STR (FRED `ECBESTRVOLWGTTRMDRT`) |

Los CDS de CoreWeave u Oracle no están en Yahoo ni en FRED. Se aproximan con spreads de crédito (`BAMLH0A0HYM2`, `BAMLC0A0CM`), precios de BDC (`BIZD`, `ARCC`) y la acción de `CRWV`.

---

## 3. Hoja de ruta de modelado

Ordenado de menor a mayor esfuerzo:

1. **Calibrar los escenarios con datos.** Sustituir las rentabilidades supuestas de `cfg/config.yaml` por la rentabilidad condicionada de cada proxy en `regime_performance.csv`.
2. ~~**Bootstrap por régimen.**~~ Hecho (`regime_bootstrap()`): bloques de 6 meses del régimen actual, 10.000 trayectorias a 24 meses, P(caída > 15 %) de cada cartera y bolsillo.
3. ~~**Early warning con logit.**~~ Hecho (`early_warning()`): P(caída > 15 % del S&P 500 en 6 meses) con el diferencial Baa, la pendiente 10a–2a, el tipo real, el VIX y el SOX, validado con ventana expansiva. Resultado a oct 2026: sin capacidad predictiva fuera de muestra (AUC 0,39, Brier peor que la frecuencia histórica); queda como referencia y como punto de partida para mejorar las variables.
4. **Regímenes latentes.** Cambiar los umbrales fijos por un modelo de Markov (`MSwM`, `depmixS4`) sobre rentabilidades, spreads e inflación, y comparar sus probabilidades con el semáforo.
5. **Ciclo de crédito con filtros macro.** Extraer el componente cíclico de los spreads y de la inflación con el filtro MBH (paquete `MacroFilters`) y usarlo como definición de régimen alternativa a la mediana móvil.
6. **Regresión cuantílica** (`quantreg`) de la rentabilidad a 12 meses sobre el CAPE, el tipo real y los spreads, para obtener percentiles de cola en lugar de medias.
7. ~~**Producto.**~~ Hecho en parte: informe diario y semanal por Telegram con GitHub Actions (`report.R`). Pendiente: informe Quarto o app Shiny.
8. **Mejorar el modelo de alerta temprana:** probit, variables de crédito privado (BDC) y de valoración, horizonte y umbral alternativos, y comparación con los regímenes de Markov.

## 4. Para el portfolio de proyectos

- Muestra un pipeline reproducible: configuración en YAML, código modular, caché de datos y modo sintético para que cualquiera lo ejecute sin claves ni datos personales.
- Publicar solo `cfg/portfolio.example.yaml` con pesos de ejemplo. `cfg/portfolio.yaml` y `data/` están en `.gitignore`.
- Los puntos 2 a 5 de la hoja de ruta son los que más valor añaden como caso de estudio: regímenes, probabilidad de cola y validación fuera de muestra. `examples/synthetic/report_weekly.html` muestra el informe semanal con datos simulados.

*No es asesoramiento financiero. Escenarios y probabilidades son supuestos del autor.*
