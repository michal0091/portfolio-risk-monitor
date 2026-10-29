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

Dependencias: `data.table`, `zoo`, `xts`, `quantmod`, `yaml`, `ggplot2`, `scales`.

Para obtener los traspasos en euros, rellena `values_eur` en `cfg/portfolio.yaml`. Si no, el cálculo se hace en puntos porcentuales.

**Posiciones seguidas** (`cfg/positions.yaml`): cada valor lleva su ticker de Yahoo, su divisa, la retención sobre dividendos y una lista de compras (`date`, `shares`, `price`, `fees`); una venta es una compra con títulos negativos. El monitor convierte todo a euros con los tipos de Yahoo, ajusta las compras por splits y calcula la plusvalía latente y realizada (coste medio) y los dividendos cobrados (títulos en cartera el día anterior a la fecha ex-dividendo, netos de retención).

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
2. **Bootstrap por régimen** (`boot`). Simular trayectorias de 24 meses remuestreando bloques del régimen actual para estimar P(caída > 15 %) de la cartera objetivo y de cada bolsillo.
3. **Early warning con logit o probit.** P(drawdown > 15 % en los próximos 6 meses) en función de los spreads HY/IG, la pendiente 10a–2a, el tipo real, la caída del SOX y el precio de los BDC. Validar fuera de muestra (AUC, Brier) con ventana expansiva.
4. **Regímenes latentes.** Cambiar los umbrales fijos por un modelo de Markov (`MSwM`, `depmixS4`) sobre rentabilidades, spreads e inflación, y comparar sus probabilidades con el semáforo.
5. **Ciclo de crédito con filtros macro.** Extraer el componente cíclico de los spreads y de la inflación con el filtro MBH (paquete `MacroFilters`) y usarlo como definición de régimen alternativa a la mediana móvil.
6. **Regresión cuantílica** (`quantreg`) de la rentabilidad a 12 meses sobre el CAPE, el tipo real y los spreads, para obtener percentiles de cola en lugar de medias.
7. **Producto.** Informe Quarto mensual o app Shiny, con ejecución programada (GitHub Actions o cron) que envíe el semáforo por correo.

## 4. Para el portfolio de proyectos

- Muestra un pipeline reproducible: configuración en YAML, código modular, caché de datos y modo sintético para que cualquiera lo ejecute sin claves ni datos personales.
- Publicar solo `cfg/portfolio.example.yaml` con pesos de ejemplo. `cfg/portfolio.yaml` y `data/` están en `.gitignore`.
- Los puntos 2 a 5 de la hoja de ruta son los que más valor añaden como caso de estudio: regímenes, probabilidad de cola y validación fuera de muestra.

*No es asesoramiento financiero. Escenarios y probabilidades son supuestos del autor.*
