# proyecto_mortino

Código en R que reproduce los resultados, tablas y figuras del artículo
"Evaluación Estocástica de Conformidad Enológica bajo Escasez de Datos: Modelado
de Error por Martingalas y Límites de Azuma-Hoeffding con Transferencia XGBoost"
(Medina-Espinosa, Fierro-Barragán y Quiloango).

## Datos

- **Wine Quality Dataset (UCI)**: 6 497 vinos de uva con puntuación de calidad
  asignada por catadores (Cortez et al., 2009). Se carga desde el paquete `ibawds`.
- **Lotes de referencia de vino de mortiño (n = 8)**: conjunto *ilustrativo*
  definido por los autores en `scripts/01_cargar_datos.R`. No son mediciones de
  laboratorio y su puntuación de calidad no procede de un panel sensorial.

## Cómo reproducir

Desde la raíz del repositorio:

```bash
bash run_pipeline.sh
```

| Script | Qué hace |
|---|---|
| `01_cargar_datos.R` | Carga UCI, define los 8 lotes de referencia y los unifica |
| `02_modelo_xgboost.R` | Elige el número de rondas por validación cruzada solo sobre UCI y entrena el modelo |
| `03_simulacion_mc.R` | Simulación Monte Carlo por inversión normal truncada (30 000 réplicas) |
| `04_bootstrap_inen.R` | Bootstrap BCa, conformidad, sensibilidad, figuras y `results/valores_paper.tex` |

`results/valores_paper.tex` contiene las macros con los valores numéricos que
lee el manuscrito en LaTeX.

## Entorno

R 4.6.1 y xgboost 3.2.1.1, semilla 123. Paquetes: `xgboost`, `dplyr`, `tidyr`,
`ggplot2`, `scales`, `ragg`, `boot`, `mgcv`, `xtable`, `tidymodels`, `ibawds`.
Los valores pueden variar ligeramente con otras versiones de xgboost.
