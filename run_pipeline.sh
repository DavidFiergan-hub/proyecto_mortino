#!/bin/bash
set -e
echo "Iniciando Pipeline de Simulacion..."
Rscript scripts/01_cargar_datos.R
Rscript scripts/02_modelo_xgboost.R
Rscript scripts/03_simulacion_mc.R
Rscript scripts/04_bootstrap_inen.R
echo "Pipeline terminado con exito. Tus resultados estan en results/"
