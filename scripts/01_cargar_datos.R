library(tidymodels)
library(ibawds)
library(dplyr)
library(tibble)
library(readr)

directorios <- c(
  "data",
  "scripts",
  "results",
  "results/graficos",
  "results/tablas"
)
lapply(directorios, dir.create, showWarnings = FALSE, recursive = TRUE)

message("Cargando Wine Quality Dataset (UCI)...")
data("wine_quality")

datos_uci <- wine_quality %>%
  mutate(
    origen = "uci",
    quality_cat = case_when(
      quality <= 4 ~ "bad",
      quality <= 6 ~ "good",
      quality >= 7 ~ "great"
    ),
    quality_cat = factor(quality_cat, levels = c("bad", "good", "great"))
  )

message("UCI: ", nrow(datos_uci), " muestras cargadas.")

message("Cargando datos reales de mortino...")
# PROCEDENCIA: los 8 lotes son un conjunto ILUSTRATIVO construido por los autores
# (valores plausibles orientados por la literatura y por la NTE INEN 374). No son
# mediciones de laboratorio de la planta, y `quality` es una puntuacion de
# referencia asignada por los autores, no una cata por panel sensorial.
# El lote 8 es un lote de control con parametros degradados.
datos_mortino <- tibble(
  id_lote = c(
    "L-INC-USFQ-01",
    "L-INC-USFQ-02",
    "L-PER-UTA-01",
    "L-PER-UTA-02",
    "L-MON-EPN-01",
    "L-MON-EPN-02",
    "L-ART-PUJ-01",
    "L-ERR-RECH-01"
  ),
  fixed_acidity = c(6.85, 7.10, 6.40, 6.25, 7.45, 7.20, 5.10, 8.90),
  volatile_acidity = c(0.25, 0.28, 0.31, 0.34, 0.22, 0.26, 0.85, 1.45),
  citric_acid = c(5.10, 5.40, 4.80, 4.50, 5.80, 5.55, 3.90, 6.20),
  residual_sugar = c(48.5, 52.0, 65.0, 70.2, 39.4, 42.1, 95.0, 120.0),
  chlorides = c(0.09, 0.11, 0.12, 0.14, 0.08, 0.09, 0.18, 0.25),
  free_sulfur_dioxide = c(24.0, 28.0, 18.0, 15.0, 31.0, 26.0, 38.0, 48.0),
  total_sulfur_dioxide = c(80.0, 85.0, 95.0, 90.0, 75.0, 78.0, 120.0, 210.0),
  density = c(1.018, 1.022, 1.026, 1.031, 1.014, 1.016, 1.042, 1.055),
  pH = c(3.15, 3.10, 3.22, 3.25, 2.98, 3.02, 3.45, 2.65),
  sulphates = c(0.22, 0.26, 0.35, 0.38, 0.20, 0.24, 0.45, 0.65),
  alcohol = c(11.5, 11.8, 10.8, 10.5, 12.0, 12.2, 9.2, 7.5),
  quality = c(8, 8, 7, 6, 9, 8, 6, 3),
  origen = "mortino"
)

message("Mortino: ", nrow(datos_mortino), " lotes cargados.")

message("Unificando datasets...")
datos_completos <- bind_rows(datos_uci, datos_mortino)

write.csv(datos_mortino, "data/datos_mortino.csv", row.names = FALSE)
write.csv(datos_uci, "data/datos_uci.csv", row.names = FALSE)
write.csv(datos_completos, "data/dataset_completo.csv", row.names = FALSE)

saveRDS(datos_mortino, "data/datos_mortino.rds")
saveRDS(datos_uci, "data/datos_uci.rds")
saveRDS(datos_completos, "data/dataset_completo.rds")

message("\nRESUMEN ESTADISTICO - MORTINO")
resumen_mortino <- datos_mortino %>%
  summarise(across(
    c(pH, alcohol, volatile_acidity, quality),
    list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE)),
    .names = "{.col}_{.fn}"
  ))
print(resumen_mortino)
