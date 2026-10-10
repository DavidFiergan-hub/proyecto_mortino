# =============================================================================
# 03_simulacion_mc.R
# Simulacion Monte Carlo (inversion normal truncada, 11 variables)
# =============================================================================

library(dplyr)
library(xgboost)

# =============================================================================
# 1. CARGA Y SIMULACION
# =============================================================================

datos_mortino <- readRDS("data/datos_mortino.rds")
modelo <- readRDS("data/modelo_xgboost.rds")

predictors <- c(
  "fixed_acidity",
  "volatile_acidity",
  "citric_acid",
  "residual_sugar",
  "chlorides",
  "free_sulfur_dioxide",
  "total_sulfur_dioxide",
  "density",
  "pH",
  "sulphates",
  "alcohol"
)

params <- datos_mortino %>%
  summarise(across(
    all_of(predictors),
    list(
      mu = ~ mean(.x, na.rm = TRUE),
      sd = ~ {
        s <- sd(.x, na.rm = TRUE)
        if (is.na(s) || s == 0) mean(.x, na.rm = TRUE) * 0.05 else s
      }
    ),
    .names = "{.col}_{.fn}"
  ))

set.seed(123)
n_sim <- 30000

simulacion_raw <- as.data.frame(lapply(predictors, function(var) {
  qnorm(
    runif(n_sim),
    mean = params[[paste0(var, "_mu")]],
    sd = params[[paste0(var, "_sd")]]
  )
}))
colnames(simulacion_raw) <- predictors

simulacion_completa <- simulacion_raw %>%
  mutate(
    fixed_acidity = pmax(4.0, pmin(12.0, fixed_acidity)),
    volatile_acidity = pmax(0.1, pmin(1.5, volatile_acidity)),
    citric_acid = pmax(0.0, pmin(6.5, citric_acid)),
    pH = pmax(2.5, pmin(3.9, pH)),
    residual_sugar = pmax(15.0, pmin(150.0, residual_sugar)),
    chlorides = pmax(0.01, pmin(0.30, chlorides)),
    alcohol = pmax(7.0, pmin(14.0, alcohol)),
    free_sulfur_dioxide = pmax(5.0, pmin(60.0, free_sulfur_dioxide)),
    total_sulfur_dioxide = pmax(20.0, pmin(400.0, total_sulfur_dioxide)),
    density = pmax(0.990, pmin(1.060, density)),
    sulphates = pmax(0.10, pmin(0.80, sulphates))
  ) %>%
  mutate(
    total_sulfur_dioxide = pmax(total_sulfur_dioxide, free_sulfur_dioxide + 10)
  )

mat_sim <- as.matrix(simulacion_completa[, predictors])
simulacion_completa$prediccion <- predict(modelo, xgb.DMatrix(data = mat_sim))

# La figura de convergencia la genera 04_bootstrap_inen.R

write.csv(simulacion_completa, "data/simulacion_mortino.csv", row.names = FALSE)
message("Simulacion Monte Carlo guardada.")
message("Script 03 completado.")
