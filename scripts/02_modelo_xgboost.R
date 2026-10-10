# =============================================================================
# 02_modelo_xgboost.R
# Entrenamiento XGBoost sin fuga de datos:
# - N_rounds se elige por validacion cruzada de 5 pliegues SOLO sobre UCI
# - los lotes de mortino no intervienen en el ajuste; se evaluan a ciegas
# =============================================================================

library(xgboost)
library(dplyr)

datos <- readRDS("data/dataset_completo.rds")
train_uci <- datos %>% filter(origen == "uci")
test_mortino <- datos %>% filter(origen == "mortino")

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

train_matrix <- xgb.DMatrix(
  as.matrix(train_uci[, predictors]),
  label = train_uci$quality
)
test_matrix <- xgb.DMatrix(as.matrix(test_mortino[, predictors]))

params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  max_depth = 6,
  eta = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  nthread = 4
)

set.seed(123)
cv <- xgb.cv(
  params = params,
  data = train_matrix,
  nrounds = 1000,
  nfold = 5,
  early_stopping_rounds = 50,
  verbose = 0
)
n_rounds <- which.min(cv$evaluation_log$test_rmse_mean)
message("N_rounds óptimo (CV 5 pliegues): ", n_rounds)
rmse_cv <- min(cv$evaluation_log$test_rmse_mean)
message("RMSE de validación cruzada (UCI): ", round(rmse_cv, 3))
dir.create("results/tablas", showWarnings = FALSE, recursive = TRUE)
write.csv(
  data.frame(n_rounds = n_rounds, rmse_cv = rmse_cv),
  "results/tablas/validacion_cruzada.csv",
  row.names = FALSE
)

set.seed(123)
modelo <- xgb.train(
  params = params,
  data = train_matrix,
  nrounds = n_rounds,
  verbose = 0
)

pred <- predict(modelo, test_matrix)
message(
  "RMSE de transferencia (mortiño): ",
  round(sqrt(mean((test_mortino$quality - pred)^2)), 3)
)

dir.create("results/tablas", showWarnings = FALSE, recursive = TRUE)
importancia <- xgb.importance(model = modelo, feature_names = predictors)
write.csv(
  importancia,
  "results/tablas/importancia_variables.csv",
  row.names = FALSE
)

saveRDS(modelo, "data/modelo_xgboost.rds")
message("Modelo XGBoost entrenado y guardado.")
message("Script 02 completado.")
