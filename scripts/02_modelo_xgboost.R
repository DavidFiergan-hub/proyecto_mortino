library(xgboost)
library(caret)
library(dplyr)
library(ggplot2)

datos <- readRDS("data/dataset_completo.rds")
train_uci <- datos %>% filter(origen == "uci")
test_mortino <- datos %>% filter(origen == "mortino")

predictors <- c("fixed_acidity", "volatile_acidity", "citric_acid", 
                "residual_sugar", "chlorides", "free_sulfur_dioxide",
                "total_sulfur_dioxide", "density", "pH", "sulphates", "alcohol")

train_mat <- as.matrix(train_uci[, predictors])
test_mat <- as.matrix(test_mortino[, predictors])

train_matrix <- xgb.DMatrix(data = train_mat, label = train_uci$quality)
test_matrix <- xgb.DMatrix(data = test_mat, label = test_mortino$quality)

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
modelo <- xgb.train(
  params = params,
  data = train_matrix,
  nrounds = 500,
  early_stopping_rounds = 50,
  watchlist = list(train = train_matrix, test = test_matrix),
  verbose = 0
)

importancia <- xgb.importance(model = modelo, feature_names = predictors)
write.csv(importancia, "results/tablas/importancia_variables.csv", row.names = FALSE)

p <- ggplot(importancia[1:5, ], aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Importancia de Variables - XGBoost", x = "Variable", y = "Ganancia") +
  theme_minimal()

ggsave("results/graficos/importancia_variables.png", p, width = 8, height = 5)
saveRDS(modelo, "data/modelo_xgboost.rds")
message("Modelo XGBoost entrenado y guardado.")
