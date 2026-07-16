library(dplyr)
library(ggplot2)

datos_mortino <- readRDS("data/datos_mortino.rds")
modelo <- readRDS("data/modelo_xgboost.rds")

params <- datos_mortino %>%
  summarise(across(
    c(pH, alcohol, volatile_acidity, residual_sugar, fixed_acidity),
    list(mu = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE)),
    .names = "{.col}_{.fn}"
  ))

set.seed(123)
n_sim <- 10000

simulacion <- data.frame(
  pH = qnorm(runif(n_sim), params$pH_mu, params$pH_sd),
  alcohol = qnorm(runif(n_sim), params$alcohol_mu, params$alcohol_sd),
  volatile_acidity = qnorm(runif(n_sim), params$volatile_acidity_mu, params$volatile_acidity_sd),
  residual_sugar = qnorm(runif(n_sim), params$residual_sugar_mu, params$residual_sugar_sd),
  fixed_acidity = qnorm(runif(n_sim), params$fixed_acidity_mu, params$fixed_acidity_sd)
) %>%
  mutate(
    pH = pmax(2.5, pmin(3.8, pH)),
    alcohol = pmax(7, pmin(13.5, alcohol)),
    volatile_acidity = pmax(0.1, pmin(1.5, volatile_acidity)),
    residual_sugar = pmax(20, pmin(140, residual_sugar)),
    fixed_acidity = pmax(4, pmin(10, fixed_acidity))
  )

simulacion_completa <- simulacion %>%
  mutate(
    citric_acid = 5.10,
    chlorides = 0.12,
    free_sulfur_dioxide = 26.0,
    total_sulfur_dioxide = 104.0,
    density = 1.028,
    sulphates = 0.32
  )

predictors <- c("fixed_acidity", "volatile_acidity", "citric_acid", 
                "residual_sugar", "chlorides", "free_sulfur_dioxide",
                "total_sulfur_dioxide", "density", "pH", "sulphates", "alcohol")

simulacion_completa$prediccion <- predict(modelo, as.matrix(simulacion_completa[, predictors]))

df_conv <- data.frame(
  iteracion = 1:n_sim,
  media = cumsum(simulacion$alcohol) / 1:n_sim
)

p_conv <- ggplot(df_conv, aes(x = iteracion, y = media)) +
  geom_line(color = "steelblue") +
  geom_hline(yintercept = mean(simulacion$alcohol), color = "red", linetype = "dashed") +
  labs(title = "Convergencia de la Media de Alcohol (Monte Carlo)", x = "Simulaciones", y = "Media Acumulada") +
  theme_minimal()

ggsave("results/graficos/convergencia_alcohol.png", p_conv, width = 8, height = 5)
write.csv(simulacion_completa, "data/simulacion_mortino.csv", row.names = FALSE)
message("Simulacion Monte Carlo completada.")
