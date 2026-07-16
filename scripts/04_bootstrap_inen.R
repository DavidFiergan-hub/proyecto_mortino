library(boot)
library(dplyr)
library(ggplot2)

datos_mortino <- readRDS("data/datos_mortino.rds")
modelo <- readRDS("data/modelo_xgboost.rds")

# 1. Definir estadístico (media de calidad)
statistic <- function(data, indices) {
  mean(data[indices, ]$quality)
}

set.seed(123)
resultado_boot <- boot(data = datos_mortino, statistic = statistic, R = 1000)

# Calcular intervalos exactos de confianza (BCa)
ic_bca <- boot.ci(resultado_boot, type = "bca")
lim_inf <- ic_bca$bca[4]
lim_sup <- ic_bca$bca[5]

# 2. GENERAR GRÁFICO VISUAL DEL BOOTSTRAP
df_boot <- data.frame(calidad_media = resultado_boot$t)

p_boot <- ggplot(df_boot, aes(x = calidad_media)) +
  geom_density(fill = "steelblue", alpha = 0.4, color = "darkblue", linewidth = 1) +
  geom_vline(xintercept = resultado_boot$t0, color = "red", linetype = "solid", linewidth = 1.2) +
  geom_vline(xintercept = c(lim_inf, lim_sup), color = "darkgreen", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Distribución Bootstrap de la Calidad Media del Vino",
    subtitle = "Basado en 1,000 réplicas numéricas no paramétricas",
    x = "Calidad Media Estimada (Escala 0-10)",
    y = "Densidad de Probabilidad"
  ) +
  theme_minimal()

ggsave("results/graficos/bootstrap_densidad.png", p_boot, width = 8, height = 5)

# 3. EVALUACIÓN INEN 374
predictors <- c("fixed_acidity", "volatile_acidity", "citric_acid", 
                "residual_sugar", "chlorides", "free_sulfur_dioxide",
                "total_sulfur_dioxide", "density", "pH", "sulphates", "alcohol")

datos_mortino$prediccion <- predict(modelo, as.matrix(datos_mortino[, predictors]))

datos_evaluados <- datos_mortino %>%
  mutate(
    cumple_acidez = volatile_acidity <= 1.2,
    cumple_alcohol = alcohol >= 8.5 & alcohol <= 15.0,
    cumple_calidad = prediccion >= 6.0,
    estado_final = case_when(
      !cumple_acidez ~ "RECHAZADO: Exceso de Acidez Volatil (> 1.2 g/L)",
      !cumple_alcohol ~ "RECHAZADO: Alcohol fuera de rango (8.5 - 15.0%)",
      !cumple_calidad ~ "RECHAZADO: Calidad Predictiva Insuficiente (< 6/10)",
      TRUE ~ "APROBADO"
    ),
    recomendacion = case_when(
      estado_final == "APROBADO" ~ "Mantener parametros operativos actuales.",
      !cumple_acidez ~ "Controlar condiciones microbiologicas e higiene para mitigar acido acetico.",
      alcohol < 8.5 ~ "Evaluar chaptalizacion controlada de los mostos antes de fermentar.",
      TRUE ~ "Reevaluar balance de nutrientes y levaduras en la fermentacion."
    )
  )

tabla_final <- datos_evaluados %>%
  select(Lote = id_lote, Alcohol = alcohol, Acidez_Volatil = volatile_acidity, 
         Calidad_Pred = prediccion, Estado = estado_final, Recomendacion = recomendacion)

write.csv(tabla_final, "results/tablas/clasificacion_inen.csv", row.names = FALSE)

# 4. ANÁLISIS DE SENSIBILIDAD
alcohol_seq <- seq(8, 14, by = 0.2)
sens_alcohol <- sapply(alcohol_seq, function(a) {
  temp <- datos_mortino
  temp$alcohol <- a
  mean(predict(modelo, as.matrix(temp[, predictors])))
})

df_sens <- data.frame(alcohol = alcohol_seq, calidad = sens_alcohol)

p_sens <- ggplot(df_sens, aes(x = alcohol, y = calidad)) +
  geom_line(color = "darkred", linewidth = 1.2) +
  geom_point(color = "black") +
  labs(title = "Analisis de Sensibilidad: Calidad vs Graduacion Alcoholica",
       x = "Alcohol (% vol)", y = "Calidad Media Predicha") +
  theme_minimal()

ggsave("results/graficos/sensibilidad_alcohol.png", p_sens, width = 8, height = 5)
message("Bootstrap optimizado e informe INEN completados.")
