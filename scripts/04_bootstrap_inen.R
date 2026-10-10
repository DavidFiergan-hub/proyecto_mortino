# =============================================================================
# 04_bootstrap_inen.R
# Script maestro: figuras (ES/EN), tablas (ES/EN) y valores para el .tex
# - Revista Politecnica (EPN): versiones en espanol (coma decimal)
# - Versiones en ingles (punto decimal)
# - results/valores_paper.tex: macros \Val... que lee mortino_poli.tex
# =============================================================================

library(dplyr)
library(ggplot2)
library(xgboost)
library(boot)
library(scales)
library(ragg)
library(tidyr)
library(xtable)
library(mgcv)

# =============================================================================
# 0. PALETA, FORMATO NUMERICO Y TEMA EDITORIAL
# =============================================================================

paleta <- c(
  azul = "#0072B2",
  naranja = "#D55E00",
  verde = "#009E73",
  rojo = "#CC79A7",
  gris = "#999999"
)

# Formato numerico por idioma: ES usa coma decimal y espacio de miles
marca_dec <- function(idioma) if (idioma == "es") "," else "."
marca_mil <- function(idioma) if (idioma == "es") " " else ","
fmt <- function(x, d, idioma) {
  formatC(x, format = "f", digits = d, decimal.mark = marca_dec(idioma))
}
fmt_mil <- function(x, idioma) {
  formatC(x, format = "d", big.mark = marca_mil(idioma))
}
# Porcentaje: "28,9 %" en ES y "28.9%" en EN
fmt_pct <- function(x, d, idioma) {
  paste0(fmt(x, d, idioma), if (idioma == "es") " %" else "%")
}

# Las figuras se generan a su tamano final (8,4 o 17,4 cm de ancho),
# de modo que los tamanos en pt son reales en el PDF.
tema_paper <- function(base_size = 8) {
  theme_bw(base_size = base_size) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", linewidth = 0.5),
      axis.ticks = element_line(color = "black", linewidth = 0.3),
      axis.text = element_text(size = 7, color = "black"),
      axis.title = element_text(
        size = base_size,
        face = "bold",
        color = "black"
      ),
      legend.position = "none",
      plot.margin = margin(4, 6, 4, 4)
    )
}

guardar_figura <- function(plot, nombre, ancho_cm, alto_cm) {
  dir.create("results/graficos", showWarnings = FALSE, recursive = TRUE)
  ggsave(
    paste0("results/graficos/", nombre, ".pdf"),
    plot,
    device = cairo_pdf,
    width = ancho_cm,
    height = alto_cm,
    units = "cm",
    bg = "white"
  )
  ggsave(
    paste0("results/graficos/", nombre, ".png"),
    plot,
    device = ragg::agg_png,
    width = ancho_cm,
    height = alto_cm,
    units = "cm",
    dpi = 1200, # ggsave ignora `res`; la resolucion se fija con `dpi`
    bg = "white"
  )
  message("Guardado: ", nombre)
}

# =============================================================================
# 1. DICCIONARIO BILINGUE
# =============================================================================

etiq <- list(
  es = list(
    gain = "Ganancia relativa (%)",
    convergencia = "Media acumulada (% vol)",
    generaciones = "Número de generaciones (escala logarítmica)",
    asintota = "Asíntota",
    error_max = "Error máx.",
    bootstrap_x = "Media de la puntuación de calidad de referencia\n(escala 0-10)",
    bootstrap_y = "Densidad de probabilidad empírica",
    media = "Media",
    ic = "IC 95 % BCa",
    sens_x = "Grado alcohólico (% vol)",
    sens_y = "Calidad sensorial predicha",
    umbral = "Umbral comercial",
    optimo = "PUNTO ÓPTIMO",
    lotes = "Lote  Alc(%)  Calidad",
    cor = "Coeficiente de\ncorrelación (r)",
    vars = c(
      fixed_acidity = "Acidez fija",
      volatile_acidity = "Acidez volátil",
      citric_acid = "Ácido cítrico",
      residual_sugar = "Azúcar residual",
      chlorides = "Cloruros",
      free_sulfur_dioxide = "SO2 libre",
      total_sulfur_dioxide = "SO2 total",
      density = "Densidad",
      pH = "pH",
      sulphates = "Sulfatos",
      alcohol = "Alcohol"
    )
  ),
  en = list(
    gain = "Relative gain (%)",
    convergencia = "Cumulative mean (% vol)",
    generaciones = "Number of generations (logarithmic scale)",
    asintota = "Asymptote",
    error_max = "Max. error",
    bootstrap_x = "Mean reference quality score\n(0-10 scale)",
    bootstrap_y = "Empirical probability density",
    media = "Mean",
    ic = "95% BCa CI",
    sens_x = "Alcoholic strength (% vol)",
    sens_y = "Predicted sensory quality",
    umbral = "Commercial threshold",
    optimo = "OPTIMAL POINT",
    lotes = "Batch Alc(%)  Quality",
    cor = "Correlation\ncoefficient (r)",
    vars = c(
      fixed_acidity = "Fixed acidity",
      volatile_acidity = "Volatile acidity",
      citric_acid = "Citric acid",
      residual_sugar = "Residual sugar",
      chlorides = "Chlorides",
      free_sulfur_dioxide = "Free SO2",
      total_sulfur_dioxide = "Total SO2",
      density = "Density",
      pH = "pH",
      sulphates = "Sulphates",
      alcohol = "Alcohol"
    )
  )
)

# =============================================================================
# 2. CARGA DE DATOS Y MODELO
# =============================================================================

# Mismo modelo que entrena 02_modelo_xgboost.R (un unico modelo en todo el paper)
modelo <- readRDS("data/modelo_xgboost.rds")
datos_mortino <- readRDS("data/datos_mortino.rds")

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

mat_mortino <- as.matrix(datos_mortino[, predictors])
datos_mortino$prediccion <- predict(modelo, xgb.DMatrix(data = mat_mortino))
datos_mortino$num_lote <- 1:nrow(datos_mortino)

# Columna con la puntuacion de calidad de referencia (asignada por los autores,
# no por un panel sensorial; ver 01_cargar_datos.R)
col_cal_obs <- if ("calidad" %in% colnames(datos_mortino)) {
  "calidad"
} else {
  "quality"
}

message(
  "Predicciones de los lotes (deben coincidir con la Tabla 2): ",
  paste(sprintf("%.2f", datos_mortino$prediccion), collapse = "; ")
)

# Region de conformidad y umbral comercial
NORMA <- list(alc_min = 6.0, av_max = 1.5, so2t_max = 400) # = Ec. (1); VERIFICAR NTE INEN 374
UMBRAL_Q <- 6.0
datos_mortino <- datos_mortino %>%
  mutate(
    conforme_inen = alcohol >= NORMA$alc_min &
      volatile_acidity <= NORMA$av_max &
      total_sulfur_dioxide <= NORMA$so2t_max
  )
rmse_transfer <- sqrt(mean(
  (datos_mortino[[col_cal_obs]] - datos_mortino$prediccion)^2
))
message("RMSE de transferencia: ", round(rmse_transfer, 3))

dir.create("results/graficos", showWarnings = FALSE, recursive = TRUE)
dir.create("results/tablas", showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# 3. COMPUTOS ESTOCASTICOS COMPARTIDOS
# =============================================================================

# A. Importancia de Variables
importancia_raw <- xgb.importance(model = modelo, feature_names = predictors)

# B. Simulacion Monte Carlo (N = 30,000)
set.seed(123)
n_sim <- 30000

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

sim_raw <- as.data.frame(lapply(predictors, function(var) {
  qnorm(
    runif(n_sim),
    params[[paste0(var, "_mu")]],
    params[[paste0(var, "_sd")]]
  )
}))
colnames(sim_raw) <- predictors

# Limites fisicos de simulacion (no son los limites de la norma)
ALC_TRUNC <- c(7.0, 14.0)
sim <- sim_raw %>%
  mutate(
    fixed_acidity = pmax(4.0, pmin(12.0, fixed_acidity)),
    volatile_acidity = pmax(0.1, pmin(1.5, volatile_acidity)),
    citric_acid = pmax(0.0, pmin(6.5, citric_acid)),
    pH = pmax(2.5, pmin(3.9, pH)),
    residual_sugar = pmax(15.0, pmin(150.0, residual_sugar)),
    chlorides = pmax(0.01, pmin(0.30, chlorides)),
    alcohol = pmax(ALC_TRUNC[1], pmin(ALC_TRUNC[2], alcohol)),
    free_sulfur_dioxide = pmax(5.0, pmin(60.0, free_sulfur_dioxide)),
    total_sulfur_dioxide = pmax(20.0, pmin(400.0, total_sulfur_dioxide)),
    density = pmax(0.990, pmin(1.060, density)),
    sulphates = pmax(0.10, pmin(0.80, sulphates))
  ) %>%
  mutate(
    total_sulfur_dioxide = pmax(total_sulfur_dioxide, free_sulfur_dioxide + 10)
  )

sim$prediccion <- predict(
  modelo,
  xgb.DMatrix(data = as.matrix(sim[, predictors]))
)
write.csv(sim, "data/simulacion_mortino.csv", row.names = FALSE)

# Convergencia estocastica centrada en el grado alcoholico (% vol)
mean_alcohol <- mean(sim$alcohol)
n_convergencia <- 26492
ALFA <- 0.01
amp_alc <- diff(ALC_TRUNC)
# Envolvente de Hoeffding al 99 % para una variable acotada en ALC_TRUNC
eps_hoeffding <- function(k) amp_alc * sqrt(log(2 / ALFA) / (2 * k))
eps_final <- eps_hoeffding(n_sim)
df_conv <- data.frame(
  iteracion = 1:n_sim,
  media = cumsum(sim$alcohol) / (1:n_sim)
) %>%
  mutate(
    eps = eps_hoeffding(iteracion),
    banda_inf = mean_alcohol - eps,
    banda_sup = mean_alcohol + eps
  )

# C. Bootstrap No Parametrico (B = 1,000) sobre la Calidad Observada
statistic_boot <- function(data, indices) mean(data[[col_cal_obs]][indices])
set.seed(123)
res_boot <- boot(datos_mortino, statistic_boot, R = 1000)

# Salvaguarda: si el BCa falla se recurre al intervalo percentil
ci <- tryCatch(
  boot.ci(res_boot, type = "bca")$bca[4:5],
  error = function(e) boot.ci(res_boot, type = "perc")$percent[4:5]
)
lim_inf <- ci[1]
lim_sup <- ci[2]
media_t0 <- res_boot$t0
df_boot <- data.frame(calidad_media = as.vector(res_boot$t))

# D. Sensibilidad Univariada al Alcohol
vars_sin_alcohol <- setdiff(predictors, "alcohol")
mediana_base <- sapply(vars_sin_alcohol, function(p) {
  median(datos_mortino[[p]], na.rm = TRUE)
})

alcohol_seq <- seq(ALC_TRUNC[1], ALC_TRUNC[2], by = 0.05)
df_sens <- data.frame(alcohol = alcohol_seq)
for (p in vars_sin_alcohol) {
  df_sens[[p]] <- rep(mediana_base[p], length(alcohol_seq))
}
df_sens <- df_sens[, predictors]
df_sens$calidad_pred <- predict(
  modelo,
  xgb.DMatrix(data = as.matrix(df_sens[, predictors]))
)

# Curva suavizada (GAM); el optimo se toma de la curva suavizada
gam_sens <- mgcv::gam(calidad_pred ~ s(alcohol, bs = "cs", k = 10), data = df_sens)
df_sens$calidad_suav <- as.numeric(predict(gam_sens, newdata = df_sens))
optimo_alc <- df_sens$alcohol[which.max(df_sens$calidad_suav)]
optimo_cal <- max(df_sens$calidad_suav)
# Tramo en que la curva suavizada supera el umbral comercial
sobre_umbral <- df_sens$alcohol[df_sens$calidad_suav >= UMBRAL_Q]
message(
  "Óptimo (curva suavizada): ", optimo_alc, " % vol; Q suavizada >= ", UMBRAL_Q,
  " en: ",
  if (length(sobre_umbral)) paste(range(sobre_umbral), collapse = " - ") else "ningún tramo"
)

# =============================================================================
# 4. GENERACION DE FIGURAS BILINGUES (TAMANO FINAL, TEXTO A 7-8 pt)
# =============================================================================

generar_graficos <- function(idioma) {
  L <- etiq[[idioma]]
  sufijo <- if (idioma == "es") "_es" else "_en"
  dec <- marca_dec(idioma)

  # ---------------------------------------------------------------------------
  # IMPORTANCIA DE VARIABLES (una columna: 8,4 x 6,5 cm)
  # ---------------------------------------------------------------------------
  imp <- importancia_raw %>%
    mutate(
      Variable = factor(L$vars[Feature], levels = L$vars[Feature][order(Gain)]),
      Gain_pct = Gain / sum(Gain) * 100
    )

  p1 <- ggplot(imp, aes(x = Variable, y = Gain_pct)) +
    geom_col(fill = paleta["azul"], color = "black", linewidth = 0.25) +
    geom_text(
      aes(label = fmt_pct(Gain_pct, 1, idioma)),
      hjust = -0.12,
      size = 2.4
    ) +
    coord_flip(clip = "off") +
    scale_y_continuous(
      labels = label_number(accuracy = 1, decimal.mark = dec),
      expand = expansion(mult = c(0, 0.20))
    ) +
    labs(x = NULL, y = L$gain) +
    tema_paper(base_size = 8)
  guardar_figura(p1, paste0("fig_importancia", sufijo), 8.4, 6.5)

  # ---------------------------------------------------------------------------
  # CONVERGENCIA MONTE CARLO (dos columnas: 17,4 x 8,5 cm)
  # ---------------------------------------------------------------------------
  y_lim_conv <- c(min(df_conv$media) - 0.15, max(df_conv$media) + 0.05)
  cortes_x <- c(1, 10, 100, 1000, 10000, 30000)
  txt_box_conv <- paste0(
    L$asintota, " = ", fmt_pct(mean_alcohol, 2, idioma), " vol\n",
    "N = ", fmt_mil(n_convergencia, idioma), "\n",
    L$error_max, " (", fmt_pct(99, 0, idioma), ", N = ", fmt_mil(n_sim, idioma),
    ") = ", fmt_pct(eps_final, 3, idioma), " vol"
  )

  p2 <- ggplot(df_conv, aes(x = iteracion)) +
    geom_ribbon(
      aes(ymin = banda_inf, ymax = banda_sup),
      fill = "gray80",
      alpha = 0.6
    ) +
    geom_line(aes(y = media), color = paleta["azul"], linewidth = 0.45) +
    geom_hline(
      yintercept = mean_alcohol,
      color = paleta["rojo"],
      linetype = "dashed",
      linewidth = 0.5
    ) +
    geom_vline(
      xintercept = n_convergencia,
      color = "black",
      linetype = "dotted",
      linewidth = 0.5
    ) +
    scale_x_log10(breaks = cortes_x, labels = fmt_mil(cortes_x, idioma)) +
    scale_y_continuous(labels = label_number(accuracy = 0.1, decimal.mark = dec)) +
    # Recuadro en la esquina inferior derecha
    annotate(
      "label",
      x = n_sim,
      y = y_lim_conv[1] + 0.02,
      label = txt_box_conv,
      hjust = 1,
      vjust = 0,
      size = 2.4,
      family = "sans",
      fill = alpha("white", 0.95),
      color = "black",
      label.padding = unit(0.30, "lines"),
      linewidth = 0.3
    ) +
    coord_cartesian(ylim = y_lim_conv) +
    labs(x = L$generaciones, y = L$convergencia) +
    tema_paper(base_size = 8)
  guardar_figura(p2, paste0("fig_convergencia", sufijo), 17.4, 8.5)

  # ---------------------------------------------------------------------------
  # DENSIDAD BOOTSTRAP (una columna: 8,4 x 6,5 cm)
  # ---------------------------------------------------------------------------
  x_min <- min(df_boot$calidad_media) - 0.25
  x_max <- max(df_boot$calidad_media) + 0.25
  dens_max <- max(density(df_boot$calidad_media, adjust = 0.9)$y)
  # Techo vertical ampliado para que el recuadro no tape la densidad
  y_max_plot <- dens_max * 1.65

  leyenda_boot <- paste0(
    L$media, " = ", fmt(media_t0, 3, idioma), "\n",
    L$ic, ": [", fmt(lim_inf, 3, idioma), "; ", fmt(lim_sup, 3, idioma), "]"
  )

  p3 <- ggplot(df_boot, aes(x = calidad_media)) +
    geom_histogram(
      aes(y = after_stat(density)),
      bins = 25,
      fill = paleta["azul"],
      color = "white",
      linewidth = 0.2,
      alpha = 0.75
    ) +
    geom_density(color = "black", linewidth = 0.5, adjust = 0.9) +
    geom_vline(xintercept = media_t0, color = paleta["rojo"], linewidth = 0.6) +
    geom_vline(
      xintercept = c(lim_inf, lim_sup),
      color = paleta["verde"],
      linetype = "dashed",
      linewidth = 0.5
    ) +
    annotate(
      "label",
      x = x_max,
      y = y_max_plot,
      label = leyenda_boot,
      hjust = 1.0,
      vjust = 1.0,
      size = 2.3,
      family = "sans",
      fill = alpha("white", 0.95),
      color = "black",
      label.padding = unit(0.30, "lines"),
      linewidth = 0.3
    ) +
    scale_x_continuous(labels = label_number(accuracy = 1, decimal.mark = dec)) +
    scale_y_continuous(labels = label_number(accuracy = 0.01, decimal.mark = dec)) +
    coord_cartesian(xlim = c(x_min, x_max), ylim = c(0, y_max_plot)) +
    labs(x = L$bootstrap_x, y = L$bootstrap_y) +
    tema_paper(base_size = 8)
  guardar_figura(p3, paste0("fig_bootstrap", sufijo), 8.4, 6.5)

  # ---------------------------------------------------------------------------
  # SENSIBILIDAD AL ALCOHOL (dos columnas: 17,4 x 8,5 cm)
  # Respuesta cruda del modelo en gris fino y curva suavizada GAM en negro.
  # Sin leyenda: el pie de figura la explica.
  # ---------------------------------------------------------------------------
  txt_umbral <- paste0(L$umbral, " (Q = ", fmt(UMBRAL_Q, 1, idioma), ")")
  txt_opt_box <- paste0(
    L$optimo, "\nAlcohol: ", fmt_pct(optimo_alc, 1, idioma), " vol"
  )

  # Resumen monoespaciado de los 8 lotes
  filas_lotes <- sprintf(
    "(%d)   %5s%%    %4s",
    datos_mortino$num_lote,
    fmt(datos_mortino$alcohol, 1, idioma),
    fmt(datos_mortino$prediccion, 2, idioma)
  )
  tabla_sens_txt <- paste(
    c(L$lotes, strrep("-", nchar(L$lotes)), filas_lotes),
    collapse = "\n"
  )

  valores_y <- c(df_sens$calidad_pred, df_sens$calidad_suav, datos_mortino$prediccion)
  y_min_s <- min(valores_y, na.rm = TRUE) - 0.35
  y_max_s <- max(valores_y, na.rm = TRUE) + 0.45

  p4 <- ggplot() +
    geom_hline(
      yintercept = UMBRAL_Q,
      linetype = "dashed",
      color = paleta["gris"],
      linewidth = 0.4
    ) +
    annotate(
      "text",
      x = 7.15,
      y = UMBRAL_Q + 0.06,
      label = txt_umbral,
      hjust = 0,
      size = 2.4,
      color = "gray30",
      fontface = "italic"
    ) +
    # Respuesta cruda del modelo (escalonada por ser un ensamble de arboles)
    geom_line(
      data = df_sens,
      aes(x = alcohol, y = calidad_pred),
      color = "gray60",
      linewidth = 0.3
    ) +
    # Curva suavizada GAM
    geom_line(
      data = df_sens,
      aes(x = alcohol, y = calidad_suav),
      color = "black",
      linewidth = 0.8
    ) +
    geom_vline(
      xintercept = optimo_alc,
      linetype = "dotted",
      color = paleta["rojo"],
      linewidth = 0.6
    ) +
    # Lotes reales, graficados con sus propias covariables
    geom_point(
      data = datos_mortino,
      aes(x = alcohol, y = prediccion),
      color = paleta["naranja"],
      fill = paleta["naranja"],
      size = 3.2,
      alpha = 0.95,
      shape = 21
    ) +
    geom_text(
      data = datos_mortino,
      aes(x = alcohol, y = prediccion, label = num_lote),
      color = "white",
      size = 2.3,
      fontface = "bold"
    ) +
    annotate(
      "label",
      x = 14.85,
      y = y_max_s - 0.05,
      label = txt_opt_box,
      hjust = 1.0,
      vjust = 1.0,
      size = 2.4,
      family = "sans",
      fontface = "bold",
      fill = alpha("white", 0.95),
      color = as.character(paleta["rojo"]),
      label.padding = unit(0.30, "lines"),
      linewidth = 0.3
    ) +
    annotate(
      "label",
      x = 14.85,
      y = y_min_s + 0.05,
      label = tabla_sens_txt,
      hjust = 1.0,
      vjust = 0.0,
      size = 2.3,
      family = "mono",
      fill = alpha("white", 0.95),
      color = "gray15",
      label.padding = unit(0.30, "lines"),
      linewidth = 0.3,
      lineheight = 1.0
    ) +
    scale_x_continuous(
      breaks = seq(7, 15, 1),
      limits = c(7, 15),
      labels = label_number(accuracy = 1, decimal.mark = dec),
      expand = c(0.01, 0.01)
    ) +
    scale_y_continuous(
      limits = c(y_min_s, y_max_s),
      breaks = seq(3.5, 7.5, 0.5),
      labels = label_number(accuracy = 0.1, decimal.mark = dec),
      expand = c(0, 0)
    ) +
    labs(x = L$sens_x, y = L$sens_y) +
    tema_paper(base_size = 8)
  # 8,5 cm de alto: con 9,0 la pagina de flotantes (Fig. 4 + Tabla 2 + Fig. 5) se desborda
  guardar_figura(p4, paste0("fig_sensibilidad", sufijo), 17.4, 8.5)

  # ---------------------------------------------------------------------------
  # MATRIZ DE CORRELACION (dos columnas: 17,4 x 11 cm; triangular inferior)
  # ---------------------------------------------------------------------------
  mat_cor <- cor(datos_mortino[, predictors], use = "pairwise.complete.obs")
  nombres_vars <- L$vars[predictors]
  rownames(mat_cor) <- nombres_vars
  colnames(mat_cor) <- nombres_vars

  df_cor <- as.data.frame(as.table(mat_cor)) %>%
    rename(r = Freq) %>%
    mutate(
      idx1 = as.numeric(Var1),
      idx2 = as.numeric(Var2)
    ) %>%
    filter(idx1 > idx2) %>% # Mantiene solo la mitad inferior sin diagonal
    mutate(
      Var1 = factor(Var1, levels = rev(nombres_vars)),
      Var2 = factor(Var2, levels = nombres_vars),
      label_r = fmt(r, 2, idioma)
    )

  p5 <- ggplot(df_cor, aes(x = Var2, y = Var1, fill = r)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = label_r), size = 2.5, color = "black") +
    scale_fill_gradient2(
      low = paleta["azul"],
      mid = "white",
      high = paleta["naranja"],
      midpoint = 0,
      limits = c(-1, 1),
      breaks = seq(-1, 1, 0.5),
      labels = label_number(accuracy = 0.1, decimal.mark = dec),
      name = L$cor
    ) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(x = NULL, y = NULL) +
    tema_paper(base_size = 8) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8),
      axis.text.y = element_text(size = 8),
      axis.ticks = element_blank(),
      panel.border = element_blank(),
      legend.position = "inside",
      legend.position.inside = c(0.88, 0.72),
      legend.direction = "vertical",
      legend.background = element_rect(
        fill = alpha("white", 0.92),
        color = "gray70",
        linewidth = 0.25
      ),
      legend.title = element_text(size = 8, face = "bold", hjust = 0.5),
      legend.text = element_text(size = 7),
      legend.key.height = unit(0.8, "cm"),
      legend.key.width = unit(0.35, "cm")
    )
  guardar_figura(p5, paste0("fig_correlacion", sufijo), 17.4, 11)
}

# Generar coleccion completa de figuras
generar_graficos("es")
generar_graficos("en")

# =============================================================================
# 4b. EXPORTAR VALORES AL .tex (macros \Val... de mortino_poli.tex)
# =============================================================================

tex_num <- function(x, d) formatC(x, format = "f", digits = d, decimal.mark = ",")
imp_pct <- importancia_raw %>% mutate(pct = Gain / sum(Gain) * 100)
pct_de <- function(f) sum(imp_pct$pct[imp_pct$Feature %in% f])
nombres_min <- c(
  chlorides = "cloruros",
  sulphates = "sulfatos",
  density = "densidad relativa",
  residual_sugar = "azúcar residual",
  pH = "pH",
  fixed_acidity = "acidez fija",
  citric_acid = "ácido cítrico"
)
resto <- imp_pct %>%
  filter(Feature %in% names(nombres_min)) %>%
  arrange(desc(pct))
items <- paste0(nombres_min[resto$Feature], " (", tex_num(resto$pct, 2), "\\,\\%)")
txt_resto <- paste0(paste(head(items, -1), collapse = ", "), " y ", tail(items, 1))

# Estado final de cada lote (misma regla que el Algoritmo 4 y la Tabla 2)
estado_lote <- function(conforme, pred) {
  case_when(
    !conforme ~ "RECHAZADO: incumple NTE INEN 374",
    round(pred, 2) < UMBRAL_Q ~ "RECHAZADO: calidad insuficiente (< 6/10)",
    TRUE ~ "APROBADO"
  )
}
datos_mortino$estado <- estado_lote(
  datos_mortino$conforme_inen,
  datos_mortino$prediccion
)
filas_inen <- sprintf(
  "%s & %s & %s & %s & %s & %s \\\\",
  paste("Lote", datos_mortino$num_lote),
  tex_num(datos_mortino$alcohol, 2),
  tex_num(datos_mortino$volatile_acidity, 2),
  tex_num(datos_mortino$prediccion, 2),
  ifelse(datos_mortino$conforme_inen, "Conforme", "No conforme"),
  gsub("(< 6/10)", "($< 6/10$)", datos_mortino$estado, fixed = TRUE)
)

valores <- c(
  ValGainAlcohol = tex_num(pct_de("alcohol"), 2),
  ValGainAcVol = tex_num(pct_de("volatile_acidity"), 2),
  ValGainSOdos = tex_num(
    pct_de(c("free_sulfur_dioxide", "total_sulfur_dioxide")),
    2
  ),
  ValGainResto = txt_resto,
  ValAsintota = tex_num(mean_alcohol, 2),
  ValEpsAlc = tex_num(eps_final, 3),
  ValOptimo = tex_num(optimo_alc, 1),
  ValOptimoEN = formatC(optimo_alc, format = "f", digits = 1), # abstract (punto decimal)
  ValOptimoCal = tex_num(optimo_cal, 2),
  ValNumAprob = sum(datos_mortino$estado == "APROBADO"),
  ValNumRechQ = sum(datos_mortino$conforme_inen & datos_mortino$estado != "APROBADO"),
  ValRMSE = tex_num(rmse_transfer, 2),
  ValMediaPanel = tex_num(media_t0, 3),
  ValDEPanel = tex_num(sd(datos_mortino[[col_cal_obs]]), 3),
  ValICinf = tex_num(lim_inf, 3),
  ValICsup = tex_num(lim_sup, 3),
  ValICamp = tex_num(lim_sup - lim_inf, 3),
  # Error de Monte Carlo del bootstrap (Hoeffding, 99 %): las medias bootstrap
  # estan acotadas por el rango de las puntuaciones observadas
  ValEpsBoot = tex_num(
    diff(range(datos_mortino[[col_cal_obs]])) * sqrt(log(2 / ALFA) / (2 * res_boot$R)),
    2
  ),
  ValCalMin = tex_num(min(datos_mortino[[col_cal_obs]]), 0),
  ValCalMax = tex_num(max(datos_mortino[[col_cal_obs]]), 0),
  ValFilasINEN = paste0("%\n", paste0("\t", filas_inen, collapse = "\n"), "\n")
)
con <- file("results/valores_paper.tex", open = "w", encoding = "UTF-8")
writeLines(
  c(
    "% Generado por 04_bootstrap_inen.R. No editar a mano.",
    sprintf("\\newcommand{\\%s}{%s}", names(valores), valores)
  ),
  con
)
close(con)
message("Guardado: results/valores_paper.tex")

# =============================================================================
# 5. GENERACION DE TABLAS BILINGUES SIN CORRUPCION
# =============================================================================

# --- Tabla 1: Importancia (ES / EN) ---
tabla_importancia_es <- importancia_raw %>%
  mutate(
    Variable = etiq$es$vars[Feature],
    Gain = round(Gain, 4),
    Cover = round(Cover, 4),
    `Gain (%)` = round(Gain / sum(Gain) * 100, 2)
  ) %>%
  select(Variable, Gain, Cover, `Gain (%)`) %>%
  arrange(desc(`Gain (%)`))
write.csv(
  tabla_importancia_es,
  "results/tablas/tabla_importancia_es.csv",
  row.names = FALSE
)

tabla_importancia_en <- importancia_raw %>%
  mutate(
    Variable = etiq$en$vars[Feature],
    Gain = round(Gain, 4),
    Cover = round(Cover, 4),
    `Gain (%)` = round(Gain / sum(Gain) * 100, 2)
  ) %>%
  select(Variable, Gain, Cover, `Gain (%)`) %>%
  arrange(desc(`Gain (%)`))
write.csv(
  tabla_importancia_en,
  "results/tablas/tabla_importancia_en.csv",
  row.names = FALSE
)

# --- Tabla 2: Estadisticas Corregidas con Regex ---
tabla_estadisticas_es <- datos_mortino %>%
  select(all_of(predictors)) %>%
  summarise(across(
    everything(),
    list(
      Media = ~ round(mean(.x, na.rm = TRUE), 3),
      DE = ~ round(sd(.x, na.rm = TRUE), 3),
      Min = ~ round(min(.x, na.rm = TRUE), 3),
      Max = ~ round(max(.x, na.rm = TRUE), 3)
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  pivot_longer(
    everything(),
    names_to = c("Variable", "Estadistico"),
    names_pattern = "(.*)_(.*)",
    values_to = "Valor"
  ) %>%
  pivot_wider(names_from = Estadistico, values_from = Valor) %>%
  mutate(Variable = etiq$es$vars[Variable])
write.csv(
  tabla_estadisticas_es,
  "results/tablas/tabla_estadisticas_es.csv",
  row.names = FALSE
)

tabla_estadisticas_en <- datos_mortino %>%
  select(all_of(predictors)) %>%
  summarise(across(
    everything(),
    list(
      Mean = ~ round(mean(.x, na.rm = TRUE), 3),
      SD = ~ round(sd(.x, na.rm = TRUE), 3),
      Min = ~ round(min(.x, na.rm = TRUE), 3),
      Max = ~ round(max(.x, na.rm = TRUE), 3)
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  pivot_longer(
    everything(),
    names_to = c("Variable", "Statistic"),
    names_pattern = "(.*)_(.*)",
    values_to = "Value"
  ) %>%
  pivot_wider(names_from = Statistic, values_from = Value) %>%
  mutate(Variable = etiq$en$vars[Variable])
write.csv(
  tabla_estadisticas_en,
  "results/tablas/tabla_estadisticas_en.csv",
  row.names = FALSE
)

# --- Tabla 3: Conformidad NTE INEN 374 ---
tabla_inen_es <- datos_mortino %>%
  mutate(
    Estado = estado_lote(conforme_inen, prediccion),
    Prioridad = case_when(
      Estado == "APROBADO" ~ "Baja",
      !conforme_inen ~ "Alta",
      TRUE ~ "Media"
    ),
    Recomendacion = case_when(
      Estado == "APROBADO" ~ "Mantener condiciones actuales de fermentacion.",
      volatile_acidity >
        NORMA$av_max ~ "Controlar condiciones microbiologicas y aplicar sulfitacion correctiva.",
      alcohol < NORMA$alc_min ~ "Evaluar chaptalizacion controlada del mosto.",
      total_sulfur_dioxide > NORMA$so2t_max ~ "Reducir la dosis de sulfitado.",
      TRUE ~ "Revisar balance de nutrientes, cepa de levadura y temperatura."
    )
  ) %>%
  select(
    No = num_lote,
    Lote = id_lote,
    Alcohol = alcohol,
    `Acidez Vol.` = volatile_acidity,
    `Calidad Pred.` = prediccion,
    Estado = Estado,
    Prioridad = Prioridad,
    Recomendacion = Recomendacion
  ) %>%
  mutate(across(c(Alcohol, `Acidez Vol.`, `Calidad Pred.`), ~ round(.x, 2)))
write.csv(tabla_inen_es, "results/tablas/tabla_inen_es.csv", row.names = FALSE)

tabla_inen_en <- datos_mortino %>%
  mutate(
    Status = case_when(
      !conforme_inen ~ "REJECTED: does not meet NTE INEN 374",
      round(prediccion, 2) < UMBRAL_Q ~ "REJECTED: insufficient quality (< 6/10)",
      TRUE ~ "APPROVED"
    ),
    Priority = case_when(
      Status == "APPROVED" ~ "Low",
      !conforme_inen ~ "High",
      TRUE ~ "Medium"
    ),
    Recommendation = case_when(
      Status == "APPROVED" ~ "Maintain current fermentation conditions.",
      volatile_acidity >
        NORMA$av_max ~ "Control microbiological conditions and apply corrective sulfiting.",
      alcohol < NORMA$alc_min ~ "Evaluate controlled chaptalization of the must.",
      total_sulfur_dioxide > NORMA$so2t_max ~ "Reduce the sulfiting dose.",
      TRUE ~ "Review nutrient balance, yeast strain, and temperature."
    )
  ) %>%
  select(
    No = num_lote,
    Batch = id_lote,
    Alcohol = alcohol,
    `Volatile Ac.` = volatile_acidity,
    `Pred. Quality` = prediccion,
    Status = Status,
    Priority = Priority,
    Recommendation = Recommendation
  ) %>%
  mutate(across(c(Alcohol, `Volatile Ac.`, `Pred. Quality`), ~ round(.x, 2)))
write.csv(tabla_inen_en, "results/tablas/tabla_inen_en.csv", row.names = FALSE)

# =============================================================================
# 6. EXPORTAR TABLAS A LATEX
# =============================================================================

print(
  xtable(
    tabla_importancia_es,
    caption = "Importancia relativa de las variables fisicoquimicas en el modelo XGBoost.",
    label = "tab:importancia"
  ),
  file = "results/tablas/tabla_importancia_es.tex",
  include.rownames = FALSE,
  booktabs = TRUE,
  floating = TRUE,
  sanitize.text.function = function(x) x
)

print(
  xtable(
    tabla_importancia_en,
    caption = "Relative importance of physicochemical variables in the XGBoost model.",
    label = "tab:importance"
  ),
  file = "results/tablas/tabla_importancia_en.tex",
  include.rownames = FALSE,
  booktabs = TRUE,
  floating = TRUE,
  sanitize.text.function = function(x) x
)

print(
  xtable(
    tabla_estadisticas_es,
    caption = "Estadisticas descriptivas de las variables fisicoquimicas del vino de mortino.",
    label = "tab:estadisticas"
  ),
  file = "results/tablas/tabla_estadisticas_es.tex",
  include.rownames = FALSE,
  booktabs = TRUE,
  floating = TRUE,
  sanitize.text.function = function(x) x
)

print(
  xtable(
    tabla_estadisticas_en,
    caption = "Descriptive statistics of the physicochemical variables of mortino wine.",
    label = "tab:statistics"
  ),
  file = "results/tablas/tabla_estadisticas_en.tex",
  include.rownames = FALSE,
  booktabs = TRUE,
  floating = TRUE,
  sanitize.text.function = function(x) x
)

print(
  xtable(
    tabla_inen_es,
    caption = "Clasificacion de los 8 lotes de vino de mortino segun NTE INEN 374 y retroalimentacion tecnica.",
    label = "tab:inen"
  ),
  file = "results/tablas/tabla_inen_es.tex",
  include.rownames = FALSE,
  booktabs = TRUE,
  floating = TRUE,
  floating.environment = "sidewaystable",
  sanitize.text.function = function(x) x
)

print(
  xtable(
    tabla_inen_en,
    caption = "Classification of the 8 mortino wine batches according to NTE INEN 374 and technical feedback.",
    label = "tab:inen_en"
  ),
  file = "results/tablas/tabla_inen_en.tex",
  include.rownames = FALSE,
  booktabs = TRUE,
  floating = TRUE,
  floating.environment = "sidewaystable",
  sanitize.text.function = function(x) x
)

message("Ejecucion completada con exito.")
