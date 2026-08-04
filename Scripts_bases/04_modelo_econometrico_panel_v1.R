# ==============================================================================
# 04_modelo_econometrico_panel.R (Versión para Replicación Académica)
# ==============================================================================
# Modelación Econométrica de Datos Panel: Impacto del Cambio Climático y 
# Moderación por Infraestructura sobre el Rendimiento Académico (Saber 11)
#
# Autor: Proyecto Investigación Cundinamarca (2014-2019)
# Paquete econométrico principal: fixest (Fast Fixed-Effects Estimation)
# ==============================================================================

suppressMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(fixest)
  library(modelsummary)
  library(ggplot2)
})

base_dir  <- "c:/Users/diego/Documents/CongresoTunja"
panel_path <- file.path(base_dir, "Bases_Procesadas", "Panel_Colegios_Clima_Coordenadas_2014_2019.csv")

cat("Cargando base de datos panel procesada...\n")
df_panel <- read_csv(panel_path, show_col_types = FALSE)

# Pre-procesamiento: rezagos, adelantos y factores
df_panel <- df_panel %>%
  arrange(cole_cod_dane_establecimiento, anio) %>%
  group_by(cole_cod_dane_establecimiento) %>%
  mutate(
    temp_lead1  = lead(temp_efectiva, 1)   # Para Prueba de Placebo
  ) %>%
  ungroup() %>%
  mutate(
    cole_cod_dane_establecimiento = as.factor(cole_cod_dane_establecimiento),
    anio_num      = anio,
    anio          = as.factor(anio),
    mcpio_norm    = as.factor(mcpio_norm),
    cole_naturaleza = as.factor(cole_naturaleza)
  )

# ==============================================================================
# DIAGNÓSTICO PREVIO
# ==============================================================================
cat("\n--- DIAGNÓSTICO PREVIO ---\n")
cat("Municipios (clusters):", n_distinct(df_panel$mcpio_norm), "\n")
cat("Correlación Temperatura-Precipitación:",
    round(cor(df_panel$temp_efectiva, df_panel$precip_efectiva, use = "complete.obs"), 4), "\n")

within_stats <- df_panel %>%
  group_by(cole_cod_dane_establecimiento) %>%
  summarise(sd_temp_within = sd(temp_efectiva, na.rm = TRUE), .groups = "drop")
cat("SD Within-Colegio Temperatura — Promedio:",
    round(mean(within_stats$sd_temp_within, na.rm = TRUE), 3),
    "°C | Mediana:", round(median(within_stats$sd_temp_within, na.rm = TRUE), 3), "°C\n")


# ==============================================================================
# ETAPA 1: ESPECIFICACIÓN PRINCIPAL
# ==============================================================================
cat("\n--- ETAPA 1: ESPECIFICACIÓN PRINCIPAL ---\n")

# Modelo 1: OLS Baseline (Sin Efectos Fijos)
m1_ols <- feols(
  prom_punt_global ~ temp_efectiva + precip_efectiva +
                     pct_estrato_bajo + pct_internet + ind_hacinamiento_prom,
  data    = df_panel,
  cluster = ~mcpio_norm
)

# Modelo 2: Two-Way Fixed Effects (Colegio + Año)
m2_twfe <- feols(
  prom_punt_global ~ temp_efectiva + precip_efectiva +
                     pct_estrato_bajo + pct_internet + ind_hacinamiento_prom |
                     cole_cod_dane_establecimiento + anio,
  data    = df_panel,
  cluster = ~mcpio_norm
)

# Modelo 3: ESPECIFICACIÓN PRINCIPAL — TWFE + Mecanismos de Infraestructura
m3_principal <- feols(
  prom_punt_global ~ temp_efectiva + precip_efectiva +
                     pct_estrato_bajo + pct_internet + ind_hacinamiento_prom +
                     temp_efectiva:infra_energia_red +
                     temp_efectiva:infra_pae |
                     cole_cod_dane_establecimiento + anio,
  data    = df_panel,
  cluster = ~mcpio_norm
)

etable(m1_ols, m2_twfe, m3_principal,
       headers = c("OLS", "TWFE", "Principal (Infra.)"))


# ==============================================================================
# ETAPA 2: PRUEBAS DE ROBUSTEZ E IDENTIFICACIÓN
# ==============================================================================
cat("\n--- ETAPA 2: ROBUSTEZ E IDENTIFICACIÓN ---\n")

# Robustez 1: Panel Balanceado (solo colegios con los 6 años completos)
colegios_balanceados <- df_panel %>%
  group_by(cole_cod_dane_establecimiento) %>%
  filter(n_distinct(anio) == 6) %>%
  ungroup()

cat("Colegios en panel balanceado:", n_distinct(colegios_balanceados$cole_cod_dane_establecimiento), "\n")

r1_balanceado <- feols(
  prom_punt_global ~ temp_efectiva + precip_efectiva + pct_estrato_bajo + pct_internet |
                     cole_cod_dane_establecimiento + anio,
  data    = colegios_balanceados,
  cluster = ~mcpio_norm
)

# Robustez 2: Solo Colegios Oficiales (Públicos)
r2_oficiales <- feols(
  prom_punt_global ~ temp_efectiva + precip_efectiva + pct_estrato_bajo + pct_internet |
                     cole_cod_dane_establecimiento + anio,
  data    = filter(df_panel,
                   str_detect(str_to_upper(cole_naturaleza), "OFICIAL") &
                   !str_detect(str_to_upper(cole_naturaleza), "NO OFICIAL")),
  cluster = ~mcpio_norm
)

# Robustez 3: Solo Colegios No Oficiales (Privados)
r3_privados <- feols(
  prom_punt_global ~ temp_efectiva + precip_efectiva + pct_estrato_bajo + pct_internet |
                     cole_cod_dane_establecimiento + anio,
  data    = filter(df_panel, str_detect(str_to_upper(cole_naturaleza), "NO OFICIAL")),
  cluster = ~mcpio_norm
)

# Robustez 4: Prueba de Placebo — Temperatura Futura (t+1)
r4_placebo <- feols(
  prom_punt_global ~ temp_efectiva + temp_lead1 + precip_efectiva +
                     pct_estrato_bajo + pct_internet |
                     cole_cod_dane_establecimiento + anio,
  data    = filter(df_panel, !is.na(temp_lead1)),
  cluster = ~mcpio_norm
)

etable(r1_balanceado, r2_oficiales, r3_privados, r4_placebo,
       headers = c("Panel Bal.", "Oficiales", "Privados", "Placebo (t+1)"))


# ==============================================================================
# ETAPA 3: GRÁFICO DE COEFICIENTES
# ==============================================================================
cat("\nGenerando figura de coeficientes...\n")

models_plot <- list(
  "OLS"             = m1_ols,
  "TWFE"            = m2_twfe,
  "Principal"       = m3_principal,
  "Placebo (t+1)"   = r4_placebo
)

p <- modelplot(
  models_plot,
  coef_map = c(
    "temp_efectiva"           = "Temperatura Efectiva (°C)",
    "precip_efectiva"         = "Precipitación Efectiva (mm)",
    "temp_lead1"              = "Placebo: Temp. Futura (t+1)"
  )
) +
  theme_minimal(base_size = 13) +
  labs(
    title    = "Sensibilidad Climática y Validación de Identificación (Saber 11)",
    subtitle = "Coeficientes e Intervalos de Confianza al 95%",
    x        = "Efecto Estimado sobre Puntaje Saber 11",
    y        = "Especificación"
  ) +
  theme(plot.title = element_text(face = "bold"))

out_fig <- file.path(base_dir, "Bases_Procesadas", "coeficientes_modelo.png")
ggsave(out_fig, p, width = 9, height = 5, dpi = 300)
cat("Grafico guardado en:", out_fig, "\n")


# ==============================================================================
# ETAPA 4: EXPORTACIÓN DE TABLA ACADÉMICA EN HTML (COMPLETA)
# ==============================================================================
cat("\nGenerando tabla resumen académica en formato HTML...\n")

all_models <- list(
  "OLS Baseline"      = m1_ols,
  "TWFE Basal"        = m2_twfe,
  "Principal (Infra)" = m3_principal,
  "Panel Balanceado"  = r1_balanceado,
  "Sector Público"    = r2_oficiales,
  "Sector Privado"    = r3_privados,
  "Placebo (Lead t+1)"= r4_placebo
)

coef_labels <- c(
  "Constant"                            = "(Intercepto)",
  "temp_efectiva"                       = "Temp. Efectiva (°C)",
  "precip_efectiva"                     = "Precip. Efectiva (mm)",
  "pct_estrato_bajo"                    = "% Estrato Vivienda Bajo",
  "pct_internet"                        = "% Conexión Internet",
  "ind_hacinamiento_prom"               = "Índice de Hacinamiento Hogar",
  "temp_efectiva:infra_energia_red"     = "Temp. × Red Energía Eléctrica",
  "temp_efectiva:infra_pae"             = "Temp. × Programa PAE",
  "temp_lead1"                          = "Placebo: Temp. Efectiva (t+1)"
)

# Mapeo personalizado de estadísticos de bondad de ajuste
gof_custom <- list(
  list("raw" = "nobs", "clean" = "Observaciones", "fmt" = 0),
  list("raw" = "r.squared", "clean" = "R²", "fmt" = 3),
  list("raw" = "adj.r.squared", "clean" = "R² Ajustado", "fmt" = 3),
  list("raw" = "within.r.squared", "clean" = "R² Within", "fmt" = 3)
)

out_html <- file.path(base_dir, "Bases_Procesadas", "tabla_resumen_modelos.html")

modelsummary(
  all_models,
  coef_map = coef_labels,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  gof_map = gof_custom,
  output = out_html,
  title = "Tabla de Regresión: Impacto de Choques Térmicos sobre Saber 11 y Mecanismos de Moderación"
)

cat("Tabla HTML guardada exitosamente en:", out_html, "\n")

