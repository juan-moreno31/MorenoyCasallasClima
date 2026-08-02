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
  library(stringr)      # Manipulación de caracteres
  library(fixest)       # Estimación ultra-rápida de Efectos Fijos
  library(modelsummary) # Generación de tablas y gráficos de publicación
  library(ggplot2)      # Gráficos de coeficientes
})

base_dir <- "c:/Users/diego/Documents/CongresoTunja"
panel_path <- file.path(base_dir, "Bases_Procesadas", "Panel_Colegios_Clima_Coordenadas_2014_2019.csv")

cat("Cargando base de datos panel procesada...\n")
df_panel <- read_csv(panel_path, show_col_types = FALSE)

# Pre-procesamiento econométrico, Rezagos (Lags) y Adelantos (Leads / Placebo) por Colegio
df_panel <- df_panel %>%
  arrange(cole_cod_dane_establecimiento, anio) %>%
  group_by(cole_cod_dane_establecimiento) %>%
  mutate(
    temp_lag1 = lag(temp_efectiva, 1),
    temp_lead1 = lead(temp_efectiva, 1),  # Variable para Prueba de Placebo / Falsación
    precip_lag1 = lag(precip_efectiva, 1)
  ) %>%
  ungroup() %>%
  mutate(
    cole_cod_dane_establecimiento = as.factor(cole_cod_dane_establecimiento),
    anio_num = anio,
    anio = as.factor(anio),
    mcpio_norm = as.factor(mcpio_norm),
    cole_naturaleza = as.factor(cole_naturaleza)
  )

# ==============================================================================
# DIAGNÓSTICO PREVIO: VARIACIÓN TEMPORAL, MULTICOLINEALIDAD Y CLUSTERS
# ==============================================================================
cat("\n----------------------------------------------------------------------\n")
cat(" DIAGNÓSTICO PREVIO: VARIACIÓN WITHIN, MULTICOLINEALIDAD Y CLUSTERS\n")
cat("----------------------------------------------------------------------\n")

cat("Número de municipios (clusters):", n_distinct(df_panel$mcpio_norm), "\n")

cor_temp_precip <- cor(df_panel$temp_efectiva, df_panel$precip_efectiva, use = "complete.obs")
cat("Correlación lineal (Temperatura vs Precipitación):", round(cor_temp_precip, 4), "\n")

within_stats <- df_panel %>%
  group_by(cole_cod_dane_establecimiento) %>%
  summarise(
    sd_temp_within = sd(temp_efectiva, na.rm = TRUE),
    sd_precip_within = sd(precip_efectiva, na.rm = TRUE),
    .groups = "drop"
  )

cat("Desviación Estándar Within Colegio (Temperatura): Promedio =", 
    round(mean(within_stats$sd_temp_within, na.rm = TRUE), 3), 
    "°C | Mediana =", round(median(within_stats$sd_temp_within, na.rm = TRUE), 3), "°C\n")

var_mcpio_anio <- df_panel %>%
  group_by(mcpio_norm, anio) %>%
  summarise(sd_temp = sd(temp_efectiva, na.rm = TRUE), .groups = "drop")
cat("Desviación Estándar Dentro de Municipio x Año (SD Temp): Mediana =", 
    round(median(var_mcpio_anio$sd_temp, na.rm = TRUE), 4), "°C (Demuestra que Mcpio x Año es una robustez extrema)\n")


# ==============================================================================
# ETAPA 1: ESPECIFICACIÓN PRINCIPAL Y MECANISMOS DE INFRAESTRUCTURA (TWFE)
# ==============================================================================
cat("\n----------------------------------------------------------------------\n")
cat(" ETAPA 1: ESPECIFICACIÓN PRINCIPAL (TWFE & MECANISMOS)\n")
cat("----------------------------------------------------------------------\n")

# Modelo 1: OLS Lineal Baseline (Sin Efectos Fijos)
m1_ols <- feols(
  prom_punt_global ~ temp_efectiva + precip_efectiva + pct_estrato_bajo + pct_internet + ind_hacinamiento_prom,
  data = df_panel,
  cluster = ~mcpio_norm
)

# Modelo 2: Efectos Fijos por Colegio (Within Estimator)
m2_fe_colegio <- feols(
  prom_punt_global ~ temp_efectiva + precip_efectiva | cole_cod_dane_establecimiento,
  data = df_panel,
  cluster = ~mcpio_norm
)

# Modelo 3: Two-Way Fixed Effects - TWFE (Colegio + Año)
m3_twfe <- feols(
  prom_punt_global ~ temp_efectiva + precip_efectiva | cole_cod_dane_establecimiento + anio,
  data = df_panel,
  cluster = ~mcpio_norm
)

# Modelo 4: TWFE + Controles Socioeconómicos Dinámicos
m4_twfe_controles <- feols(
  prom_punt_global ~ temp_efectiva + precip_efectiva + pct_estrato_bajo + pct_internet + ind_hacinamiento_prom | cole_cod_dane_establecimiento + anio,
  data = df_panel,
  cluster = ~mcpio_norm
)

# Modelo 5: ESPECIFICACIÓN PRINCIPAL (TWFE + Mecanismos con Sintaxis Limpia de Interacción)
m5_principal <- feols(
  prom_punt_global ~ temp_efectiva + precip_efectiva + 
                     pct_estrato_bajo + pct_internet + ind_hacinamiento_prom +
                     temp_efectiva:infra_energia_red + 
                     temp_efectiva:infra_pae | cole_cod_dane_establecimiento + anio,
  data = df_panel,
  cluster = ~mcpio_norm
)

# Visualización limpia de la Etapa 1
cat("\nTabla Resumen Etapa 1 (Modelos 1 a 5):\n")
etable(m1_ols, m2_fe_colegio, m3_twfe, m4_twfe_controles, m5_principal, 
       headers = c("OLS", "FE Colegio", "TWFE", "TWFE + Controles", "Principal (Energía + PAE)"))


# ==============================================================================
# ETAPA 2: PRUEBAS DE ROBUSTEZ, IDENTIFICACIÓN, PLACEBO Y PONDERACIÓN
# ==============================================================================
cat("\n----------------------------------------------------------------------\n")
cat(" ETAPA 2: PRUEBAS DE ROBUSTEZ, IDENTIFICACIÓN Y PONDERACIÓN (WLS)\n")
cat("----------------------------------------------------------------------\n")

# Robustez 1: Panel Balanceado Estricto (Uso defensivo de n_distinct y distinct)
colegios_balanceados <- df_panel %>%
  group_by(cole_cod_dane_establecimiento) %>%
  filter(n_distinct(anio) == 6) %>%
  distinct(cole_cod_dane_establecimiento, anio, .keep_all = TRUE) %>%
  ungroup()

cat("Colegios en panel balanceado estricto (6 años completos):", n_distinct(colegios_balanceados$cole_cod_dane_establecimiento), "\n")

r1_panel_balanceado <- feols(
  prom_punt_global ~ temp_efectiva + precip_efectiva + pct_estrato_bajo + pct_internet | cole_cod_dane_establecimiento + anio,
  data = colegios_balanceados,
  cluster = ~mcpio_norm
)

# Robustez 2: Heterogeneidad Oficial (Público) vs No Oficial (Privado)
r2_oficiales <- feols(
  prom_punt_global ~ temp_efectiva + precip_efectiva + pct_estrato_bajo + pct_internet | cole_cod_dane_establecimiento + anio,
  data = filter(df_panel, str_detect(str_to_upper(cole_naturaleza), "OFICIAL") & !str_detect(str_to_upper(cole_naturaleza), "NO OFICIAL")),
  cluster = ~mcpio_norm
)

r2_privados <- feols(
  prom_punt_global ~ temp_efectiva + precip_efectiva + pct_estrato_bajo + pct_internet | cole_cod_dane_establecimiento + anio,
  data = filter(df_panel, str_detect(str_to_upper(cole_naturaleza), "NO OFICIAL")),
  cluster = ~mcpio_norm
)

# Robustez 3: Tendencias Temporales Específicas por Colegio (Colegio[anio])
r3_tendencias_colegio <- feols(
  prom_punt_global ~ temp_efectiva + precip_efectiva + pct_estrato_bajo + pct_internet | cole_cod_dane_establecimiento[anio_num] + anio,
  data = df_panel,
  cluster = ~mcpio_norm
)

# Robustez 4: Robustez Extrema - Efectos Fijos Municipio x Año
r4_mcpio_anio <- feols(
  prom_punt_global ~ temp_efectiva + precip_efectiva + pct_estrato_bajo + pct_internet | cole_cod_dane_establecimiento + mcpio_norm^anio,
  data = df_panel,
  cluster = ~mcpio_norm
)

# Robustez 5: PRUEBA DE PLACEBO / FALSACIÓN (Temperatura Futura t+1)
r5_placebo_lead <- feols(
  prom_punt_global ~ temp_efectiva + temp_lead1 + precip_efectiva + pct_estrato_bajo + pct_internet | cole_cod_dane_establecimiento + anio,
  data = filter(df_panel, !is.na(temp_lead1)),
  cluster = ~mcpio_norm
)

# Robustez 6: Two-Way Clustering (Municipio + Colegio)
r6_tw_clustering <- feols(
  prom_punt_global ~ temp_efectiva + precip_efectiva + pct_estrato_bajo + pct_internet | cole_cod_dane_establecimiento + anio,
  data = df_panel,
  cluster = ~mcpio_norm + cole_cod_dane_establecimiento
)

# Robustez 7: Estimación Ponderada WLS por Tamaño de Cohorte (total_estudiantes)
r7_wls_ponderado <- feols(
  prom_punt_global ~ temp_efectiva + precip_efectiva + pct_estrato_bajo + pct_internet | cole_cod_dane_establecimiento + anio,
  data = df_panel,
  weights = ~total_estudiantes,
  cluster = ~mcpio_norm
)

# Visualización limpia de la Etapa 2
cat("\nTabla Resumen Etapa 2 (Robustez, Placebo y WLS Ponderado):\n")
etable(r1_panel_balanceado, r2_oficiales, r2_privados, r3_tendencias_colegio, r4_mcpio_anio, r5_placebo_lead, r6_tw_clustering, r7_wls_ponderado,
       headers = c("Panel Bal.", "Oficiales", "Privados", "Tend. Colegio", "Mcpio x Año FE", "Placebo (t+1)", "Two-Way Cluster", "WLS Ponderado"))


# ==============================================================================
# ETAPA 3: GENERACIÓN DE GRÁFICO DE COEFICIENTES (MODELPLOT)
# ==============================================================================
cat("\nGenerando figura de coeficientes etiquetada para el paper (modelplot)...\n")
models_list <- list(
  "TWFE Basal" = m3_twfe,
  "TWFE + Controles" = m4_twfe_controles,
  "Placebo (Lead t+1)" = r5_placebo_lead,
  "WLS Ponderado" = r7_wls_ponderado
)

p <- modelplot(
  models_list, 
  coef_map = c(
    "temp_efectiva" = "Temperatura Efectiva (°C)",
    "precip_efectiva" = "Precipitación Efectiva (mm)",
    "temp_lead1" = "Placebo: Temp Futura (t+1)"
  )
) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Sensibilidad Climática y Validación de Identificación (Saber 11)",
    subtitle = "Coeficientes e Intervalos de Confianza (95%) con Nombres de Variables Etiquetados",
    x = "Efecto Estimado sobre Puntaje Saber 11",
    y = "Especificación Econométrica"
  ) +
  theme(plot.title = element_text(face = "bold"))

out_fig <- file.path(base_dir, "Bases_Procesadas", "coeficientes_modelo.png")
ggsave(out_fig, p, width = 9, height = 6, dpi = 300)
cat("✔ Gráfico de coeficientes guardado en:", out_fig, "\n")

cat("\n=== SCRIPT ECONOMÉTRICO LISTO PARA ESTIMACIÓN Y REPLICACIÓN DE RESULTADOS ===\n")




