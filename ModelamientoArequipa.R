# MODELAMIENTO ZERO-INFLATED EN R - AREQUIPA
# Análisis completo para tesis de bachiller - Comparación con Lima

# Cargar librerías (ya deberían estar instaladas)
library(pscl)      # Para modelos zero-inflated (ZIP, ZINB)
library(MASS)      # Para binomial negativa
library(readxl)    # Para leer Excel
library(ggplot2)   # Para gráficos
library(dplyr)     # Para manipulación de datos
library(AER)       # Para tests estadísticos

cat("=======================================================\n")
cat("MODELAMIENTO ZERO-INFLATED EN R - AREQUIPA\n")
cat("=======================================================\n")

# =============================================================================
# 1. CARGAR Y PREPARAR DATOS AREQUIPA
# =============================================================================

# Cargar datos de Arequipa
datos_arequipa <- read_excel("Dataset_Arequipa_para_R.xlsx", sheet = "Datos_Arequipa")

cat("\n--- INFORMACIÓN DEL DATASET AREQUIPA ---\n")
cat("Dimensiones:", dim(datos_arequipa), "\n")
cat("Variables:", names(datos_arequipa), "\n")

# Convertir variables categóricas a factores
datos_arequipa$tipo_accidente <- as.factor(datos_arequipa$tipo_accidente)
datos_arequipa$periodo_dia <- as.factor(datos_arequipa$periodo_dia)
datos_arequipa$año <- as.factor(datos_arequipa$año)
datos_arequipa$nombre_dia <- as.factor(datos_arequipa$nombre_dia)

# Verificar la conversión
cat("\nEstructura de las variables:\n")
str(datos_arequipa)

# Estadísticas descriptivas básicas
cat("\n--- ESTADÍSTICAS DESCRIPTIVAS AREQUIPA ---\n")
cat("Variable dependiente: num_fallecidos\n")
summary(datos_arequipa$num_fallecidos)

# Análisis de ceros
ceros_aqp <- sum(datos_arequipa$num_fallecidos == 0)
total_aqp <- nrow(datos_arequipa)
porc_ceros_aqp <- (ceros_aqp / total_aqp) * 100

cat("Ceros:", ceros_aqp, "(", round(porc_ceros_aqp, 2), "%)\n")
cat("No ceros:", total_aqp - ceros_aqp, "(", round(100 - porc_ceros_aqp, 2), "%)\n")

# Ratio varianza/media (indicador de sobredispersión)
media_aqp <- mean(datos_arequipa$num_fallecidos)
varianza_aqp <- var(datos_arequipa$num_fallecidos)
ratio_var_media_aqp <- varianza_aqp / media_aqp

cat("Media:", round(media_aqp, 3), "\n")
cat("Varianza:", round(varianza_aqp, 3), "\n")
cat("Ratio Var/Media:", round(ratio_var_media_aqp, 3), "\n")

if(ratio_var_media_aqp > 1) {
  cat("-> Hay sobredispersión (ratio > 1)\n")
} else {
  cat("-> No hay sobredispersión aparente\n")
}

# =============================================================================
# 2. COMPARACIÓN AREQUIPA vs LIMA
# =============================================================================

cat("\n--- COMPARACIÓN PRELIMINAR AREQUIPA vs LIMA ---\n")
cat("AREQUIPA:\n")
cat("  • Observaciones:", total_aqp, "\n")
cat("  • Media fallecidos:", round(media_aqp, 3), "\n")
cat("  • % Ceros:", round(porc_ceros_aqp, 2), "%\n")
cat("  • Ratio Var/Media:", round(ratio_var_media_aqp, 3), "\n")

cat("\nLIMA (para comparación):\n")
cat("  • Observaciones: 1553\n")
cat("  • Media fallecidos: 0.131\n")
cat("  • % Ceros: 90.15%\n")
cat("  • Ratio Var/Media: 6.292\n")

# =============================================================================
# 3. VISUALIZACIÓN EXPLORATORIA AREQUIPA
# =============================================================================

cat("\n--- CREANDO GRÁFICOS EXPLORATORIOS AREQUIPA ---\n")

# Histograma de fallecidos
p1_aqp <- ggplot(datos_arequipa, aes(x = num_fallecidos)) +
  geom_histogram(binwidth = 1, fill = "darkgreen", alpha = 0.7, color = "black") +
  labs(title = "Distribución de Fallecidos - Arequipa", 
       x = "Número de Fallecidos", 
       y = "Frecuencia") +
  theme_minimal()

print(p1_aqp)

# Boxplot por tipo de accidente  
p2_aqp <- ggplot(datos_arequipa, aes(x = tipo_accidente, y = num_fallecidos)) +
  geom_boxplot(fill = "lightgreen", alpha = 0.7) +
  labs(title = "Fallecidos por Tipo de Accidente - Arequipa",
       x = "Tipo de Accidente",
       y = "Número de Fallecidos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p2_aqp)

# =============================================================================
# 4. MODELAMIENTO ESTADÍSTICO AREQUIPA
# =============================================================================

cat("\n=======================================================\n")
cat("AJUSTE DE LOS 4 MODELOS - AREQUIPA\n")
cat("=======================================================\n")

# Modelo 1: POISSON SIMPLE
cat("\n--- MODELO 1: POISSON SIMPLE (AREQUIPA) ---\n")
modelo_poisson_aqp <- glm(num_fallecidos ~ tipo_accidente + periodo_dia, 
                          data = datos_arequipa, 
                          family = poisson(link = "log"))

cat("✓ Modelo Poisson ajustado\n")
cat("AIC:", AIC(modelo_poisson_aqp), "\n")
cat("Deviance:", deviance(modelo_poisson_aqp), "\n")

# Modelo 2: BINOMIAL NEGATIVA
cat("\n--- MODELO 2: BINOMIAL NEGATIVA (AREQUIPA) ---\n")
modelo_nb_aqp <- glm.nb(num_fallecidos ~ tipo_accidente + periodo_dia, 
                        data = datos_arequipa)

cat("✓ Modelo Binomial Negativa ajustado\n")
cat("AIC:", AIC(modelo_nb_aqp), "\n")
cat("Theta (parámetro de dispersión):", modelo_nb_aqp$theta, "\n")

# Modelo 3: ZERO-INFLATED POISSON (ZIP)
cat("\n--- MODELO 3: ZERO-INFLATED POISSON (AREQUIPA) ---\n")
modelo_zip_aqp <- zeroinfl(num_fallecidos ~ tipo_accidente + periodo_dia | 1, 
                           data = datos_arequipa, 
                           dist = "poisson")

cat("✓ Modelo ZIP ajustado\n")
cat("AIC:", AIC(modelo_zip_aqp), "\n")
cat("Log-likelihood:", logLik(modelo_zip_aqp), "\n")

# Modelo 4: ZERO-INFLATED NEGATIVE BINOMIAL (ZINB)
cat("\n--- MODELO 4: ZERO-INFLATED NEGATIVE BINOMIAL (AREQUIPA) ---\n")
modelo_zinb_aqp <- zeroinfl(num_fallecidos ~ tipo_accidente + periodo_dia | 1, 
                            data = datos_arequipa, 
                            dist = "negbin")

cat("✓ Modelo ZINB ajustado\n")
cat("AIC:", AIC(modelo_zinb_aqp), "\n")
cat("Log-likelihood:", logLik(modelo_zinb_aqp), "\n")

# =============================================================================
# 5. COMPARACIÓN DE MODELOS AREQUIPA
# =============================================================================

cat("\n=======================================================\n")
cat("COMPARACIÓN DE MODELOS - AREQUIPA\n")
cat("=======================================================\n")

# Tabla comparativa de AIC/BIC
modelos_aqp <- list("Poisson" = modelo_poisson_aqp,
                    "Binomial_Negativa" = modelo_nb_aqp,
                    "ZIP" = modelo_zip_aqp,
                    "ZINB" = modelo_zinb_aqp)

# Crear tabla de comparación
comparacion_aqp <- data.frame(
  Modelo = names(modelos_aqp),
  AIC = sapply(modelos_aqp, AIC),
  BIC = sapply(modelos_aqp, BIC),
  LogLik = sapply(modelos_aqp, function(x) as.numeric(logLik(x))),
  df = sapply(modelos_aqp, function(x) attr(logLik(x), "df"))
)

# Ordenar por AIC (menor es mejor)
comparacion_aqp <- comparacion_aqp[order(comparacion_aqp$AIC), ]
rownames(comparacion_aqp) <- NULL

cat("\nTabla de Comparación AREQUIPA (ordenada por AIC):\n")
print(comparacion_aqp)

# Identificar el mejor modelo
mejor_modelo_aqp <- comparacion_aqp$Modelo[1]
cat("\n🏆 MEJOR MODELO AREQUIPA:", mejor_modelo_aqp, "\n")
cat("   AIC:", comparacion_aqp$AIC[1], "\n")

# =============================================================================
# 6. COMPARACIÓN FINAL LIMA vs AREQUIPA
# =============================================================================

cat("\n=======================================================\n")
cat("COMPARACIÓN FINAL: LIMA vs AREQUIPA\n")
cat("=======================================================\n")

# Resultados de Lima (recordatorio)
cat("\n🏆 GANADORES POR DEPARTAMENTO:\n")
cat("LIMA: Binomial Negativa (AIC = 1164.54)\n")
cat("AREQUIPA:", mejor_modelo_aqp, "(AIC =", comparacion_aqp$AIC[1], ")\n")

# Estadísticas comparativas
cat("\n📊 ESTADÍSTICAS COMPARATIVAS:\n")
cat("                    LIMA      AREQUIPA\n")
cat("Observaciones:      1553     ", total_aqp, "\n")
cat("Media fallecidos:   0.131    ", round(media_aqp, 3), "\n")
cat("% Ceros:            90.15%   ", round(porc_ceros_aqp, 2), "%\n")
cat("Ratio Var/Media:    6.292    ", round(ratio_var_media_aqp, 3), "\n")

# =============================================================================
# 7. INTERPRETACIÓN DEL MEJOR MODELO AREQUIPA
# =============================================================================

cat("\n=======================================================\n")
cat("INTERPRETACIÓN DEL MEJOR MODELO - AREQUIPA\n")
cat("=======================================================\n")

# Mostrar resumen del mejor modelo de Arequipa
if(mejor_modelo_aqp == "ZIP") {
  cat("\nResumen del modelo ZIP (Arequipa):\n")
  print(summary(modelo_zip_aqp))
} else if(mejor_modelo_aqp == "ZINB") {
  cat("\nResumen del modelo ZINB (Arequipa):\n")
  print(summary(modelo_zinb_aqp))
} else if(mejor_modelo_aqp == "Binomial_Negativa") {
  cat("\nResumen del modelo Binomial Negativa (Arequipa):\n")
  print(summary(modelo_nb_aqp))
} else {
  cat("\nResumen del modelo Poisson (Arequipa):\n")
  print(summary(modelo_poisson_aqp))
}

# =============================================================================
# 8. TESTS ESTADÍSTICOS AREQUIPA
# =============================================================================

cat("\n=======================================================\n")
cat("TESTS ESTADÍSTICOS - AREQUIPA\n")
cat("=======================================================\n")

# Test de Vuong (ZIP vs Poisson)
cat("\n--- Test de Vuong (ZIP vs Poisson) - AREQUIPA ---\n")
vuong_test_aqp <- vuong(modelo_zip_aqp, modelo_poisson_aqp)
print(vuong_test_aqp)

# Test de sobredispersión
cat("\n--- Test de Sobredispersión - AREQUIPA ---\n")
dispersiontest(modelo_poisson_aqp)

cat("\n=======================================================\n")
cat("✓ ANÁLISIS COMPLETADO - AREQUIPA\n")
cat("✓ Comparación Lima vs Arequipa lista para tesis\n")
cat("✓ Conclusiones finales disponibles\n")
cat("=======================================================\n")