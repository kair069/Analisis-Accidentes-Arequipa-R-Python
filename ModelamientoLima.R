
# MODELAMIENTO ZERO-INFLATED EN R - LIMA
# Análisis completo para tesis de bachiller

# =============================================================================
# 1. INSTALACIÓN Y CARGA DE LIBRERÍAS
# =============================================================================

# Instalar librerías (ejecutar solo una vez)
#install.packages(c("pscl", "MASS", "readxl", "ggplot2", "dplyr", "AER"))

# Cargar librerías
library(pscl)      # Para modelos zero-inflated (ZIP, ZINB)
library(MASS)      # Para binomial negativa
library(readxl)    # Para leer Excel
library(ggplot2)   # Para gráficos
library(dplyr)     # Para manipulación de datos
library(AER)       # Para tests estadísticos

cat("=======================================================\n")
cat("MODELAMIENTO ZERO-INFLATED EN R - LIMA\n")
cat("=======================================================\n")

# =============================================================================
# 2. CARGAR Y PREPARAR DATOS
# =============================================================================

# Cargar datos de Lima
datos_lima <- read_excel("Dataset_Lima_para_R.xlsx", sheet = "Datos_Lima")

cat("\n--- INFORMACIÓN DEL DATASET ---\n")
cat("Dimensiones:", dim(datos_lima), "\n")
cat("Variables:", names(datos_lima), "\n")

# Convertir variables categóricas a factores
datos_lima$tipo_accidente <- as.factor(datos_lima$tipo_accidente)
datos_lima$periodo_dia <- as.factor(datos_lima$periodo_dia)
datos_lima$año <- as.factor(datos_lima$año)
datos_lima$nombre_dia <- as.factor(datos_lima$nombre_dia)

# Verificar la conversión
cat("\nEstructura de las variables:\n")
str(datos_lima)

# Estadísticas descriptivas básicas
cat("\n--- ESTADÍSTICAS DESCRIPTIVAS ---\n")
cat("Variable dependiente: num_fallecidos\n")
summary(datos_lima$num_fallecidos)

# Análisis de ceros
ceros <- sum(datos_lima$num_fallecidos == 0)
total <- nrow(datos_lima)
porc_ceros <- (ceros / total) * 100

cat("Ceros:", ceros, "(", round(porc_ceros, 2), "%)\n")
cat("No ceros:", total - ceros, "(", round(100 - porc_ceros, 2), "%)\n")

# Ratio varianza/media (indicador de sobredispersión)
media <- mean(datos_lima$num_fallecidos)
varianza <- var(datos_lima$num_fallecidos)
ratio_var_media <- varianza / media

cat("Media:", round(media, 3), "\n")
cat("Varianza:", round(varianza, 3), "\n")
cat("Ratio Var/Media:", round(ratio_var_media, 3), "\n")

if(ratio_var_media > 1) {
  cat("-> Hay sobredispersión (ratio > 1)\n")
} else {
  cat("-> No hay sobredispersión aparente\n")
}


# =============================================================================
# 3. VISUALIZACIÓN EXPLORATORIA
# =============================================================================

cat("\n--- CREANDO GRÁFICOS EXPLORATORIOS ---\n")

# Histograma de fallecidos
p1 <- ggplot(datos_lima, aes(x = num_fallecidos)) +
  geom_histogram(binwidth = 1, fill = "steelblue", alpha = 0.7, color = "black") +
  labs(title = "Distribución de Fallecidos - Lima", 
       x = "Número de Fallecidos", 
       y = "Frecuencia") +
  theme_minimal()

print(p1)

# Boxplot por tipo de accidente  
p2 <- ggplot(datos_lima, aes(x = tipo_accidente, y = num_fallecidos)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  labs(title = "Fallecidos por Tipo de Accidente - Lima",
       x = "Tipo de Accidente",
       y = "Número de Fallecidos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p2)

# =============================================================================
# 4. MODELAMIENTO ESTADÍSTICO
# =============================================================================

cat("\n=======================================================\n")
cat("AJUSTE DE LOS 4 MODELOS\n")
cat("=======================================================\n")

# Modelo 1: POISSON SIMPLE
cat("\n--- MODELO 1: POISSON SIMPLE ---\n")
modelo_poisson <- glm(num_fallecidos ~ tipo_accidente + periodo_dia, 
                      data = datos_lima, 
                      family = poisson(link = "log"))

cat("✓ Modelo Poisson ajustado\n")
cat("AIC:", AIC(modelo_poisson), "\n")
cat("Deviance:", deviance(modelo_poisson), "\n")

# Modelo 2: BINOMIAL NEGATIVA
cat("\n--- MODELO 2: BINOMIAL NEGATIVA ---\n")
modelo_nb <- glm.nb(num_fallecidos ~ tipo_accidente + periodo_dia, 
                    data = datos_lima)

cat("✓ Modelo Binomial Negativa ajustado\n")
cat("AIC:", AIC(modelo_nb), "\n")
cat("Theta (parámetro de dispersión):", modelo_nb$theta, "\n")

# Modelo 3: ZERO-INFLATED POISSON (ZIP)
cat("\n--- MODELO 3: ZERO-INFLATED POISSON (ZIP) ---\n")
modelo_zip <- zeroinfl(num_fallecidos ~ tipo_accidente + periodo_dia | 1, 
                       data = datos_lima, 
                       dist = "poisson")

cat("✓ Modelo ZIP ajustado\n")
cat("AIC:", AIC(modelo_zip), "\n")
cat("Log-likelihood:", logLik(modelo_zip), "\n")

# Modelo 4: ZERO-INFLATED NEGATIVE BINOMIAL (ZINB)
cat("\n--- MODELO 4: ZERO-INFLATED NEGATIVE BINOMIAL (ZINB) ---\n")
modelo_zinb <- zeroinfl(num_fallecidos ~ tipo_accidente + periodo_dia | 1, 
                        data = datos_lima, 
                        dist = "negbin")

cat("✓ Modelo ZINB ajustado\n")
cat("AIC:", AIC(modelo_zinb), "\n")
cat("Log-likelihood:", logLik(modelo_zinb), "\n")



# =============================================================================
# 5. COMPARACIÓN DE MODELOS
# =============================================================================

cat("\n=======================================================\n")
cat("COMPARACIÓN DE MODELOS\n")
cat("=======================================================\n")

# Tabla comparativa de AIC/BIC
modelos <- list("Poisson" = modelo_poisson,
                "Binomial_Negativa" = modelo_nb,
                "ZIP" = modelo_zip,
                "ZINB" = modelo_zinb)

# Crear tabla de comparación
comparacion <- data.frame(
  Modelo = names(modelos),
  AIC = sapply(modelos, AIC),
  BIC = sapply(modelos, BIC),
  LogLik = sapply(modelos, function(x) as.numeric(logLik(x))),
  df = sapply(modelos, function(x) attr(logLik(x), "df"))
)

# Ordenar por AIC (menor es mejor)
comparacion <- comparacion[order(comparacion$AIC), ]
rownames(comparacion) <- NULL

cat("\nTabla de Comparación (ordenada por AIC):\n")
print(comparacion)

# Identificar el mejor modelo
mejor_modelo <- comparacion$Modelo[1]
cat("\n🏆 MEJOR MODELO:", mejor_modelo, "\n")
cat("   AIC:", comparacion$AIC[1], "\n")

# =============================================================================
# 6. INTERPRETACIÓN DEL MEJOR MODELO
# =============================================================================

cat("\n=======================================================\n")
cat("INTERPRETACIÓN DEL MEJOR MODELO\n")
cat("=======================================================\n")

# Mostrar resumen del mejor modelo
if(mejor_modelo == "ZIP") {
  cat("\nResumen del modelo ZIP:\n")
  print(summary(modelo_zip))
} else if(mejor_modelo == "ZINB") {
  cat("\nResumen del modelo ZINB:\n")
  print(summary(modelo_zinb))
} else if(mejor_modelo == "Binomial_Negativa") {
  cat("\nResumen del modelo Binomial Negativa:\n")
  print(summary(modelo_nb))
} else {
  cat("\nResumen del modelo Poisson:\n")
  print(summary(modelo_poisson))
}

# =============================================================================
# 7. TESTS ESTADÍSTICOS
# =============================================================================

cat("\n=======================================================\n")
cat("TESTS ESTADÍSTICOS\n")
cat("=======================================================\n")

# Test de Vuong (ZIP vs Poisson)
cat("\n--- Test de Vuong (ZIP vs Poisson) ---\n")
vuong_test <- vuong(modelo_zip, modelo_poisson)
print(vuong_test)

# Test de sobredispersión
cat("\n--- Test de Sobredispersión ---\n")
dispersiontest(modelo_poisson)

cat("\n=======================================================\n")
cat("✓ ANÁLISIS COMPLETADO - LIMA\n")
cat("✓ Resultados listos para interpretación en tesis\n")
cat("=======================================================\n")