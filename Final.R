# SCRIPT DE CULMINACIÓN - TABLAS Y GRÁFICOS FINALES PARA TESIS
# Comparación completa Lima vs Arequipa - Modelos Zero-Inflated

# Cargar librerías necesarias
library(ggplot2)
library(dplyr)
library(gridExtra)
library(knitr)
library(readxl)

cat("=======================================================\n")
cat("SCRIPT DE CULMINACIÓN - ANÁLISIS FINAL\n")
cat("TABLAS Y GRÁFICOS PARA TESIS DE BACHILLER\n")
cat("=======================================================\n")

# =============================================================================
# 1. RECOPILAR RESULTADOS DE AMBOS DEPARTAMENTOS
# =============================================================================

# IMPORTANTE: Estos datos deben actualizarse con los resultados reales obtenidos
# Resultados de LIMA (ya conocidos)
resultados_lima <- data.frame(
  Departamento = "LIMA",
  Modelo = c("Binomial_Negativa", "ZINB", "ZIP", "Poisson"),
  AIC = c(1164.54, 1166.54, 1309.54, 1348.95),
  BIC = c(1218.02, 1225.37, 1363.02, 1397.08),
  LogLik = c(-572.27, -572.27, -644.77, -665.47),
  Ranking = c(1, 2, 3, 4)
)

# RESULTADOS DE AREQUIPA (datos reales obtenidos)
resultados_arequipa <- data.frame(
  Departamento = "AREQUIPA",
  Modelo = c("Binomial_Negativa", "ZINB", "ZIP", "Poisson"),
  AIC = c(689.06, 691.06, 736.35, 839.48),
  BIC = c(735.81, 742.48, 783.09, 881.55),
  LogLik = c(-334.53, -334.53, -358.17, -410.74),
  Ranking = c(1, 2, 3, 4)
)

# Mensaje importante
cat("\n⚠️  IMPORTANTE: Actualiza 'resultados_arequipa' con tus datos reales antes de ejecutar\n")
cat("Los valores actuales son solo ejemplos\n\n")

# Combinar resultados
resultados_completos <- rbind(resultados_lima, resultados_arequipa)

# =============================================================================
# 2. TABLA RESUMEN FINAL
# =============================================================================

cat("=== TABLA RESUMEN FINAL ===\n")

# Tabla completa con todos los modelos
tabla_final <- resultados_completos %>%
  arrange(Departamento, Ranking) %>%
  select(Departamento, Modelo, AIC, BIC, LogLik, Ranking)

print(knitr::kable(tabla_final, 
                   caption = "Comparación de Modelos por Departamento",
                   digits = 3,
                   col.names = c("Departamento", "Modelo", "AIC", "BIC", "Log-Likelihood", "Ranking")))

# Tabla de ganadores por departamento
ganadores <- resultados_completos %>%
  filter(Ranking == 1) %>%
  select(Departamento, Modelo, AIC, BIC)

cat("\n🏆 MODELOS GANADORES POR DEPARTAMENTO:\n")
print(knitr::kable(ganadores,
                   caption = "Mejores Modelos por Departamento",
                   digits = 3,
                   col.names = c("Departamento", "Mejor Modelo", "AIC", "BIC")))

# =============================================================================
# 3. GRÁFICO COMPARATIVO DE AIC
# =============================================================================

cat("\n=== CREANDO GRÁFICOS COMPARATIVOS ===\n")

# Gráfico de barras comparativo AIC
p1 <- ggplot(resultados_completos, aes(x = Modelo, y = AIC, fill = Departamento)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  geom_text(aes(label = round(AIC, 1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3, size = 3) +
  scale_fill_manual(values = c("LIMA" = "#2E86AB", "AREQUIPA" = "#A23B72")) +
  labs(title = "Comparación de AIC por Modelo y Departamento",
       subtitle = "Menor AIC = Mejor Modelo",
       x = "Modelo Estadístico",
       y = "Criterio de Información de Akaike (AIC)",
       fill = "Departamento") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12)) +
  scale_y_continuous(breaks = seq(0, max(resultados_completos$AIC) + 100, 200))

print(p1)

# Gráfico de líneas para mostrar ranking
p2 <- ggplot(resultados_completos, aes(x = Modelo, y = AIC, color = Departamento, group = Departamento)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 4, alpha = 0.9) +
  geom_text(aes(label = paste("Rank", Ranking)), 
            vjust = -1, size = 3) +
  scale_color_manual(values = c("LIMA" = "#2E86AB", "AREQUIPA" = "#A23B72")) +
  labs(title = "Evolución del AIC por Modelo",
       subtitle = "Comparación Lima vs Arequipa",
       x = "Modelo Estadístico",
       y = "AIC",
       color = "Departamento") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12))

print(p2)

# =============================================================================
# 4. ESTADÍSTICAS DESCRIPTIVAS COMPARATIVAS
# =============================================================================

cat("\n=== ESTADÍSTICAS DESCRIPTIVAS COMPARATIVAS ===\n")

# Cargar estadísticas de ambos departamentos (actualizar con datos reales)
estadisticas_comparativas <- data.frame(
  Métrica = c("Observaciones", "Media_fallecidos", "Porcentaje_ceros", 
              "Ratio_Var_Media", "Máximo_fallecidos", "Mejor_Modelo"),
  LIMA = c(1553, 0.131, 90.15, 6.292, 33, "Binomial_Negativa"),
  AREQUIPA = c(792, 0.174, 89.39, 3.932, 16, "Binomial_Negativa")
)

print(knitr::kable(estadisticas_comparativas,
                   caption = "Estadísticas Descriptivas Comparativas",
                   col.names = c("Métrica", "Lima", "Arequipa")))

cat("\n⚠️  Actualizar fila AREQUIPA con datos reales\n")

# =============================================================================
# 5. GRÁFICO DE EXCESO DE CEROS
# =============================================================================

# Datos para gráfico de ceros (datos reales)
datos_ceros <- data.frame(
  Departamento = c("LIMA", "AREQUIPA"),
  Ceros = c(90.15, 89.39),
  No_Ceros = c(9.85, 10.61)
)

# Transformar para ggplot
datos_ceros_long <- datos_ceros %>%
  tidyr::pivot_longer(cols = c(Ceros, No_Ceros), 
                      names_to = "Categoria", 
                      values_to = "Porcentaje")

p3 <- ggplot(datos_ceros_long, aes(x = Departamento, y = Porcentaje, fill = Categoria)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white", fontweight = "bold") +
  scale_fill_manual(values = c("Ceros" = "#E74C3C", "No_Ceros" = "#2ECC71")) +
  labs(title = "Distribución de Ceros vs No-Ceros por Departamento",
       subtitle = "Evidencia del Exceso de Ceros",
       x = "Departamento",
       y = "Porcentaje (%)",
       fill = "Categoría") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12))

print(p3)

# =============================================================================
# 6. CONCLUSIONES FINALES PARA LA TESIS
# =============================================================================

cat("\n=======================================================\n")
cat("CONCLUSIONES FINALES PARA LA TESIS\n")
cat("=======================================================\n")

cat("\n📊 HALLAZGOS PRINCIPALES:\n")
cat("1. EXCESO DE CEROS CONFIRMADO:\n")
cat("   • Lima: 90.15% de accidentes sin fallecidos\n")
cat("   • Arequipa: 89.39% de accidentes sin fallecidos\n")

cat("\n2. SOBREDISPERSIÓN DETECTADA:\n")
cat("   • Lima: Ratio Var/Media = 6.292 (severa)\n")
cat("   • Arequipa: Ratio Var/Media = 3.932 (moderada)\n")

cat("\n3. MODELOS GANADORES:\n")
cat("   • Lima: Binomial Negativa (AIC = 1164.54)\n")
cat("   • Arequipa: Binomial Negativa (AIC = 689.06)\n")
cat("   • PATRÓN CONSISTENTE: Mismo modelo ganador\n")

cat("\n4. IMPLICACIONES METODOLÓGICAS:\n")
cat("   • Los modelos Zero-Inflated NO fueron superiores\n")
cat("   • La Binomial Negativa maneja mejor la sobredispersión\n")
cat("   • El modelo Poisson simple es completamente inadecuado\n")

cat("\n5. DIFERENCIAS GEOGRÁFICAS:\n")
cat("   • Arequipa es MÁS MORTAL: 0.174 vs 0.131 fallecidos promedio\n")
cat("   • Arequipa tiene MENOS sobredispersión que Lima\n")
cat("   • Ambos con ~90% de ceros (exceso similar)\n")
cat("   • Lima: muestra más grande (1553 vs 792)\n")

# =============================================================================
# 7. EXPORTAR RESULTADOS
# =============================================================================

cat("\n=== EXPORTANDO RESULTADOS FINALES ===\n")

# Guardar gráficos
ggsave("Comparacion_AIC_Modelos.png", plot = p1, width = 12, height = 8, dpi = 300)
ggsave("Evolucion_AIC_Departamentos.png", plot = p2, width = 12, height = 8, dpi = 300)
ggsave("Distribucion_Ceros_Departamentos.png", plot = p3, width = 10, height = 6, dpi = 300)

# Exportar tabla final a CSV
write.csv(tabla_final, "Tabla_Comparacion_Final.csv", row.names = FALSE)
write.csv(ganadores, "Modelos_Ganadores.csv", row.names = FALSE)

cat("\n✅ GRÁFICOS Y TABLAS EXPORTADOS:\n")
cat("   • Comparacion_AIC_Modelos.png\n")
cat("   • Evolucion_AIC_Departamentos.png\n")
cat("   • Distribucion_Ceros_Departamentos.png\n")
cat("   • Tabla_Comparacion_Final.csv\n")
cat("   • Modelos_Ganadores.csv\n")

cat("\n=======================================================\n")
cat("✅ SCRIPT DE CULMINACIÓN COMPLETADO\n")
cat("✅ TABLAS Y GRÁFICOS LISTOS PARA TESIS\n")
cat("✅ RESULTADOS FINALES DISPONIBLES\n")
cat("=======================================================\n")

cat("\n🎓 PRÓXIMOS PASOS:\n")
cat("1. Actualizar datos de Arequipa con valores reales\n")
cat("2. Revisar conclusiones y adaptarlas a tus hallazgos\n")
cat("3. Incluir gráficos y tablas en documento de tesis\n")
cat("4. Redactar interpretación final\n")