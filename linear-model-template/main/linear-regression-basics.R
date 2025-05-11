# -----------------------------------------------------------------------------
# Cargar librerías necesarias
# -----------------------------------------------------------------------------
library(lattice)
library(caret)
library(ggplot2)
library(gridExtra)
library(kableExtra)
library(RKEEL)

# -----------------------------------------------------------------------------
# Cargar funciones auxiliares
# -----------------------------------------------------------------------------
source("helper_functions.R")  # Asegúrate de que este archivo esté en el mismo directorio

# -----------------------------------------------------------------------------
# Código principal
# -----------------------------------------------------------------------------

# Configurar directorio de trabajo (si es necesario)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  # Ajusta el directorio de trabajo al lugar donde está el archivo

# Cargar y preprocesar datos
adv <- read.csv("../data/2025_Advertising.csv")  # Ruta relativa al archivo CSV
adv <- na_mode_fill(adv)  # Usar la función definida en helper_functions.R

# -----------------------------------------------------------------------------
# Gráfico de Regresión Lineal
# -----------------------------------------------------------------------------

# Generar gráfico de dispersión con línea de regresión
ggplot(adv, aes(x = TV, y = Sales)) +
  geom_point(color = "blue") +  # Añadir los puntos
  geom_smooth(method = "lm", color = "red") +  # Línea de regresión
  labs(title = "Relación entre TV y Sales", x = "TV", y = "Sales")

# -----------------------------------------------------------------------------
# Validación cruzada y entrenamiento de modelos
# -----------------------------------------------------------------------------

# Ejecutar validación cruzada para encontrar el mejor modelo
cv <- cross_validate_lm(Sales ~ TV + Radio + Newspaper, data = adv, target = "Sales", k = 10)

# Mostrar los resultados de la validación cruzada (10 modelos)
print(cv$results_df)

# Obtener el mejor modelo (el que tiene el menor MAE)
best_model <- cv$best_model
cat("\n>>> Mejor Modelo (MAE más bajo)\n")
summary(best_model)

# -----------------------------------------------------------------------------
# Identificar la fila con mayor error absoluto
# -----------------------------------------------------------------------------

# Predicciones del modelo
pred <- predict(best_model, newdata = adv)

# Calcular los residuos (errores)
res <- adv$Sales - pred

# Identificar el índice con el mayor error absoluto
worst_idx <- which.max(abs(res))
cat("\nCampaña peor explicada: fila", worst_idx, "(error absoluto =", round(abs(res[worst_idx]), 2), ")\n")

# -----------------------------------------------------------------------------
# Optimización de presupuesto (para un presupuesto de $50,000)
# -----------------------------------------------------------------------------

# Optimizar la asignación del presupuesto de $50,000 entre los canales
opt <- optimize_budget_linear(best_model, channels = c("TV", "Radio", "Newspaper"), budget_total = 50, step = 5, min_each = 5)

# Mostrar la mejor asignación del presupuesto
cat("\nAsignación óptima de 50k$ (en miles):\n")
print(head(opt, 1))

# -----------------------------------------------------------------------------
# Identificación de campañas con más beneficio (ventas/inversión)
# -----------------------------------------------------------------------------

# Calcular beneficio como ventas / inversión total (TV + Radio + Newspaper)
adv$Total_Invest <- adv$TV + adv$Radio + adv$Newspaper
adv$Beneficio <- adv$Sales / adv$Total_Invest

# Obtener las 10 campañas con mayor beneficio
top_beneficio <- adv[order(-adv$Beneficio), ]
cat("\n>>> Top 10 campañas con más beneficio (ventas/inversión):\n")
print(head(top_beneficio[, c("TV", "Radio", "Newspaper", "Sales", "Total_Invest", "Beneficio")], 10))

# -----------------------------------------------------------------------------
# Identificación de campañas más sorprendentes (mayor error positivo y negativo)
# -----------------------------------------------------------------------------

# Ya tenemos 'pred' como predicciones y 'res' como errores reales

# Añadir errores a los datos
adv$Pred <- pred
adv$Error <- adv$Sales - pred

# Campañas con mayor error positivo (valor real muy superior a la predicción)
top_pos_diff <- adv[order(-adv$Error), ]
cat("\n>>> Top 5 campañas con mayor diferencia positiva (real > predicción):\n")
print(head(top_pos_diff[, c("TV", "Radio", "Newspaper", "Sales", "Pred", "Error")], 5))

# Campañas con mayor error negativo (valor real mucho menor que la predicción)
top_neg_diff <- adv[order(adv$Error), ]
cat("\n>>> Top 5 campañas con mayor diferencia negativa (real < predicción):\n")
print(head(top_neg_diff[, c("TV", "Radio", "Newspaper", "Sales", "Pred", "Error")], 5))
