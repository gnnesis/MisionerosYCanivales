# --- Cargar librerías -----------------------------------------------------
library(tidyverse)
library(caret)
library(rpart)
library(rpart.plot)
library(glue)

# --- Cargar los datos -----------------------------------------------------
# Cargar el archivo CSV
loans <- read.csv(file="../data/2025_loan_approval_dataset.csv", sep=",", header = TRUE, stringsAsFactors = TRUE)

# Eliminar la columna 'loan_id'
loans <- loans %>% select(-loan_id)

# Imputación de valores faltantes (NA o espacios vacíos)
for(col in colnames(loans)) {
  if (is.factor(loans[[col]]) | is.character(loans[[col]])) {
    moda <- names(sort(table(loans[[col]]), decreasing=TRUE))[1]
    loans[[col]][is.na(loans[[col]]) | loans[[col]] == ""] <- moda
  } else {
    # Para variables numéricas: usar la mediana
    loans[[col]][is.na(loans[[col]])] <- median(loans[[col]], na.rm=TRUE)
  }
}

# --- Mostrar un vistazo de los datos -------------------------------------
knitr::kable(head(loans), caption = "Un vistazo de los datos")

# --- Crear un data frame para almacenar las métricas ---------------------
results <- data.frame(
  Overall_Accuracy = numeric(),
  Accuracy_Yes = numeric(),
  Accuracy_No = numeric(),
  Accuracy_Graduate = numeric(),
  Accuracy_Not_Graduate = numeric(),
  Accuracy_Self_Employed = numeric(),
  Accuracy_Not_Self_Employed = numeric()
)

set.seed(123)  # Para reproducibilidad

# --- Entrenamiento y evaluación del modelo --------------------------------
for (i in 1:10) {
  perc <- 0.75
  index.train <- createDataPartition(y = loans$loan_status, p = perc, list = FALSE)
  
  loans.train <- loans[index.train, ]
  loans.test  <- loans[-index.train, ]
  
  print(glue("Iteración {i}"))
  print(glue("Tamaño del conjunto de entrenamiento: {nrow(loans.train)}"))
  print(glue("Tamaño del conjunto de prueba: {nrow(loans.test)}"))
  
  # Crear el modelo de árbol con maxdepth = 5
  control <- rpart.control(maxdepth = 5)
  model <- rpart(formula = loan_status ~ ., data = loans.train, method = "class", control = control)
  
  # Predicción
  prediction <- predict(model, loans.test, type = "class")
  
  # Matriz de confusión
  mat <- confusionMatrix(prediction, loans.test$loan_status)
  overall_acc <- mat$overall["Accuracy"]
  
  # Precisión por clase
  acc_yes <- mat$byClass["Sensitivity"]  # Loan approved (Yes)
  acc_no <- mat$byClass["Specificity"]   # Loan not approved (No)
  
  # Accuracy para Graduate
  grad_rows <- loans.test$education == "Graduate"
  if (sum(grad_rows) > 0) {
    acc_grad <- mean(prediction[grad_rows] == loans.test$loan_status[grad_rows])
  } else {
    acc_grad <- NA
  }
  
  # Accuracy para Not Graduate
  notgrad_rows <- loans.test$education == "Not Graduate"
  if (sum(notgrad_rows) > 0) {
    acc_notgrad <- mean(prediction[notgrad_rows] == loans.test$loan_status[notgrad_rows])
  } else {
    acc_notgrad <- NA
  }
  
  # Accuracy para Self Employed = Yes
  selfemp_yes_rows <- loans.test$self_employed == "Yes"
  if (sum(selfemp_yes_rows) > 0) {
    acc_selfemp_yes <- mean(prediction[selfemp_yes_rows] == loans.test$loan_status[selfemp_yes_rows])
  } else {
    acc_selfemp_yes <- NA
  }
  
  # Accuracy para Self Employed = No
  selfemp_no_rows <- loans.test$self_employed == "No"
  if (sum(selfemp_no_rows) > 0) {
    acc_selfemp_no <- mean(prediction[selfemp_no_rows] == loans.test$loan_status[selfemp_no_rows])
  } else {
    acc_selfemp_no <- NA
  }
  
  # Guardar las métricas de la iteración
  results[i, ] <- c(overall_acc, acc_yes, acc_no, acc_grad, 1 - acc_grad, acc_selfemp_yes, 1 - acc_selfemp_no)
  
  print(glue("Precisión general: {round(100 * overall_acc, 2)} %"))
}

# --- Graficar el árbol generado -------------------------------------------
rpart.plot(model, main = "Árbol de decisión (última iteración)", extra = 104)

# --- Obtener la importancia de las variables -----------------------------
importance <- as.data.frame(model$variable.importance)
colnames(importance) <- "Importance"  # Renombrar columna
importance$Variable <- rownames(importance)

# Ordenar y mostrar las 5 variables más importantes
importance <- importance %>% arrange(desc(Importance)) %>% head(5)
knitr::kable(importance, caption = "Top 5 variables más importantes")

# --- Mostrar resultados de las 10 ejecuciones y su promedio --------------
results$Iteration <- 1:10
average_row <- colMeans(results[, -ncol(results)], na.rm = TRUE)
results <- rbind(results, c(average_row, Iteration = "Promedio"))
knitr::kable(results, caption = "Resultados de 10 ejecuciones y promedio")
