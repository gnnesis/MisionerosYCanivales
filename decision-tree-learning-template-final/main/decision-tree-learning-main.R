# ── Cargar librerías ──────────────────────────────────────────────────────
library(tidyverse)
library(caret)
library(rpart)
library(rpart.plot)
library(glue)
library(purrr)

# ── Cargar y preparar los datos ───────────────────────────────────────────
loans <- read.csv("../data/2025_loan_approval_dataset.csv",
                  sep = ",", header = TRUE, stringsAsFactors = TRUE) %>% 
  select(-loan_id)  # quitar ID

## Imputar NA / vacíos
for (col in colnames(loans)) {
  if (is.factor(loans[[col]]) | is.character(loans[[col]])) {
    moda <- names(sort(table(loans[[col]]), decreasing = TRUE))[1]
    loans[[col]][is.na(loans[[col]]) | loans[[col]] == ""] <- moda
  } else {
    loans[[col]][is.na(loans[[col]])] <- median(loans[[col]], na.rm = TRUE)
  }
}

# ── Visión rápida de los datos ────────────────────────────────────────────
knitr::kable(head(loans), caption = "Un vistazo de los datos")

# ── Contenedores para resultados y modelos ────────────────────────────────
results <- tibble(
  Overall_Accuracy           = numeric(),
  Accuracy_Yes               = numeric(),
  Accuracy_No                = numeric(),
  Accuracy_Graduate          = numeric(),
  Accuracy_Not_Graduate      = numeric(),
  Accuracy_Self_Employed     = numeric(),
  Accuracy_Not_Self_Employed = numeric(),
  Iteration                  = character()   # carácter para evitar problemas de tipo
)
model_list <- vector("list", 10)
acc_vec    <- numeric(10)

set.seed(123)  # reproducibilidad

# ── Entrenamiento y evaluación (10 repeticiones) ──────────────────────────
for (i in 1:10) {
  idx   <- createDataPartition(loans$loan_status, p = .75, list = FALSE)
  train <- loans[idx, ]
  test  <- loans[-idx, ]
  
  control <- rpart.control(maxdepth = 5)
  model   <- rpart(loan_status ~ ., data = train, method = "class",
                   control = control)
  
  pred <- predict(model, test, type = "class")
  mats <- confusionMatrix(pred, test$loan_status)
  
  # Métricas globales
  overall_acc <- mats$overall["Accuracy"]
  acc_yes     <- mats$byClass["Sensitivity"]
  acc_no      <- mats$byClass["Specificity"]
  
  # Métricas por subgrupos
  grad_rows    <- test$education == "Graduate"
  notgrad_rows <- test$education == "Not Graduate"
  selfyes_rows <- test$self_employed == "Yes"
  selfno_rows  <- test$self_employed == "No"
  
  acc_grad        <- ifelse(any(grad_rows),
                            mean(pred[grad_rows] == test$loan_status[grad_rows]),  NA)
  acc_notgrad     <- ifelse(any(notgrad_rows),
                            mean(pred[notgrad_rows] == test$loan_status[notgrad_rows]), NA)
  acc_self_yes    <- ifelse(any(selfyes_rows),
                            mean(pred[selfyes_rows] == test$loan_status[selfyes_rows]), NA)
  acc_self_no     <- ifelse(any(selfno_rows),
                            mean(pred[selfno_rows] == test$loan_status[selfno_rows]),  NA)
  
  # Guardar métricas y modelo
  results <- add_row(
    results,
    Overall_Accuracy           = overall_acc,
    Accuracy_Yes               = acc_yes,
    Accuracy_No                = acc_no,
    Accuracy_Graduate          = acc_grad,
    Accuracy_Not_Graduate      = acc_notgrad,
    Accuracy_Self_Employed     = acc_self_yes,
    Accuracy_Not_Self_Employed = acc_self_no,
    Iteration                  = as.character(i)
  )
  model_list[[i]] <- model
  acc_vec[i]      <- overall_acc
  
  cat(glue("Iteración {i} | Precisión global: {round(100 * overall_acc, 2)} %\n"))
}

# ── Fila promedio sin advertencia de deprecación ─────────────────────────
avg <- results %>% 
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE))) %>% 
  mutate(Iteration = "Promedio")

results <- bind_rows(results, avg)

knitr::kable(results, caption = "Resultados de 10 ejecuciones y promedio")

# ── Seleccionar y mostrar el mejor árbol ─────────────────────────────────
best_idx   <- which.max(acc_vec)
best_model <- model_list[[best_idx]]

rpart.plot(best_model,
           main  = glue("Mejor árbol (iteración {best_idx})"),
           extra = 104)

# ── Importancia de variables del mejor modelo ────────────────────────────
importance <- best_model$variable.importance %>% 
  enframe(name = "Variable", value = "Importance") %>% 
  arrange(desc(Importance)) %>% 
  slice_head(n = 5)

knitr::kable(importance, caption = "Top-5 variables más importantes")

# ── Mostrar reglas del mejor árbol ─────────────────────────────────────────
rpart.rules(best_model, roundint = FALSE)

# ── Pregunta 1: % reales vs predichos por dependents (si existe) ──────────
if ("dependents" %in% colnames(loans)) {
  pred_all <- predict(best_model, loans, type = "class")
  df_dep   <- loans %>%
    mutate(pred = pred_all) %>%
    group_by(dependents) %>%
    summarise(
      pct_real_yes = mean(loan_status == "Y"),
      pct_pred_yes = mean(pred        == "Y")
    ) %>%
    pivot_longer(-dependents, names_to = "serie", values_to = "porc")
  
  ggplot(df_dep, aes(dependents, porc, fill = serie)) +
    geom_col(position = "dodge") +
    labs(title = "% reales vs predichos por dependents",
         y = "Porcentaje de aprobados")
} else {
  cat("La columna 'dependents' no existe en los datos, se omite pregunta 1.\n")
}

# ── Pregunta 2: Rechazados que aprobarían cambiando education a Graduate ────
rech <- loans %>% filter(loan_status == "N")
rech_grad <- rech %>% mutate(education = "Graduate")
aprueban <- predict(best_model, rech_grad, type = "class") == "Y"
cat("Rechazados que pasarían a aprobados cambiando education → Graduate: ",
    sum(aprueban), "\n")

# ── Pregunta 3: Ingreso mínimo necesario para aprobar (por rechazado) ─────
min_income <- function(fila) {
  inc <- fila$income_annum
  tope <- quantile(loans$income_annum, 0.99, na.rm = TRUE)  # límite razonable
  while (inc <= tope) {
    fila$income_annum <- inc
    prediccion <- predict(best_model, fila, type = "class")
    if (prediccion == "Y") return(inc)
    inc <- inc + 1000
  }
  NA_real_
}
rech$min_income_needed <- pmap_dbl(rech, min_income)

# ── Pregunta 4: Préstamo máximo que seguiría aprobado (por aprobado) ───────
max_loan <- function(fila) {
  amt <- fila$loan_amount
  techo <- quantile(loans$loan_amount, 0.99, na.rm = TRUE) * 2
  repeat {
    fila$loan_amount <- amt
    prediccion <- predict(best_model, fila, type = "class")
    if (prediccion == "N") return(amt - 1000)
    amt <- amt + 1000
    if (amt > techo) return(NA_real_)
  }
}
aprobados <- loans %>% filter(loan_status == "Y")
aprobados$max_loan_allowed <- pmap_dbl(aprobados, max_loan)

# ── Revisión rápida atributos (número de valores únicos) ──────────────────
unique_counts <- sapply(loans, function(x) length(unique(x)))
print(unique_counts)

