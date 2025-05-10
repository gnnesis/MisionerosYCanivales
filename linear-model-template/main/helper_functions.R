# -----------------------------------------------------------------------------
# Funciones Auxiliares
# -----------------------------------------------------------------------------

# Función para reemplazar los NAs con el valor más frecuente (moda) en cada columna
na_mode_fill <- function(df) {
  for (col in names(df)) {
    if (any(is.na(df[[col]]) | df[[col]] == "")) {
      moda <- names(sort(table(df[[col]]), decreasing = TRUE))[1]
      if (is.numeric(df[[col]])) moda <- as.numeric(moda)
      df[[col]][is.na(df[[col]]) | df[[col]] == ""] <- moda
    }
  }
  return(df)
}

# Función para dividir el conjunto de datos en entrenamiento y prueba
train_test_split <- function(df, split = 0.75, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  idx <- sample(seq_len(nrow(df)), size = floor(split * nrow(df)))
  list(train = df[idx, , drop = FALSE], test = df[-idx, , drop = FALSE])
}

# Función para calcular las métricas de regresión (MAE, MSE, MAPE)
regression_metrics <- function(y_true, y_pred) {
  mae  <- mean(abs(y_true - y_pred))
  mse  <- mean((y_true - y_pred)^2)
  mape <- mean(abs((y_true - y_pred) / y_true)) * 100
  return(c(MAE = mae, MSE = mse, MAPE = mape))
}

# Función para validación cruzada de modelos de regresión lineal
cross_validate_lm <- function(formula, data, target, k = 10, split = 0.75) {
  results <- data.frame(Fold = integer(k), MAE = numeric(k), MSE = numeric(k), MAPE = numeric(k))
  models  <- vector("list", k)
  
  for (i in seq_len(k)) {
    ss <- train_test_split(data, split, seed = i)
    model <- lm(formula, data = ss$train)
    pred <- predict(model, ss$test)
    
    results[i, 1] <- i
    results[i, 2:4] <- regression_metrics(ss$test[[target]], pred)
    models[[i]] <- model
  }
  
  best_idx <- which.min(results$MAE)
  list(best_model = models[[best_idx]], results_df = results, models = models)
}

# Función para optimización de presupuesto con regresión lineal
optimize_budget_linear <- function(model, channels = c("TV", "Radio", "Newspaper"), 
                                   budget_total = 50, step = 5, min_each = 5) {
  grid_vals <- seq(min_each, budget_total, by = step)
  combos <- expand.grid(rep(list(grid_vals), length(channels)))
  names(combos) <- channels
  combos <- combos[rowSums(combos) == budget_total, , drop = FALSE]
  
  coef <- coef(model)
  combos$Sales_Pred <- as.numeric(coef[1] + as.matrix(combos) %*% coef[-1])
  combos[order(-combos$Sales_Pred), ]
}
