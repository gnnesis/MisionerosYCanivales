# File: stochastic-random-restart.R
# Implementación de Stochastic Hill‑Climbing con Random Restarts

stochastic.random.restart <- function(file,
                                      restarts,
                                      max_iterations = 50,
                                      count_print    = 10,
                                      trace          = FALSE) {
  name_method <- paste0("Stochastic Random Restart (restarts=", restarts, ")")
  best_result <- NULL
  best_eval   <- Inf
  
  # Tiempo global de inicio
  start_time <- Sys.time()
  
  for (i in seq_len(restarts)) {
    # 1) Reinicializar el problema desde el fichero
    problem <- initialize.problem(file)
    
    # 2) Generar un estado inicial aleatorio:
    pts   <- problem$positions
    n_ant <- problem$num_antennas
    sel   <- sample(nrow(pts), n_ant, replace = TRUE)
    problem$state_initial <- as.vector(t(pts[sel, ]))
    
    # 3) Invocar Stochastic Hill‑Climbing
    result_i <- stochastic.hill.climbing.search(problem,
                                                max_iterations = max_iterations,
                                                count_print    = count_print,
                                                trace          = trace)
    
    # 4) Comparar y conservar el mejor resultado
    eval_i <- result_i$state_final$evaluation
    if (eval_i < best_eval) {
      best_eval   <- eval_i
      best_result <- result_i
    }
  }
  
  # Tiempo global de fin
  end_time <- Sys.time()
  
  # Construir y devolver la lista con los cuatro campos requeridos
  list(
    name        = name_method,
    runtime     = end_time - start_time,
    state_final = best_result$state_final,
    report      = best_result$report
  )
}
