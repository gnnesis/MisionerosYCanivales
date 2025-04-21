random.restart.hill.climbing <- function(file,
                                         restarts,
                                         max_iterations = 50,
                                         count_print    = 10,
                                         trace          = FALSE) {
  name_method <- "Random Restart Hill Climbing"
  best_result <- NULL
  best_eval   <- Inf
  
  # Tiempo global de inicio
  start_time <- Sys.time()
  
  for (i in seq_len(restarts)) {
    # 1) Reinicializar problema desde el fichero
    problem <- initialize.problem(file)
    
    # 2) Generar un estado inicial aleatorio:
    #    Cada antena toma aleatoriamente la posición de uno de los puntos
    pts   <- problem$positions
    n_ant <- problem$num_antennas
    sel   <- sample(nrow(pts), n_ant, replace = TRUE)
    # crear vector [x1,y1, x2,y2, ..., xn,yn]
    rand_state <- as.vector(t(pts[sel, ]))
    
    # 3) Asignar al campo que hill.climbing.search consume
    problem$state_initial <- rand_state
    
    # 4) Ejecutar Hill‑Climbing básico
    result_i <- hill.climbing.search(problem,
                                     max_iterations = max_iterations,
                                     count_print    = count_print,
                                     trace          = trace)
    
    # 5) Evaluar y quedarnos con el mejor
    eval_i <- result_i$state_final$evaluation
    if (eval_i < best_eval) {
      best_eval   <- eval_i
      best_result <- result_i
    }
  }
  
  # Tiempo global de fin
  end_time <- Sys.time()
  
  # Construir y devolver la salida con los cuatro campos requeridos
  list(
    name        = name_method,
    runtime     = end_time - start_time,
    state_final = best_result$state_final,
    report      = best_result$report
  )
}
