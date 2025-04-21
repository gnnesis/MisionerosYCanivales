local.beam.search <- function(problem,
                              beams = 3,
                              max_iterations = 50,
                              count_print = 10,
                              trace = FALSE) {
  name_method      <- paste0("Beam Search (beams=", beams, ")")
  actions_possible <- cbind(problem$actions.possible)
  
  # Tiempo de inicio
  start_time <- Sys.time()
  
  # 1) Inicializar 'beams' nodos actuales con estados aleatorios
  nodes <- vector("list", beams)
  for (i in seq_len(beams)) {
    pts   <- problem$positions
    n_ant <- problem$num_antennas
    sel   <- sample(nrow(pts), n_ant, replace = TRUE)
    rand_state <- as.vector(t(pts[sel, ]))
    nodes[[i]] <- list(
      parent     = NULL,
      state      = rand_state,
      actions    = character(),
      depth      = 1,
      cost       = get.cost(state = rand_state, problem = problem),
      evaluation = get.evaluation(rand_state, problem)
    )
  }
  
  # Registro de la evolución del mejor beam
  report <- data.frame(iteration = integer(),
                       best_evaluation = numeric())
  
  # 2) Bucle principal de iteraciones
  for (count in seq_len(max_iterations)) {
    # Para cada nodo actual, expandir y seleccionar su mejor sucesor
    new_nodes <- vector("list", beams)
    for (i in seq_along(nodes)) {
      succs <- local.expand.node(nodes[[i]], actions_possible, problem)  # :contentReference[oaicite:0]{index=0}&#8203;:contentReference[oaicite:1]{index=1}
      # Orden ascendente por evaluación y tomar el primero
      succs <- succs[order(sapply(succs, function(x) x$evaluation))]
      new_nodes[[i]] <- succs[[1]]
    }
    nodes <- new_nodes
    
    # Registrar la mejor evaluación de esta iteración
    evals <- sapply(nodes, function(nd) nd$evaluation)
    best_iter_eval <- min(evals)
    report <- rbind(report,
                    data.frame(iteration = count,
                               best_evaluation = best_iter_eval))
    
    if (count %% count_print == 0) {
      cat("Iteración:", count,
          "Mejor evaluación =", best_iter_eval, "\n")
    }
  }
  
  # 3) Seleccionar el mejor nodo final
  end_time <- Sys.time()
  evals    <- sapply(nodes, function(nd) nd$evaluation)
  idx_best <- which.min(evals)
  best_node <- nodes[[idx_best]]
  
  # 4) Devolver resultado con los cuatro campos pedidos
  list(
    name        = name_method,
    runtime     = end_time - start_time,
    state_final = best_node,
    report      = report
  )
}
