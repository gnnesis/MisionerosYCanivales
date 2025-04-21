stochastic.hill.climbing.search = function(problem,
                                           max_iterations = 50,
                                           count_print = 10,
                                           trace = FALSE) {
  
  name_method <- "Stochastic Hill Climbing Search"
  state_initial <- problem$state_initial
  actions_possible <- cbind(problem$actions.possible)
  
  print(paste0("* START: ", name_method), quote = F)
  start_time <- Sys.time()
  
  node_current <- list(parent = NULL,
                       state = state_initial,
                       actions = NULL,
                       depth = 1,
                       cost = get.cost(state = state_initial, problem = problem),
                       evaluation = get.evaluation(state_initial, problem))
  
  report <- data.frame(iteration = numeric(), depth_of_expanded = numeric())
  count <- 1
  end_reason <- 0
  
  while (count <= max_iterations) {
    if (count %% count_print == 0 || trace) {
      print(paste0("Iteration: ", count, ", evaluation=", node_current$evaluation), quote = FALSE)
      if (trace) to.string(state = node_current$state, problem = problem)
    }
    
    # Expand current node
    successor_nodes <- local.expand.node(node_current, actions_possible, problem)
    
    # Filter better successors (strictly better)
    better_successors <- Filter(function(n) n$evaluation < node_current$evaluation, successor_nodes)
    
    if (length(better_successors) == 0) {
      end_reason <- "Local_Best"
      break
    }
    
    # Select one randomly among better successors
    node_current <- better_successors[[sample(length(better_successors), 1)]]
    
    report <- rbind(report, data.frame(iteration = count,
                                       depth_of_expanded = node_current$depth))
    count <- count + 1
  }
  
  end_time <- Sys.time()
  
  result <- list()
  result$name <- name_method
  result$runtime <- end_time - start_time
  result$state_final <- node_current
  result$report <- report
  
  if (end_reason == "Local_Best") {
    print("Local best found!!", quote = FALSE)
  } else {
    print(paste0("Maximum iterations reached: ", max_iterations), quote = FALSE)
  }
  
  print(paste0("Final State: ", to.string(state = node_current$state, problem = problem)), quote = FALSE)
  print(paste0("* END: ", name_method), quote = F)
  
  return(result)
}
