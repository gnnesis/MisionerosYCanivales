a.star.search = function(problem,
                         max_iterations = 1000, 
                         count_print = 100, 
                         trace = FALSE,
                         graph_search = FALSE) {
  
  name_method      <- paste0("A* Search", ifelse(graph_search, " + GS", ""))
  state_initial    <- problem$state_initial
  state_final      <- problem$state_final
  actions_possible <- problem$actions_possible
  
  # Get Start time
  print(paste0("* START: ", name_method), quote = F)
  start_time       <- Sys.time()
  
  node <- list(parent = c(),
               state = state_initial,
               actions = c(),
               depth = 0,
               cost = 0,
               evaluation = 0)
  frontier <- list(node)
  
  if (graph_search) {
    expanded_nodes <- list()     
  }
  
  count <- 1
  end_reason <- 0
  
  #Initialization of information for further analysis
  report <- data.frame(iteration = numeric(),
                       nodes_frontier = numeric(),
                       depth_of_expanded = numeric(),
                       nodes_added_frontier = numeric())
  
  #Perform "max_iterations" iterations of the expansion process of the first node in the frontier list
  while (count <= max_iterations) {
    # Print a search trace for each "count_print" iteration
    if (count %% count_print == 0) {
      print(paste0("Iteration: ", count, ", Nodes in the frontier: ", length(frontier)), quote = F)
    }
    
    #If the frontier list remains empty, the algorithm ends without finding a solution
    if (length(frontier) == 0) {
      end_reason <- "Frontier"
      break
    }
    
    #Remove the first node of the frontier list
    node_first <- frontier[[1]]
    frontier[[1]] = NULL
    
    #If "trace" is on, the information of each node extracted from frontier is displayed
    if (trace) {
      print("------------------------------", quote = F)
      string_aux <- to.string(node_first$state)
      print(paste0("<- Extracted: ", string_aux, " / depth=", node_first$depth, ", cost=", node_first$depth), quote = FALSE)
    }
    
    #If the node extracted from frontier contains the final state
    #the algorithm ends because the solution has be founded
    if (is.final.state(node_first$state, state_final, problem)) {
      end_reason <- "Solution"
      
      #Add of information for further analysis
      report <- rbind(report,
                      data.frame(iteration = count,
                                 nodes_frontier = length(frontier),
                                 depth_of_expanded = node_first$depth,
                                 nodes_added_frontier = nodes_added_frontier))
      break
    }

    #The graph search stores the expanded states to check for repeated states
    if (graph_search) {
      expanded_nodes <- append(expanded_nodes, list(node_first))
    }
    
    nodes_added_frontier <- 0
    #The node extracted from frontier is expanded and its successor nodes are inserted into frontier
    successor_nodes <- expand.node(node_first, actions_possible, problem)
    
    if (length(successor_nodes) > 0) {
      # Update evaluation of each successor node to meet A* Search: f(n) = g(n) + h(n)
      for (i in 1:length(successor_nodes)) {
        successor_nodes[[i]]$evaluation <- successor_nodes[[i]]$cost + get.evaluation(successor_nodes[[i]]$state, problem)
      }
      
      #Graph Search implementation
      if (graph_search) {
        #Nodes that are not repeated are stores in a list
        not_repeated_nodes <- list()
        
        # For each successor node
        for (i in 1:length(successor_nodes)) {
          successor_node <- successor_nodes[[i]]
          #Check if the successor nodeo is on frontier or expanded
          if (!any(sapply(frontier, function (x) identical(x$state, successor_node$state))) &&
              !any(sapply(expanded_nodes, function (x) identical(x$state, successor_node$state)))) {
            not_repeated_nodes <- append(not_repeated_nodes, list(successor_node))
          } else {
            # Check if the successor node is already present in frontier or expanded
            lists <- check.repeated.state(node_first, successor_node, frontier, expanded_nodes, problem)
            # Update frontier and expanded
            frontier <- lists[[1]]
            expanded_nodes <- lists[[2]]
          }
        }
        
        #Successor nodes list is updated
        successor_nodes <- not_repeated_nodes
      } # Graph search

      #NOTE: Successor nodes are added at the back of the list
      frontier <- c(frontier, successor_nodes)
      
      nodes_added_frontier <- length(successor_nodes)
      
      #If "trace" is on, the information of each new node is displayed
      if (trace && length(successor_nodes) > 0) {
        for (i in 1:length(successor_nodes)) {
          string_aux <- to.string(successor_nodes[[i]]$state)
          print(paste0("-> Added: ", string_aux, " / depth=", successor_nodes[[i]]$depth, ", cost=", successor_nodes[[i]]$depth), quote = FALSE)
        }
      }
      
      #Frontier list is ordered according to EVALUATION
      frontier = frontier[order(sapply(frontier, function (x) x$evaluation))]
    } # length(successor_nodes) > 0
    
    if (trace) {
      print(paste0("<> Nodes in frontier: ", length(frontier)), quote = FALSE)
    }
    
    #Add of information for further analysis
    report <- rbind(report,
                    data.frame(iteration = count,
                               nodes_frontier = length(frontier),
                               depth_of_expanded = node_first$depth,
                               nodes_added_frontier = nodes_added_frontier))
    count <- count + 1
  }
  
  # Get runtime
  end_time <- Sys.time()
  
  result <- list()
  result$name    <- name_method
  result$runtime <- end_time - start_time
  
  # Show the obtained (or not) final solution
  if (end_reason == "Solution") {
    print("Solution found!!", quote=F)
    to.string(node_first$state)
    print("Executed Actions: ", quote=F)
    print(node_first$actions, quote=F)
    result$state_final = node_first
  } else {
    if (end_reason == "Frontier") {
      print("Frontier is empty. No Solution found", quote=F)
    } else {
      print("Maximum Number of iterations reached. No Solution found", quote = F)
    }
    
    result$state_final <- NA
  }
  
  result$report <- report
  print(paste0("* END: ", name_method), quote = F)
  
  return(result)
}


update.node <- function(existing, successor, frontier, expanded, problem) {
  # Update existing node if its evaluation is worse than the successor node
  if (existing$evaluation > successor$evaluation) {
    existing$parent     <- successor$parent
    existing$actions    <- successor$actions
    existing$depth      <- successor$depth
    existing$cost       <- successor$cost
    existing$evaluation <- successor$evaluation
    
    # Remove successors of existing node that are in frontier
    if (any(sapply(frontier, function (x) identical(x$state, existing$state)))) {
      indexes <- which(sapply(frontier, function (x) x$parent %in% existing$parent))
      frontier <- frontier[-indexes]
    }

    # Update successors of existing node that are in frontier
    if (any(sapply(expanded, function (x) identical(x$state, existing$state)))) {
      # Update successors of existing node
      for (j in 1:length(frontier)) {
        if (frontier[[j]]$parent %in% existing$parent) {
          frontier[[j]]$evaluation <- frontier[[j]]$cost + get.evaluation(frontier[[j]]$state, problem)
        }
      }
    }
  }
  
  return(list(frontier, expanded))
}

# Check for repeated states
check.repeated.state <- function(node, successor, frontier, expanded, problem) {
  # Check if the successor node is already present in frontier or expanded
  present_in_frontier <- any(sapply(frontier, function(x) identical(x$state, successor$state)))
  present_in_expanded <- any(sapply(expanded, function(x) identical(x$state, successor$state)))
  
  # Update the existing node if it's in frontier
  if (present_in_frontier) {
    existing_node_index <- which(sapply(frontier, function(x) identical(x$state, successor$state)))
    existing_node <- frontier[[existing_node_index]]
    lists <- update.node(existing_node, successor, frontier, expanded, problem)
    frontier <- lists[[1]]
    expanded <- lists[[2]]
  }
  
  # Update the existing node if it's in expanded
  if (present_in_expanded) {
    existing_node_index <- which(sapply(expanded, function(x) identical(x$state, successor$state)))
    existing_node <- expanded[[existing_node_index]]
    lists <- update.node(existing_node, successor, frontier, expanded, problem)
    frontier <- lists[[1]]
    expanded <- lists[[2]]
  }
  
  return(list(frontier, expanded))
}
