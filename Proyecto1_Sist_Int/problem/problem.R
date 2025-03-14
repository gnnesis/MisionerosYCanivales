# =========================================================================
# Implementación del problema de los Misioneros y Caníbales
# =========================================================================

# Esta función inicializa el problema
initialize.problem <- function(num_missionaries = 3, num_cannibals = 3, boat_capacity = 2) {
  problem <- list()
  
  # Nombre del problema
  problem$name <- "Missionaries and Cannibals"
  
  # Estado inicial: todos los misioneros y caníbales en la orilla izquierda, barca en la izquierda
  problem$state_initial <- as.numeric(c(num_missionaries, num_cannibals, 1, 0, 0))  # (Mizq, Cizq, B, Mder, Cder)
  
  # Estado final: todos en la orilla derecha, barca en la derecha
  problem$state_final <- as.numeric(c(0, 0, 0, num_missionaries, num_cannibals))
  
  # Acciones posibles: combinaciones de misioneros y caníbales que pueden viajar
  actions_list <- list(
    c(1, 0), c(2, 0), c(0, 1), c(0, 2), c(1, 1)
  )
  
  # Convertir acciones en data.frame
  problem$actions_possible <- as.data.frame(do.call(rbind, actions_list))
  colnames(problem$actions_possible) <- c("missionaries", "cannibals")
  
  # Capacidad de la barca
  problem$boat_capacity <- boat_capacity
  
  return(problem)
}

# Verifica si una acción es aplicable en un estado dado
is.applicable <- function(state, action, problem) {
  state <- as.numeric(state)  # Asegurar que sea numérico
  missionaries_left <- state[1]
  cannibals_left <- state[2]
  boat_side <- state[3]
  
  missionaries_moving <- action[1]
  cannibals_moving <- action[2]
  
  if (sum(action) > problem$boat_capacity || sum(action) == 0) {
    return(FALSE)  # No puede llevar más personas de las permitidas ni viajar vacío
  }
  
  if (boat_side == 1) {  # Barca en la izquierda
    if (missionaries_moving > missionaries_left || cannibals_moving > cannibals_left) {
      return(FALSE)  # No hay suficientes personas en la orilla
    }
  } else {  # Barca en la derecha
    if (missionaries_moving > state[4] || cannibals_moving > state[5]) {
      return(FALSE)
    }
  }
  
  # Verificar que no se violen las restricciones después de la acción
  new_state <- effect(state, action, problem)
  missionaries_left_new <- new_state[1]
  cannibals_left_new <- new_state[2]
  missionaries_right_new <- new_state[4]
  cannibals_right_new <- new_state[5]
  
  if ((missionaries_left_new > 0 && cannibals_left_new > missionaries_left_new) ||
      (missionaries_right_new > 0 && cannibals_right_new > missionaries_right_new)) {
    return(FALSE)
  }
  
  return(TRUE)
}

# Devuelve el estado resultante tras aplicar una acción
effect <- function(state, action, problem) {
  state <- as.numeric(state)  # Asegurar que sea numérico
  missionaries_moving <- action[1]
  cannibals_moving <- action[2]
  boat_side <- state[3]
  
  if (boat_side == 1) {  # Barca en la izquierda
    state[1] <- state[1] - missionaries_moving
    state[2] <- state[2] - cannibals_moving
    state[4] <- state[4] + missionaries_moving
    state[5] <- state[5] + cannibals_moving
    state[3] <- 0  # Mueve la barca a la derecha
  } else {  # Barca en la derecha
    state[1] <- state[1] + missionaries_moving
    state[2] <- state[2] + cannibals_moving
    state[4] <- state[4] - missionaries_moving
    state[5] <- state[5] - cannibals_moving
    state[3] <- 1  # Mueve la barca a la izquierda
  }
  
  return(as.numeric(state))  # Asegurar que el resultado sea numérico
}

# Verifica si un estado es final
is.final.state <- function(state, final_state, problem) {
  state <- as.numeric(state)  # Asegurar que sea numérico
  return(all(state == final_state))
}

# Representación del estado en string
to.string <- function(state, problem) {
  state <- as.numeric(state)  # Asegurar que sea numérico
  return(paste("Left:", state[1], "M", state[2], "C | Boat:", ifelse(state[3] == 1, "Left", "Right"), "| Right:", state[4], "M", state[5], "C"))
}

# Coste de una acción
get.cost <- function(action, state, problem) {
  state <- as.numeric(state)  # Asegurar que sea numérico
  return(1)  # Cada cruce tiene un coste de 1
}

# Función heurística para búsquedas informadas
get.evaluation <- function(state, problem) {
  state <- as.numeric(state)  # Asegurar que sea numérico
  return(state[1] + state[2])  # Heurística basada en el número de personas que quedan en la izquierda
}