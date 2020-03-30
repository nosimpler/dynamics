# SINDy extras

# Reconstruct trajectories based on SINDy fit

x_fit <- 
  
sindy_vector_to_ode <- function(svector){
  lhs <- colnames(svector)
  rhs <- apply(sindy_element_to_function, )
  ode <- tibble(lhs=lhs, rhs=rhs)
  return(ode)
}

sindy_element_to_function <- function(rowname, constant){
  names <- strsplit(rowname, ':')
  func <- function(names)
}

rn <- "O1:O1:O1"
c <- 4

