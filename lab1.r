install.packages("igraph")
library(igraph)

# Parameters
dim = 1
S = 500
nie = 4

# WS base numbers to normalize 
L_base <- average.path.length(watts.strogatz.game(dim, S, nie, 0))
C_base <- transitivity(watts.strogatz.game(dim, S, nie, 0))


# Function to create random WS graphs and plot transitivity and diameter
ws_generator <- function(dim, S, nie){
  
  x <- 10^(seq(log10(.0001),0,.2))
  iters <- length(x)
  L <- numeric(iters)
  C <- numeric(iters)
  
  # Loop over each p-value
  for(i in 1:iters){
    average_L <- 0
    average_C <- 0
    # Generate 100 random graphs 
    for(j in 1:10){
      ws <- watts.strogatz.game(dim,S,nie,x[i])
      average_L <- average_L + average.path.length(ws)
      average_C <- average_C + transitivity(ws)
    }
    L[i] <- (average_L/10)/L_base
    C[i] <- (average_C/10)/C_base
  }
  values <- cbind(L,C) # col 1 = diameter, col 2 = transitivity
  
  # Create Plot
  y <- as.factor(10^(seq(log10(.0001),0,.2)))
  plot(y, ws_graph[,1])
  par(new = 'true')
  plot(y,ws_graph[,2])
  
}

ws_generator(dim, S, nie)










