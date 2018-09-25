install.packages("igraph")
install.packages("ggplot2")
install.packages("rlang")
library(igraph)
library(ggplot2)

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
  
   # Plot the data
  x = as.factor(x)
  ws_graph = data.frame(p = x, L, C)
  ggplot(ws_graph, aes(x=p, y=C)) +
    geom_point(data = ws_graph, aes(x=p,y=C, colour="C(p)/C(0)")) +
     geom_point(data = ws_graph, aes(x=p,y=L, colour = "L(p)/L(0)")) +
      labs(title = "WS model metrics\n", x = "p", y = "", color = "\n") +
      theme(axis.text.x=element_text(angle = 90, hjust = 0)) +
      scale_x_discrete(labels = abbreviate) +
      theme(plot.title = element_text(hjust = 0.5))
  
}

ws_generator(dim, S, nie)

# Function to create ER graphs and plot average shortest path

er_generator <- function(){
  
  iters = c(10,50,100,500,1000,10000) #, 200000) #, 400000)
  path <- c()
  
  for(i in iters){
      n = i # Number of vertices of graph
      e = 0.2
      p = ((1+e)*log(n))/n #probability of edge between two arbitrary edges
      er <- erdos.renyi.game(n,p)
      path <- append(path, average.path.length(er))
  }
  
  (path)
}
er_generator()









