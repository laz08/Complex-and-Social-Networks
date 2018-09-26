<<<<<<< HEAD
# Load and install necessary packages
requiredPackages <- c("igraph", "ggplot2")

for (pac in requiredPackages) {
    if(!require(pac,  character.only=TRUE)){
        install.packages(pac, repos="http://cran.rstudio.com")
        library(pac,  character.only=TRUE)
    } 
}
rm(pac)
rm(requiredPackages)

##################################
# 1./
# 
# Function to create random WS graphs and plot transitivity and diameter
ws_generator <- function(dim, S, nie){
  
  # WS base numbers to normalize 
  L_base <- average.path.length(watts.strogatz.game(dim, S, nie, 0))
  C_base <- transitivity(watts.strogatz.game(dim, S, nie, 0))
    
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
  
  # Prepare data for plotting
  ws_graph = data.frame(p = as.factor(x), L, C)
  
  # Plot the data
  ggplot(ws_graph, aes(x=p, y=C)) +
      geom_point(aes(x=p,y=C, colour="C(p)/C(0)")) +
      geom_point(aes(x=p,y=L, colour = "L(p)/L(0)")) +
      labs(title = "WS model metrics\n", x = "p", y = "", color = "\n") +
      theme(axis.text.x=element_text(angle = 90, hjust = 0)) +
      scale_x_discrete(labels = abbreviate) +
      theme(plot.title = element_text(hjust = 0.5))
  
}

# Parameters
dim = 1
S = 500
nie = 4

ws_generator(dim, S, nie)



##################################
# 2./
# 
# Function to create ER graphs and plot average shortest path
er_generator <- function(){
  
  iters = c(5,10,15,30,50,100,500,1000,10000,50000)
  path <- c()
  
  for(i in iters){
      n = i # Number of vertices of graph
      e = 0.2
      p = ((1+e)*log(n))/n #probability of edge between two arbitrary edges
      er <- erdos.renyi.game(n,p)
      path <- append(path, average.path.length(er))
  }
  
  # One possible result, since it takes a while to compute...:
  # 1.666667 1.666667 2.351648 3.110837 2.713469 2.677778 3.336160 3.510851 4.102915 4.515819
  
  # Prepare data for plotting
  er_graph = data.frame(N = iters, Path = path)
  
  # Plot the data
  ggplot(er_graph, aes(x=N, y=Path, group = 1)) +
      geom_line(colour = "Gray", show.legend=F) +
      geom_point(colour = "Blue", show.legend=F) + 
      labs(title = "ER model avg. shortest path", x = "num nodes", y = "Avg. shortest path")

}

er_generator()








