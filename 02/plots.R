
degree_sequence = read.table("./data/English_out-degree_sequence.txt", header = FALSE)

######## Plotting Data vs Geometric ######## 

g_dist <- function(x, q){
  k = x
  return( ( (1-q)^(k-1) ) * q )
}


geom_plot <- function(degree_sequence, q, language){
  # Prepare data frame
  N = length(degree_sequence$V1) # number of nodes in network
  max_d = max(degree_sequence$V1) # largest degree in network
  df <- data.frame(table(degree_sequence)) # data frame to store data for plotting
  df$degree_sequence <- as.numeric(as.character(df$degree_sequence)) # change to numeric so can plot
  df$gFreq <- df$degree_sequence # add column for distribution column
  df[3] <- apply(df[3], 2, g_dist, as.numeric(q)) # apply(data_frame,1,function,arguments_to_function_if_any)
  df[2] <- df[2]/N # normalize data to show probability
  max_p <- max(df[2]) # largest probability in network
  min_p <- min(df[2]) # smallest probability in network

  # Plot the data
  ggplot(df, aes(degree_sequence)) + 
    geom_point( aes(y=Freq), colour="blue") +  # first layer is point graph
    geom_line( aes(y=gFreq), colour = "red") + # second layer is a line graph
    scale_x_continuous(trans='log10', limits=(c(1,10000))) + # sets x-axis to log10 scale
    scale_y_continuous(trans='log10', limits=c(min_p, max_p+.3)) +   # sets y-axis to log10 scale
    ylab("Probability") + 
    xlab("Degree") + 
    labs(title=paste("Geometric distribution comparison with", language, sep= " "))+
    theme(plot.title = element_text(hjust = 0.5))
}


######## Plotting Data vs Zeta ######## 

z_dist <- function(x, maxdegree, gamma){
  k = x
  h = sum(seq(1,maxdegree)^(-gamma))
  return( k^(-gamma)/h )
}


zeta_plot <- function(degree_sequence, maxdegree, gamma, language){
  # Prepare data frame
  N = length(degree_sequence$V1) # number of nodes in network
  max_d = max(degree_sequence$V1) # largest degree in network
  df <- data.frame(table(degree_sequence)) # data frame to store data for plotting
  df$degree_sequence <- as.numeric(as.character(df$degree_sequence)) # change to numeric so can plot
  df$gFreq <- df$degree_sequence # add column for distribution column
  df[3] <- apply(df[3], 2, z_dist, as.numeric(maxdegree), as.numeric(gamma)) # apply(data_frame,1,function,arguments_to_function_if_any)
  df[2] <- df[2]/N # normalize data to show probability
  max_p <- max(df[2]) # largest probability in network
  min_p <- min(df[2]) # smallest probability in network
  
  # Plot the data
  ggplot(df, aes(degree_sequence)) + 
    geom_point( aes(y=Freq), colour="blue") +  # first layer is point graph
    geom_line( aes(y=gFreq), colour = "green") + # second layer is a line graph
    scale_x_continuous(trans='log10', limits=(c(1,10000))) + # sets x-axis to log10 scale
    scale_y_continuous(trans='log10', limits=c(min_p, max_p+.3)) +   # sets y-axis to log10 scale
    ylab("Probability") + 
    xlab("Degree") + 
    labs(title=paste("Right Truncated Zeta distribution comparison with", language, sep= " "))+
    theme(plot.title = element_text(hjust = 0.5))
}


######## Plotting Data vs Altmann ######## 

a_dist <- function(x, gamma, delta, N){
  c <- 1/sum(seq(1:N)^(-gamma)*exp(-delta*seq(1:N)))
  
  return( c * x^(-gamma) * exp(-delta * x) )
}


altmann_plot <- function(degree_sequence, gamma, delta, language){
  # Prepare data frame
  N = length(degree_sequence$V1) # number of nodes in network
  df <- data.frame(table(degree_sequence)) # data frame to store data for plotting
  df$degree_sequence <- as.numeric(as.character(df$degree_sequence)) # change to numeric so can plot
  df$gFreq <- df$degree_sequence # add column for distribution column
  df[3] <- apply(df[3], 2, a_dist, as.numeric(gamma), as.numeric(delta), as.numeric(N)) # apply(data_frame,1,function,arguments_to_function_if_any)
  df[2] <- df[2]/N # normalize data to show probability
  max_p <- max(df[2]) # largest probability in network
  min_p <- min(df[2]) # smallest probability in network
  
  # Plot the data
  ggplot(df, aes(degree_sequence)) + 
    geom_point( aes(y=Freq), colour="blue") +  # first layer is point graph
    geom_line( aes(y=gFreq), colour = "green") + # second layer is a line graph
    scale_x_continuous(trans='log10', limits=(c(.99,10000))) + # sets x-axis to log10 scale
    scale_y_continuous(trans='log10', limits=c(min_p, max_p+.3)) +   # sets y-axis to log10 scale
    ylab("Probability") + 
    xlab("Degree") + 
    labs(title=paste("Altmann distribution with", language, sep= " "))+
    theme(plot.title = element_text(hjust = 0.5))
}

altmann_plot(degree_sequence, 1.25, .011, "English")
