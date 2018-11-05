# Load and install necessary packages
requiredPackages <- c("igraph", "ggplot2", "data.table", "knitr")

for (pac in requiredPackages) {
    if(!require(pac,  character.only=TRUE)){
        install.packages(pac, repos="http://cran.rstudio.com")
        library(pac,  character.only=TRUE)
    } 
}
rm(pac)
rm(requiredPackages)


# Set WD and load data
wd = getwd()
if(grepl("nora", wd)) {
    setwd("~/git/csn-labs/03")
} else {
    setwd("~/Google Drive/UPC/Fall 2018/CSN/Labs/git_labs/Complex-and-Social-Networks/03")
}
rm(wd)

#####
computeBasicMetricsTable <- function(){
    source = read.table("language_lists.txt", header = FALSE, stringsAsFactors = FALSE)
    
    languages = c("Arabic", "Basque", "Catalan", "Chinese", "Czech", "English", "Greek", "Hungarian", "Italian", "Turkish")
    table_1 <- data.table("Language" = character(),
                          "N" = numeric(),
                          "E" = numeric(),
                          "k" = numeric(),
                          "delta" = numeric(),
                          stringsAsFactors = FALSE)
    
    for (x in 1:length(languages)){
        file <- source$V1[x]
        language <- languages[x]
        
        degree_sequence = read.table(file,
                                     header = FALSE,
                                     stringsAsFactors = FALSE,
                                     sep = " ",
                                     quote="")
        
        tmp_graph = graph.data.frame(degree_sequence[-1, ])
        tmp_graph = simplify(tmp_graph, remove.multiple = TRUE, remove.loops = TRUE)
        E = gsize(tmp_graph)
        
        N = as.numeric(degree_sequence[1, 1])
        k = 2*E/N
        delta = 2*E/(N * (N-1))
        
        table_1 <- rbind(table_1, list(language, N, E, k, delta))
    }
    return(table_1)
}



computeGraphClosenessOLD <- function(graph, N) {
  x_vec = closeness(graph, normalized = TRUE) 
  x = sum(x_vec)/N
  return(x)
}

# Compute closeness estimate (no bounds function)
computeGraphCloseness <- function(graph_simple, N){
    closeness = 0
    M = N/50 # Estimate the closeness
    m_sample = sample(1:N, N, replace = FALSE)
    for (i in seq(1:M)){
        #cat(i, "\n")
        temp = distances(graph_simple, m_sample[i], V(graph_simple), mode = "all", algorithm = "dijkstra"  )
        temp[which(!is.finite(temp))] <- 0
        temp <- temp[temp >0]
        temp <- sapply(temp, function(x) 1/x)
        if(length(temp) > 0){
            temp <- sum(temp)/(N-1)    
        } else {
            temp <- 0
        }
        closeness <- closeness + temp
    }
    closeness = closeness/(M)
    
    return(closeness)
}

# Compute closeness using bounds functions
computeGraphClosenessBounds <- function(graph_simple, N, C_original, M_max){
  closeness = 0
  M = M_max # Estimate the closeness
  m_sample = sample(1:N, N, replace = FALSE)
  
  for (i in seq(1:M)){
    #cat(i, "\n")
    temp = distances(graph_simple, m_sample[i], V(graph_simple), mode = "all", algorithm = "dijkstra" )
    temp[which(!is.finite(temp))] <- 0
    temp <- temp[temp >0]
    temp <- sapply(temp, function(x) 1/x)
    if(length(temp) > 0){
      temp <- sum(temp)/(N-1)    
    } else {
      temp <- 0
    }
    
    # Current closness measure after first "i" iterations
    closeness <- closeness + temp
    
    # Comput Cnh_min and Cnh_max
    Cnh_min = ((1/N) * closeness) 
    Cnh_max = ((1/N) * closeness) +  1 - (i/N)
    
    # If Cnh_min is greater than actual C, return 1     
    if (Cnh_min >= C_original){ return(1)}
    
    # Compute Cnh_max and check if: C <= Cnh_max
    if (Cnh_max < C_original) { return(0)}
  
  }
  closeness = closeness/(M)
  
  return(closeness)
}

# To test different vertex input orderings in closeness bounds function
computeGraphClosenessBoundsOrdering <- function(graph_simple, N, C_original, M_max, order){
  
  #### Set ordering
  m_sample = 1:N
  # 1 = original (above), 2 = random, 3 = degre increasing, 4 = degree decreasing
  if (order == 2){ 
    m_sample = sample(1:N, N, replace = FALSE) 
  }
  else if (order == 3){ 
    d = degree(graph_simple,V(graph_simple))
    m_sample <- m_sample[order(d, decreasing = FALSE)]
  }
  else if (order == 4){ 
    d = degree(graph_simple,V(graph_simple))
    m_sample <- m_sample[order(d, decreasing = TRUE)]
  }
  
  #### Compute closeness and record num iterations (Mmax)
  closeness = 0
  M = M_max 
  for (i in seq(1:M-2)){
    temp = distances(graph_simple, m_sample[i], V(graph_simple), mode = "all" )
    temp[which(!is.finite(temp))] <- 0
    temp <- temp[temp >0]
    temp <- sapply(temp, function(x) 1/x)
    if(length(temp) > 0){
      temp <- sum(temp)/(N-1)    
    } else {
      temp <- 0
    }
    
    # Current closness measure after first "i" iterations
    closeness <- closeness + temp
    
    # Comput Cnh_min and Cnh_max
    Cnh_min = ((1/N) * closeness) 
    Cnh_max = ((1/N) * closeness) +  1 - (i/N)
    
    # If Cnh_min is greater than actual C, return 1     
    if (Cnh_min >= C_original){ 
      cat(i, "\n")
      return(1)
    }
    
    # Compute Cnh_max and check if: C <= Cnh_max
    if (Cnh_max < C_original) { 
      cat(i, "\n")
      return(0)
    }
    
  }
  closeness = closeness/(M)
  cat(i, "\n")
  return(closeness)
}

