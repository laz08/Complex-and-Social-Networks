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
    setwd("~/Google Drive/UPC/Fall 2018/CSN/Labs/git_labs/Complex-and-Social-Networks/02")
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



computeGraphCloseness <- function(graph, N) {
  x_vec = closeness(graph, normalized = TRUE) 
  x = sum(x_vec)/N
  return(x)
}
