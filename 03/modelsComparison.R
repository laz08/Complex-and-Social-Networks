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

###
# Load of baseMetrics for closeness function loading.
source("baseMetrics.R")
source("binomial.R")
source("switching.R")
#############################
TESTING = TRUE
options(scipen=999)
#############################

computeModelsComparisonTable <- function(){
    
    source = read.table("language_lists.txt", header = FALSE, stringsAsFactors = FALSE)
    
    languages = c("Arabic", "Basque", "Catalan", "Chinese", "Czech", "English", "Greek", "Hungarian", "Italian", "Turkish")
    
    table_2 <- data.table("Language" = character(),
                          "Metric" = numeric(),
                          "p-val.(Binomial)" = numeric(),
                          "p-val.(Switching)" = numeric(),
                          stringsAsFactors = FALSE)
    
    computeSwitching = TRUE
    for (x in 1:length(languages)){
        if(TESTING){
            x = 2    
        }
        
        cat("Iteration: ", x, "\n")
        file <- source$V1[x]
        language <- languages[x]
        
        if(x == 4 || x == 5) {
            computeSwitching = FALSE
        } else{
            computeSwitching = TRUE
        }
        
        degree_sequence = read.table(file,
                                     header = FALSE,
                                     stringsAsFactors = FALSE,
                                     sep = " ",
                                     quote="")
        
        N = as.numeric(degree_sequence[1, 1])
        E = as.numeric(degree_sequence[1, 2])
        original_graph = graph.data.frame(degree_sequence[-1, ])
        original_graph = simplify(original_graph, remove.multiple = TRUE, remove.loops = TRUE)
        
        metric <- computeGraphCloseness(original_graph, N)
        bin <- computeBinomialPValue(metric, N, E, 20)
        switch <- computeSwitchingPValue(metric, original_graph, N, 20)
        
        table_2 <- rbind(table_2, list(language, metric, bin, switch))
        
        if(TESTING){
            break
        }
    }
    return(table_2)
}


table = computeModelsComparisonTable()
table
