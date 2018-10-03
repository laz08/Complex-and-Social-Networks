requiredPackages <- c("igraph", "ggplot2", "data.table", "expss", "stats4", "VGAM")
for (pac in requiredPackages) {
  if(!require(pac,  character.only=TRUE)){
    install.packages(pac, repos="http://cran.rstudio.com")
    library(pac,  character.only=TRUE)
  } 
}
rm(pac)
rm(requiredPackages)

# Set WD and load data
setwd("~/Google Drive/UPC/Fall 2018/CSN/Labs/Lab2")
degree_sequence = read.table("./data/English_out-degree_sequence.txt",
                             header = FALSE)
num_nodes <- nrow(degree_sequence)
sum_degree <- sum(degree_sequence)
mean_degree <- sum(degree_sequence)/dim(degree_sequence)[1]


# Remove nodes of out-degree 0 (k=0)
degree_sequence$V1 <- degree_sequence[!(degree_sequence$V1==0),]


# Create table of degree statistics
source = read.table("list.txt", header = TRUE, as.is = c("language","file"))
     
write_summary <- function(language,file) {
  degree_sequence = read.table(file, header = FALSE)
  l <- list(language,length(degree_sequence$V1),max(degree_sequence$V1),sum(degree_sequence$V1)/length(degree_sequence$V1),length(degree_sequence$V1)/sum(degree_sequence$V1))
  return(l)
}

create_sum_table <- function(){
  
  temp_df <- data.table("language" = character(), "N" = numeric(), "Maximum Degree" = numeric(), "M/N" = numeric(), "N/M" = numeric(), stringsAsFactors = FALSE)
  
  for (x in 1:nrow(source)){
    file <- source$file[x]
    language <- source$language[x]
    sequence <- write_summary(language, file)
    temp_df <- rbind(temp_df,sequence)
  }
  return(temp_df)
}               
                    
summary_table <- create_sum_table()      

# Barplots of data
degree_sequence = read.table("./data/English_out-degree_sequence.txt", header = FALSE)
degree_spectrum = table(degree_sequence)
barplot(degree_spectrum, main = "English", xlab = "degree", ylab = "number of vertices")
barplot(degree_spectrum, main = "English", xlab = "degree", ylab = "number of vertices", log = "xy")

# Variables to be used in distribution functions
get_MP <- function(x) { return(sum(log(x)))}
get_C <- function(x) { C = 0
                       for(i in x){
                           j = 2
                           while( j <= i){
                             C <- C + log(j)
                             j = j+1
                           }
                         } 
                        return(C)
                      }


########### Minus Log-likelihood Functions ########### 
x <- degree_sequence$V1

# Displaced Poisson function
minus_log_likelihood_poisson <- function(lambda){
  -(sum(x) * log(lambda) - length(x)*(lambda + log(1 - exp(-lambda))) - get_C(x)) 
}

# Displaced Geometric function
minus_log_likelihood_geometric <- function(q){
  -(sum(x) - length(x) * log(1 - q)) - length(x) * log(q)
}

# Minus log-likelihood function (minus_log_likelihood = -L)
minus_log_likelihood_zeta <- function(gamma){ 
  length(x) * log(zeta(gamma)) + gamma * sum(log(x))
}

# Minus log-likelihood zeta function with lambda - 2
minus_log_likelihood_zeta2 <- function(){
  2 * sum(log(x)) + length(x) * log(pi^2/6)
}

# Minus log-likelihood right-truncated zeta
minus_log_likelihood_zeta3 <- function(gamma, k){
  gamma * sum(log(x)) + length(x) * log(sum(seq(1,k)^-gamma)) # TODO - correct???
}



###########  Estimating log-likelihood parameters ########### 
mle_poisson <- mle(minus_log_likelihood_poisson, 
                start = list(lambda = sum(x)/length(x)),
                method = "L-BFGS-B",
                lower = c(1.00000001))
attributes(summary(mle_poisson))$coef[1]

mle_geometric <- mle(minus_log_likelihood_geometric, 
                start = list(q = length(x)/sum(x)),
                method = "L-BFGS-B",
                lower = c(.00001), # TODO - is this correct? 
                upper = c(.99999)) # TODO - is this correct? 
attributes(summary(mle_zeta))$coef[1]

mle_zeta <- mle(minus_log_likelihood_zeta2, 
                start = list(gamma = 2),
                method = "L-BFGS-B",
                lower = c(1.00000001))

attributes(summary(mle_zeta))$coef[1]

mle_zeta2 <- mle(minus_log_likelihood_zeta,  # TODO fix everything
                start = list(gamma = 2),
                method = "L-BFGS-B",
                lower = c(1.00000001))

attributes(summary(mle_zeta))$coef[1]

mle_zeta3 <- mle(minus_log_likelihood_zeta3, # TODO check that it works
                start = list(gamma = 2, k = length(x)),
                method = "L-BFGS-B",
                lower = c(1.00000001, length(x)))

attributes(summary(mle_zeta))$coef[1]




