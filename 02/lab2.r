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
df <-as.data.frame(degree_sequence[!(degree_sequence$V1==0),])


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

# Estimating parameters with log-likelihood (Maximum likelihood estimation)
# TODO


# Minus log-likelihood function (minus_log_likelihood = -L)
minus_log_likelihood_zeta <- function(gamma){ 
  length(x) * log(zeta(gamma)) + gamma * sum(log(x))
}

mle_zeta <- mle(minus_log_likelihood_zeta, 
                start = list(gamma = 2),
                method = "L-BFGS-B",
                lower = c(1.00000001))
summary(mle_zeta)
attributes(summary(mle_zeta))
attributes(summary(mle_zeta))$coef[1]


# Displaced Poisson function
x <- degree_sequence$V1  
lambda_0 = sum(x)/length(x) # Strating Lambda for trial 
poisson <- function(lambda){
  -(sum(x) * log(lambda) - length(x)*(lambda + log(1 - exp(-lambda))) - get_C(x))
}
poisson(lambda_0)

mle_poisson <- mle(poisson, 
                start = list(lambda = lambda_0),
                method = "L-BFGS-B",
                lower = c(1.00000001))
summary(mle_poisson)
attributes(summary(mle_poisson))
attributes(summary(mle_poisson))$coef[1]

