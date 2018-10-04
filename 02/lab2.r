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
wd = getwd()
if(grepl("nora", wd)) {
    setwd("~/git/csn-labs/02")
} else {
  setwd("~/Google Drive/UPC/Fall 2018/CSN/Labs/Lab2")
}
rm(wd)


degree_sequence = read.table("./data/English_out-degree_sequence.txt",
                             header = FALSE)
num_nodes <- nrow(degree_sequence)
sum_degree <- sum(degree_sequence)
mean_degree <- sum(degree_sequence)/dim(degree_sequence)[1]


# Remove nodes of out-degree 0 (k=0)
degree_sequence$V1 <- degree_sequence[!(degree_sequence$V1==0),]


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



# Create table of degree statistics
source = read.table("list_out.txt", header = TRUE, as.is = c("language","file"))
     
write_summary <- function(language,file) {
  degree_sequence = read.table(file, header = FALSE)
  l <- list(language,length(degree_sequence$V1),sum(degree_sequence$V1),max(degree_sequence$V1),sum(degree_sequence$V1)/length(degree_sequence$V1),length(degree_sequence$V1)/sum(degree_sequence$V1), get_MP(degree_sequence$V1), get_C(degree_sequence$V1))
  return(l)
}

create_sum_table <- function(){
  
  temp_df <- data.table("language" = character(),
                        "N" = numeric(),
                        "M" = numeric(),
                        "Maximum Degree" = numeric(),
                        "M/N" = numeric(),
                        "N/M" = numeric(),
                        "MP" = numeric(),
                        "C" = numeric(),
                        stringsAsFactors = FALSE)
  
  for (x in 1:nrow(source)){
    file <- source$file[x]
    language <- source$language[x]
    sequence <- write_summary(language, file)
    temp_df <- rbind(temp_df,sequence)
  }
  return(temp_df)
}               
                    
(summary_table <- create_sum_table())

# Barplots of data
degree_sequence = read.table("./data/English_out-degree_sequence.txt", header = FALSE)
degree_spectrum = table(degree_sequence)
barplot(degree_spectrum, main = "English", xlab = "degree", ylab = "number of vertices")
barplot(degree_spectrum, main = "English", xlab = "degree", ylab = "number of vertices", log = "xy")
barplot(degree_spectrum, main = "English", xlab = "degree", ylab = "number of vertices", log = "y")


get_H <- function(k, gamma) {
    # TODO: Check H
    return(sum(seq(1, k)^(-gamma)))
}

########### Minus Log-likelihood Functions ########### 

# Basic metrics
x <- degree_sequence$V1
M <- sum(x)
N <- length(x)

    
    
# Displaced Poisson function
minus_log_likelihood_poisson <- function(lambda, M, N, C){
  -(M * log(lambda) - N*(lambda + log(1 - exp(-lambda))) - C) 
}

# Displaced Geometric function
minus_log_likelihood_geometric <- function(q, M, N){
  -(M - N * log(1 - q)) - (N * log(q))
}

# Minus log-likelihood function (minus_log_likelihood = -L)
minus_log_likelihood_zeta <- function(gamma, N, MP){ 
    gamma * MP + N * log(zeta(gamma))
}

# Minus log-likelihood zeta function with lambda - 2
minus_log_likelihood_zeta2 <- function(N, MP){
  2 * MP + N * log(pi^2/6)
}

# Minus log-likelihood right-truncated zeta
minus_log_likelihood_zeta3 <- function(gamma, k, N, MP){
  gamma * MP + N * log(get_H(k, gamma))
}



###########  Estimating log-likelihood parameters ########### 
compute_log_likelihoods <- function(M, N, maxDegree, MP, C){
    
    mle_poisson <- mle(minus_log_likelihood_poisson, 
                    start = list(lambda = M/N, M = M, N = N, C = C),
                    method = "L-BFGS-B",
                    lower = c(1.00000001))
    attributes(summary(mle_poisson))$coef[1]
    
    mle_geometric <- mle(minus_log_likelihood_geometric, 
                    start = list(q = N/M, M = M, N = N),
                    method = "L-BFGS-B",
                    lower = c(0.00001), # TODO - is this correct? 
                    upper = c(0.99)) # TODO - is this correct? 
                #Laura: Do not go above 0.99 or it will give an error 
                #'  non-finite finite-difference value [1]
    
    attributes(summary(mle_geometric))$coef[1]
    
    mle_zeta <- mle(minus_log_likelihood_zeta, 
                    start = list(gamma = 2, N = N, MP = MP),
                    method = "L-BFGS-B",
                    lower = c(1.00000001))
    
    attributes(summary(mle_zeta))$coef[1]
    
    
    # TODO: Check what to do with zeta 2
    mle_zeta2 <- minus_log_likelihood_zeta2(N, MP)
   
    
    # TODO: Check warning below. Doesn't look good.
    mle_zeta3 <- mle(minus_log_likelihood_zeta3, 
                    start = list(gamma = 2, k = maxDegree, N = N, MP = MP), #Alternatively, k = N
                    method = "L-BFGS-B",
                    lower = c(1.001, length(x)))
    
    attributes(summary(mle_zeta3))$coef[1]
    
    vec_coeffs <- list(
        attributes(summary(mle_poisson))$coef[1],
        attributes(summary(mle_geometric))$coef[1],
        attributes(summary(mle_zeta))$coef[1],
        mle_zeta2,
        attributes(summary(mle_zeta3))$coef[1]
    )
    return(vec_coeffs)
}


### MODEL SELECTION #####



compute_coeffs_table <- function(summary_table) {
    coeff_table <- data.table(#"language" = character(),
                          "lambda" = numeric(),
                          "q" = numeric(),
                          "gamma_1" = numeric(),
                          "gamma_2" = numeric(),
                          "k_max" = numeric(),
                          stringsAsFactors = FALSE)
    
    for (i in seq(length(summary_table$language))) {
        language <- summary_table[i]$language 
        M <- summary_table[i]$M
        N <- summary_table[i]$N
        MP <- summary_table[i]$MP
        C <- summary_table[i]$C
        maxDegree <- summary_table[i]$`Maximum Degree`
        coeffs <- compute_log_likelihoods(M, N, maxDegree, MP, C)
        
        coeff_table <- rbind(coeff_table, coeffs)
    }
    return(coeff_table)
}
coeffs_table <- compute_coeffs_table(summary_table)
coeffs_table




attributes(summary(mle_zeta))$m2logL
get_AIC <- function(m2logL,K,N) {
    m2logL + 2*K*N/(N-K-1) # AIC with a correction for sample size
}


vec_aics <- c(
    get_AIC(attributes(summary(mle_poisson))$m2logL, 1, N),
    get_AIC(attributes(summary(mle_geometric))$m2logL, 1, N),
    get_AIC(attributes(summary(mle_zeta))$m2logL, 1, N),
    get_AIC(attributes(summary(mle_zeta2))$m2logL, 1, N),
    get_AIC(attributes(summary(mle_zeta3))$m2logL, 1, N)
)

best_AIC <- min(vec_aics)
vec_delta <- vec_aics - best_AIC