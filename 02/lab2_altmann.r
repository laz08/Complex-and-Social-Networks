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
  setwd("~/Google Drive/UPC/Fall 2018/CSN/Labs/git_labs/Complex-and-Social-Networks/02")
}
rm(wd)

########### Minus Log-likelihood Functions ########### 

get_AIC <- function(m2logL,K,N) {
  m2logL + 2*K*N/(N-K-1) # AIC with a correction for sample size
}

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

get_c <- function(x, lambda, delta){ 
  c = 0
  for (k in 1:length(x)){
    c = c + ( k^(-lambda) * exp(-delta * k) )
  }
  return(1/c)
  
}


############# Creating summary table

write_summary <- function(language,file) {
  degree_sequence = read.table(file, header = FALSE)
  l <- list(language,length(degree_sequence$V1),sum(degree_sequence$V1),max(degree_sequence$V1),sum(degree_sequence$V1)/length(degree_sequence$V1),length(degree_sequence$V1)/sum(degree_sequence$V1), get_MP(degree_sequence$V1), get_C(degree_sequence$V1))
  return(l)
}


create_sum_table <- function(source){
  
  temp_df <- data.table("Language" = character(),
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


###########  Estimating log-likelihood parameters ########### 
compute_log_likelihoods <- function(M, N, maxDegree, MP, C, deg_seq){
  
  # Displaced Poisson function
  minus_log_likelihood_poisson <- function(lambda){
    -(M * log(lambda) - N*(lambda + log(1 - exp(-lambda))) - C) 
  }
  
  # Displaced Geometric function
  minus_log_likelihood_geometric <- function(q){
    -(M - N) * log(1 - q) - N * log(q)
  }
  
  # Minus log-likelihood function (minus_log_likelihood = -L)
  minus_log_likelihood_zeta <- function(gamma){ 
    gamma * MP + N * log(zeta(gamma))
  }
  
  # Minus log-likelihood zeta function with lambda - 2
  minus_log_likelihood_zeta2 <- function(){
    2 * MP + N * log(pi^2/6)
  }
  
  # Minus log-likelihood right-truncated zeta
  minus_log_likelihood_zeta3 <- function(gamma, k){
    gamma * MP + N * log(sum(seq(1, k)^(-gamma)))
  }
  
  # Altmann function
  minus_log_likelihood_altmann <- function(gamma, delta, dat){
      c <- 1/sum(seq(1:N)^(-gamma)*exp(-delta*seq(1:N)))
      -1 * sum(log(c) - log(dat) * gamma - delta * dat)
  }
  
  mle_poisson <- mle(minus_log_likelihood_poisson,
                     start = list(lambda = M/N),
                     method = "L-BFGS-B",
                     lower = c(1.000001))
  
  mle_geometric <- mle(minus_log_likelihood_geometric,
                       start = list(q = N/M), 
                       method = "L-BFGS-B",
                       lower = c(0.01), 
                       upper = c(.99)) # No higher than .99 (ex: .999)
  
  mle_zeta <- mle(minus_log_likelihood_zeta,
                  start = list(gamma = 2),
                  method = "L-BFGS-B",
                  lower = c(1.00000001))
  
  mle_zeta2 <- mle(minus_log_likelihood_zeta,
                   fixed = list(gamma = 2),
                   method = "L-BFGS-B",
                   lower = c(1.00000001))
  
  mle_zeta3 <- mle(minus_log_likelihood_zeta3,
                   start = list(gamma = 2, k = maxDegree), 
                   method = "L-BFGS-B",
                   lower = c(1.00000001, N))
  
  
  mle_altmann <- mle(minus_log_likelihood_altmann,
                     start = list(gamma = 1, delta = 1), 
                     fixed = list(dat = deg_seq),
                     method = "L-BFGS-B",
                     lower = c(0.0001, 0.0001))
  
  vec <- c(
    round(attributes(summary(mle_poisson))$coef[1], digits=3),
    round(attributes(summary(mle_geometric))$coef[1], digits=3),
    round(attributes(summary(mle_zeta))$coef[1], digits=3),
    round(attributes(summary(mle_zeta3))$coef[1], digits=3),
    round(attributes(summary(mle_zeta3))$coef[2], digits=3),
    round(attributes(summary(mle_altmann))$coef[1], digits=3),
    round(attributes(summary(mle_altmann))$coef[2], digits=3),
    round(get_AIC(attributes(summary(mle_poisson))$m2logL, 1, N), digits=3),
    round(get_AIC(attributes(summary(mle_geometric))$m2logL, 1, N), digits=3),
    round(get_AIC(attributes(summary(mle_zeta))$m2logL, 1, N), digits=3),
    round(get_AIC(attributes(summary(mle_zeta2))$m2logL, 0, N), digits=3),
    round(get_AIC(attributes(summary(mle_zeta3))$m2logL, 2, N), digits=3),
    round(get_AIC(attributes(summary(mle_altmann))$m2logL, 2, N), digits=3)
  )
  
  return(vec)
}



########### MODEL SELECTION ########### 
compute_coeffs_table <- function(summary_table, out_source) {
  coeff_table <- data.table("Language" = character(),
                            "lambda" = numeric(),
                            "q" = numeric(),
                            "gamma " = numeric(),
                            "gamma3" = numeric(),
                            "k_max" = numeric(),
                            "delta_altmann" = numeric(),
                            "gamma_altmann" = numeric(),
                            stringsAsFactors = FALSE)
  
  aic_table <- data.table("Language" = character(),
                          "Poisson" = numeric(),
                          "Geometric" = numeric(),
                          "(Zeta)" = numeric(),
                          "(Zeta gamma2)" = numeric(),
                          "(RT Zeta)" = numeric(),
                          "Altmann" = numeric(),
                          stringsAsFactors = FALSE)
  
  for (i in seq(length(summary_table$Language))) {
    language <- summary_table[i]$Language 
    M <- summary_table[i]$M
    N <- summary_table[i]$N
    MP <- summary_table[i]$MP
    C <- summary_table[i]$C
    maxDegree <- summary_table[i]$`Maximum Degree`
    deg_seq = read.table(out_source$file[i], header = FALSE)
    deg_seq = deg_seq$V1
    resultList <- compute_log_likelihoods(M, N, maxDegree, MP, C, deg_seq)
    g <- as.list(resultList[1:7])
    h <- as.list(resultList[8:13])
    coeff_table <- rbind(coeff_table, c(language, g))
    aic_table <- rbind(aic_table, c(language, h))
  }
  
  # Computing delta: AIC - best AIC 
  for (i in seq(length(aic_table$'Poisson'))) {
    aic_table[i, 2:7] <- aic_table[i, 2:7] - min(aic_table[i, 2:7])
  }
  return(list(coeff_table, aic_table))
}


out_source = read.table("list_out.txt", header = TRUE, as.is = c("language","file")) 
summary_table <- create_sum_table(out_source)
aic_c_table <- compute_coeffs_table(summary_table, out_source)
coeffs_table <- as.data.table(aic_c_table[1])
aic_table <-  as.data.table(aic_c_table[2])

coeffs_table
aic_table



########### Testing models on contrived data ########### 

geometric_01 = read.table("./samples_from_discrete_distributions/data/sample_of_geometric_with_parameter_0.1.txt",
                          header = FALSE)


geom_source = read.table("./list_geometric.txt", header = TRUE, as.is = c("language","file")) # For testing 

(test_table <- create_sum_table(geom_source))
aic_c_table_test <- compute_coeffs_table(test_table)

coeffs_table_test <- as.data.table(aic_c_table_test[1])
aic_table_test <- as.data.table(aic_c_table_test[2])

names(coeffs_table_test) <- c("Test", "Lambda", "q", "gamma 1", "gamma 2", "k_max")
names(aic_table_test) <- c("Test", "Poisson", "Geometric", "Zeta Gamma 2", "Zeta", "RT Zeta")


coeffs_table_test
aic_table_test

# OK for geom. distrib.

zeta_source = read.table("./list_zeta.txt", header = TRUE, as.is = c("language","file")) # For testing 

(test_table <- create_sum_table(zeta_source))
aic_c_table_test <- compute_coeffs_table(test_table)


coeffs_table_test <- as.data.table(aic_c_table_test[1])
aic_table_test <- as.data.table(aic_c_table_test[2])


names(coeffs_table_test) <- c("Test Distribution", "Lambda", "q", "gamma 1", "gamma 3", "k_max")
names(aic_table_test) <- c("Test Distribution", "Poisson", "Geometric", "Zeta", "(Zeta gamma 2)", "(RT Zeta)")


coeffs_table_test
aic_table_test

