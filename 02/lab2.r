install.packages("expss")
install.packages('knitr')
install.packages('data.table')
library(expss)
library(knitr)
library(data.table)

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

