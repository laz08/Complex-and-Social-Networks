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


set.seed(42)

## Read file
degree_sequence = read.table("./Basque_syntactic_dependency_network.txt",
                             header = FALSE,
                             stringsAsFactors = FALSE)

N = as.numeric(degree_sequence[1, 1])
E = as.numeric((degree_sequence[1, 2]))

degree_sequence = degree_sequence[-1, ]
graph = graph.data.frame(degree_sequence)


# Remove loops
is_simple(graph)
graph_simple = simplify(graph, remove.multiple = TRUE, remove.loops = TRUE)
is_simple(graph_simple)

##############
er_graph <- sample_gnm(N, E)


## Monte Carlo Algorithm


T = 20
f_bin = 0
x_vec = closeness(graph_simple) # Closeness of basque language tree
x = sum(x_vec)/N
xnh = 0

for(i in range(0, T)){
    er_graph <- sample_gnm(N, E)
    # Assuming ER has the same probability of creating an edge between two vertices
    # we can take the first M nodes without ordering the structure.
    # M = 10%
    M = N*10/100
    x_nh = sum(closeness(er_graph, vids=V(er_graph)[1:M]))/N
    if(x_nh >= x){
        f_bin = f_bin + 1
    }
}
p_xnh_x = f_bin/T
p_xnh_x


# graph_from_adjacency_matrix()


# TODO: Migrate this to a table for table 2 results
# Switching method
Q = log(E)
for(i in range(0, Q*E)){
    u_v_id = floor(runif(1) * E / 100) 
    s_t_id = floor(runif(1) * E / 100) 
}

