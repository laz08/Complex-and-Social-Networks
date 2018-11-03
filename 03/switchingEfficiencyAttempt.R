# Load and install necessary packages
requiredPackages <- c("igraph", "ggplot2", "data.table", "knitr", "Rcpp", "Matrix")
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


set.seed(42)

## Read file
degree_sequence = read.table("./Basque_syntactic_dependency_network.txt",
                             header = FALSE,
                             stringsAsFactors = FALSE)

N = as.numeric(degree_sequence[1, 1])

# Remove first row (summary row)
degree_sequence = degree_sequence[-1, ]

# Create graph from edge list
graph = graph.data.frame(degree_sequence)

# Remove loops
is_simple(graph)
graph_simple = simplify(graph, remove.multiple = TRUE, remove.loops = TRUE)
is_simple(graph_simple)

# TODO: Migrate this to a table for table 2 results
# Switching method
success_ctr = 0
fail_ctr = 0

# Create edgelist
edgelist = as_edgelist(graph_simple, names = FALSE)
is_simple(graph_from_edgelist(edgelist))

E = nrow(edgelist)
Q = log(E)
zero_deg_v = N-length(V(graph_simple))

# Create adjacency matrix (igraph and rstyle)
m = as_adjacency_matrix(graph_simple, names = FALSE)
rstyle_m <- as.matrix(m)
#rstyle_m = Matrix(m, sparse=TRUE)

# Calculating all potential switches simultaneously (faster than using runif multiple times)
u_v_id = sample(1:E, Q*E, replace = TRUE)
s_t_id = sample(1:E, Q*E, replace = TRUE)

# Perform switching - repeat Q*E times
for(i in seq(1, floor(E*Q))){

      # Extract trial edges from edge list
    u_v = edgelist[u_v_id[i], ]
    s_t = edgelist[s_t_id[i], ]
    
    # Retain original edges
    u_v_original = u_v
    s_t_original = s_t
    
    # Perform switching operation if possible
    # Check they are not the same vertices + check for multiedges + check for loops
    if( u_v[1] != s_t[1] && u_v[2] != s_t[2] && # Vertices all all different
        u_v[1] != s_t[2] && s_t[1] != u_v[2] &&  # vertices are all different
        rstyle_m[s_t[1], u_v[2]] == 0 &&  rstyle_m[u_v[2], s_t[1]] == 0 && # No loop or hyperedges
        rstyle_m[u_v[1], s_t[2]] == 0 && rstyle_m[s_t[2], u_v[1]] == 0) { # No loop or hyperedges
      
        # Make the switch
        temp = u_v[2] 
        u_v[2] = s_t[2]
        s_t[2] = temp
        
        # Reassign edges
        edgelist[u_v_id[i], ] = u_v 
        edgelist[s_t_id[i], ] = s_t 
        
        # Delete adjacencies
        rstyle_m[u_v_original[1], u_v_original[2]] = 0
        rstyle_m[s_t_original[1], s_t_original[2]] = 0
        
        # Add new adjacencies
        rstyle_m[u_v[1], u_v[2]] = 1
        rstyle_m[s_t[1], s_t[2]] = 1
        
        # Increment success counter
        success_ctr = success_ctr + 1
    } else {
        # Increment failure counter
        fail_ctr = fail_ctr + 1
    }
    
    cat("Iteration: ", i, "\n")
}

fail_ctr
success_ctr

# Create new graph from edgelist
new_graph = graph_from_edgelist(edgelist)

# Add vertices with no edges
new_graph = add_vertices(new_graph, nv=zero_deg_v)
is_simple(new_graph)

# Closeness of basque language tree
x_vec = closeness(new_graph, mode = "out", normalized = TRUE) 
x = sum(x_vec)/N
(x)
