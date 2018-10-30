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

degree_sequence = degree_sequence[-1, ]
graph = graph.data.frame(degree_sequence)


# Remove loops
is_simple(graph)
graph_simple = simplify(graph, remove.multiple = TRUE, remove.loops = TRUE)
is_simple(graph_simple)


    
# TODO: Migrate this to a table for table 2 results
# Switching method


check_adjacency <- function(v1, v2, names, m) {
    
    s_midx = which(v1, names)
    t_midx = which(v2, names)
    if(m[s_midx, t_midx] == 1 || m[t_midx, s_midx] == 1){
        return(TRUE)       
    }
    return(FALSE)
}



change_adjacency <- function(v1, v2, names, m, value) {
    
    s_midx = which(v1, names)
    t_midx = which(v2, names)
    m[s_midx, t_midx] = value
    m[t_midx, s_midx] = value
    
    return(m)
}

success_ctr = 0
fail_ctr = 0

edgelist = as_edgelist(graph_simple)
#edgel = as_adj_edge_list(graph_simple)
Q = log10(E)
E = nrow(edgelist)

m = as_adjacency_matrix(graph_simple)
names = colnames(m)

g_tmp = graph_simple

for(i in seq(0, floor(E*Q))){
    
    u_v_id = floor(runif(1) * E) + 1
    s_t_id = floor(runif(1) * E) + 1
    
    u_v = edgelist[u_v_id, ]
    s_t = edgelist[s_t_id, ]
    
    u_v_original = u_v
    s_t_original = s_t
    
    
    
    # Switching
    #  Check they are not the same vertices + check for multiedges + check for loops
    #if(u_v[1] != s_t[1] && u_v[2] != s_t[2] && !are_adjacent(g_tmp,  s_t[1], u_v[2]) && !are_adjacent(g_tmp, u_v[2], s_t[1])) {
    if(u_v[1] != s_t[1] && u_v[2] != s_t[2] && !check_adjacency(s_t[1], u_v[2], names, m) && !check_adjacency(u_v[2], s_t[1], names, m)) {
        temp = u_v[2] 
        u_v[2] = s_t[2]
        s_t[2] = temp
        
        # Reassign edges
        edgelist[u_v_id, ] = u_v 
        edgelist[s_t_id, ] = s_t 
        
        # Modifying the original graph
        # are_adjacent(g_tmp, u_v_original[1], u_v_original[2])
        # are_adjacent(g_tmp, s_t_original[1], s_t_original[2])
        #e1 = paste(u_v_original[1], "|", u_v_original[2], sep="")
        #e2 = paste(s_t_original[1], "|", s_t_original[2], sep="")
        
        
        # Delete adjacencies
        m = change_adjacency(u_v_original[1], u_v_original[2], names, m, 0)
        m = change_adjacency(s_t_original[1], s_t_original[2], names, m, 0)
        
        # Add new adjacencies
        m = change_adjacency(u_v[1], u_v[2], names, m, 1)
        m = change_adjacency(s_t[1], s_t[2], names, m, 1)
        
        success_ctr = success_ctr + 1
    } else {
        fail_ctr = fail_ctr + 1
    }
    
    cat("Iteration: ", i, "\n")
}

fail_ctr
success_ctr

new_graph = graph_from_edgelist(edgelist)
is_simple(new_graph)

x_vec = closeness(graph_simple) # Closeness of basque language tree
x = sum(x_vec)/N

x_vec2 = closeness(new_graph) # Closeness of switching model
x2 = sum(x_vec2)/N

x
x2


g = make_ring(5)
g = set_vertex_attr(g, "name", value = c("First", "Sec", "Thrd", "Frth", "Fifth"))
m = as_adjacency_matrix(g)

names = colnames(m)

match("Sec", names)
as.data.frame(get.adjedgelist(g))
