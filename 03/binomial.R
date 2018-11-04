#####
set.seed(42)

## Monte Carlo Algorithm
computeBinomialPValue <- function(x, N, E, T_it=20) {
    # x is the closeness of the original language
    f_bin = 0
    for(i in seq(1, T_it)){
        er_graph <- sample_gnm(N, E)
    
        er_graph_simpl = simplify(er_graph, remove.multiple = TRUE, remove.loops = TRUE)
        
        disconnected_nodes = length(V(er_graph_simpl))
        # Assuming ER has the same probability of creating an edge between two vertices
        # we can take the first M nodes without ordering the structure.
        # M = 10%
        M = N*10/100
        x_nh = sum(closeness(er_graph_simpl, vids=V(er_graph_simpl)[1:M], mode = "out", normalized = TRUE))/N
        cat("X_nh: ", x_nh, "x: ", x, "\n")
        if(x_nh >= x){
            f_bin = f_bin + 1
        }
    }
    p_xnh_x = f_bin/T_it
    return(p_xnh_x)
}
