

computeSwitchingCloseness <- function(graph, N) {
  success_ctr = 0
  fail_ctr = 0
  
  # Add vertices with no edges
  zero_deg_v = N-length(V(graph))
  graph = add_vertices(graph, nv=zero_deg_v)
  
  # Create edgelist
  edgelist = as_edgelist(graph, names = FALSE)
  is_simple(graph_from_edgelist(edgelist))
  
  E = nrow(edgelist)
  Q = log(E)
  
  # Create adjacency matrix (igraph and rstyle)
  m = as_adjacency_matrix(graph, names = FALSE)
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
      
      #cat("Iteration: ", i, "\n")
  }
  cat("# Fails: ", fail_ctr, "\n")
  cat("# Success: ", success_ctr, "\n")
  
  # Create new graph from edgelist
  new_graph = graph_from_edgelist(edgelist)
  cat("New graph is simple ",  is_simple(new_graph), "\n")
  
  # Closeness of basque language tree
  x_vec = closeness(new_graph, mode = "out", normalized = TRUE) 
  x = sum(x_vec)/N
  return(x)
}


## Monte Carlo Algorithm
computeSwitchingPValue <- function(x, original_graph, N, T_it=20) {
    # x is the closeness of the original language
    f_bin = 0
    for(i in seq(1, T_it)){
        x_nh = computeSwitchingCloseness(original_graph, N)
        cat("X_nh: ", x_nh, "x: ", x, "\n")
        if(x_nh >= x){
            f_bin = f_bin + 1
        }
    }
    p_xnh_x = f_bin/T_it
    return(p_xnh_x)
}
