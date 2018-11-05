
source("baseMetrics.R")

# graph = graph to switch, N = number of nodes, C_original = C metric of actual language data
computeSwitchingCloseness <- function(graph, N, C_original) {
  success_ctr = 0
  fail_ctr = 0
  
  # Create edgelist
  edgelist = as_edgelist(graph, names = FALSE)

  E = nrow(edgelist)
  Q = 4
  
  # Create adjacency matrix (igraph and rstyle)
  m = as_adjacency_matrix(graph, names = FALSE, type = "both")
  rstyle_m <- as.matrix(m)

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
          rstyle_m[u_v_original[2], u_v_original[1]] = 0
          rstyle_m[s_t_original[2], s_t_original[1]] = 0
          
          # Add new adjacencies
          rstyle_m[u_v[1], u_v[2]] = 1
          rstyle_m[s_t[1], s_t[2]] = 1
          rstyle_m[u_v[2], u_v[1]] = 1
          rstyle_m[s_t[2], s_t[1]] = 1
          
          # Increment success counter
          success_ctr = success_ctr + 1
      } else {
          # Increment failure counter
          fail_ctr = fail_ctr + 1
      }
    }
  cat("# Fails: ", fail_ctr, "\n")
  cat("# Success: ", success_ctr, "\n")
  
  # Create new graph from edgelist
  new_graph = graph_from_edgelist(edgelist, directed = FALSE)
  #cat("New graph is simple ",  is_simple(new_graph), "\n")
  
  ##### For computing best ordering
  #x = computeGraphClosenessBoundsOrdering(new_graph, N, C_original, N, 4)
  
  # For normal runs
  x = computeGraphClosenessBounds(new_graph, N, C_original, N/1000)
  return(x)
}


## Monte Carlo Algorithm ---- x = C metric, original_graph = language data graph, N = num nodes, T = Num iters in Monte Carlo
computeSwitchingPValue <- function(x, original_graph, N, T_it=20) {
    cat("X: ", x, "\n")
    results = c()
    # x is the closeness of the original language
    f_bin = 0
    for(i in seq(1, T_it)){
        x_nh = computeSwitchingCloseness(original_graph, N, x)
        cat("X_nh: ", x_nh, "x: ", x, "\n")
        if(x_nh >= x){
            f_bin = f_bin + 1
        }
        
        results = append(results, x_nh)
    }
    
    cat("Results switching: ", results, "\n")
    p_xnh_x = f_bin/T_it
    cat("p-value: ", p_xnh_x)
    return(p_xnh_x)
}

