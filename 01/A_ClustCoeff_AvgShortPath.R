# Load and install necessary packages
requiredPackages <- c("igraph")

for (pac in requiredPackages) {
    if(!require(pac,  character.only=TRUE)){
        install.packages(pac, repos="http://cran.rstudio.com")
        library(pac,  character.only=TRUE)
    } 
}
rm(pac)
rm(requiredPackages)

##################################
##################################

PLOT_GRAPH = FALSE

dim = 1
size = 100
nei = 4

clusCoeff = NULL
avgShortPath = NULL


# Diff values of p to test
ps = c(0.000001, 0.000005, 0.0000075, 
       0.00001, 0.00005, 0.000075,
       0.0001, 0.0005, 0.00075,
       0.001, 0.005, 0.0075,
       0.01, 0.05, 0.075,
       0.1, 0.5, 0.75,
       1)


for (p in ps) {
    
    # Generate graph
    ws_graph <- watts.strogatz.game(dim, size, nei, p)
    if(PLOT_GRAPH){
        plot(ws_graph, layout=layout.circle, vertex.label=NA, vertex.size=3)
    }
    
    
    # Compute Clust. coeff
    (trans = transitivity(ws_graph))    ## Transitivity == clustering coeff.
    clusCoeff = append(clusCoeff, trans)
    
    # Compute  average.path.length(ws_graph)
    (avgPL = average.path.length(ws_graph))
    avgShortPath = append(avgShortPath, avgPL)
    
}


# Plot for diff. p:
# Clustering coefficient
# Avg. shortest path

# Normalize values for plotting
(avgShortPathNorm <- avgShortPath/avgShortPath[1])
(clusCoeffNorm <- clusCoeff/clusCoeff[1])

psAsFactor = as.factor(ps)

df <- data.frame(p = psAsFactor, clusteringCoeff = clusCoeffNorm) #, avgShortestPath = avgShortPath)
#df <- data.frame(p = ps, clusteringCoeff = clusCoeff) #, avgShortestPath = avgShortPath)

#  TODO 19/09/18: Plot this with ggplot beautifully instead of plot
plot(df, 
     main = "Avg path and Clust. coeff comparison for diff p",
     xlab = "p",
     ylab = "")

points(avgShortPathNorm)

