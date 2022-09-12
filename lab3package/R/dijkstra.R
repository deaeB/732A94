dijkstra <-
function(graph, init_node){
  node_count <- nlevels(factor(graph[, 1])) 
  #number of all nodes
  list_spath <- array(rep(Inf, node_count)) 
  #initialize an array, stores the shortet weight to each node that you will return
  visited <- array(rep(0), node_count)
  #initialize an array, stores visited nodes
  assert_args <- is.data.frame(graph) && all(names(graph) == c("v1", "v2", "w")) && any(init_node == graph[1]) && is.numeric(init_node) && length(init_node) == 1
  stopifnot(assert_args)
  # Assert arguments
  for (i in c(1:(node_count - 1))) {
    #loop [node_count - 1] times for shortest path takes [n - 1] edges
    temp_gph <- graph[which(graph[, 1] == init_node), ]
    # shorten the graph (unnecessary)
    if (list_spath[init_node] == Inf) list_spath[init_node] <- 0
    # set first node's distance to itself(0)
    for (j in temp_gph[, 2]) {
      if (list_spath[j] == Inf) {
        list_spath[j] <- temp_gph[which(temp_gph[,2] == j), 3] + list_spath[init_node]
      }  else
        list_spath[j] <- min(temp_gph[which(temp_gph[,2] == j), 3] + list_spath[init_node], list_spath[j])
    }
    #compare the weight to next stand node by node, then update it
    visited[i] <- init_node
    init_node <- max(which(list_spath == min(list_spath[-visited])))
    #update visited list and next node to go
  }
  return(list_spath)
}
