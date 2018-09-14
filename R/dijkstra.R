#' Implement Dijkstras algorithm.
#'
#' The algorithm takes a graph and an initial node and calculates the
#' shortest path from the initial node to every other node in the graph.
#'
#' @param graph A data.frame with three variables (v1, v2 and w) that
#' contains the edges of the graph (from
#' v1 to v2) with the weight of the edge (w).
#' @param init_node A numeric scalar that exist in the graph.
#'
#' @return The shortest path to every other node from the starting node as a vector.
#'
#' @examples
#' data(wiki_graph)
#' dijkstra(wiki_graph, 1)
#' dijkstra(wiki_graph, 5)
#'
#' @export
dijkstra <- function(graph, init_node) {

  if (!requireNamespace("plyr", quietly = TRUE)) {
    stop("plyr must be installed", call. = FALSE)
  }

  # check there are 3 variables in graph
  if (ncol(graph) != 3) {
    stop("There is missing column of graph.")
  }
  # check the varibles names are all right
  if (any(names(graph) != c("v1", "v2", "w"))) {
    stop("The column name of graph is wrong.")
  }
  # check every column is numeric
  if (!is.numeric(graph[, 1])) {
    stop("v1 has to be numeric")
  }
  if (!is.numeric(graph[, 2])) {
    stop("v2 has to be numeric")
  }
  if (!is.numeric(graph[, 2])) {
    stop("w has to be numeric")
  }

  # find total numbers
  result_num <- unique(graph[, 1])
  # find number's length
  result_length <- length(result_num)
  # result vector for return
  result <- rep(0, result_length)

  # ckeck init_node is exists in graph
  if (!(init_node %in% result_num)) {
    stop("init_node has to be exists in graph!")
  }

  #=== make a chain table for every route
  original_chain <- graph[, 1:2]
  i <- which(graph[, 1] == init_node)
  chain <- graph[i, 1:2]

  # mapping graph for each route
  for (time in 1:(result_length-3)) {
    names(original_chain) <- c(names(chain[ncol(chain)]), paste0("v",ncol(chain) + 1))

    chain <- suppressMessages(plyr::join(chain, original_chain))
    chain <- chain[-which(chain[, ncol(chain)] == chain[, ncol(chain)-2]), ]
  }

  ncol_chain <- ncol(chain)

  # delete repeat routes
  delete_rows <- c()
  for (i in 1:nrow(chain)) {
    for (j in 1:(ncol_chain-2)) {
      col <- which(chain[i, (j+1):ncol_chain] == chain[i, j])
      if (length(col) != 0) {
        delete_rows <- c(delete_rows, i)
        break
      }
    }
  }
  chain <- chain[-delete_rows, ]
  #===

  # function for sum up each route
  check_sum <- function(graph, chain) {
    chain_sum <- 0
    for (i in 1:(length(chain)-1)) {
      chain_sum <- chain_sum + graph[which(graph[, 1] == chain[i] & graph[, 2] == chain[i+1]), 3]
    }
    return(chain_sum)
  }

  # find every route for each end node
  for (num in setdiff(result_num, init_node)) {
    chain_sum_vector <- c()
    for (i in 1:nrow(chain)) {
      chain_end <- which(chain[i,1:ncol_chain] == num)
      if (length(chain_end) == 0) {
        next()
      }
      a_chain <- as.numeric(chain[i, 1:chain_end])
      chain_sum_vector <- c(chain_sum_vector, check_sum(graph, a_chain))
    }
    result[num] <- min(chain_sum_vector)
  }

  return (result)
}

#' @importFrom plyr join
NULL
