#' Extract Feedback Loops from a Causal Loop Diagram
#'
#' @param CLD a CLD object
#'
#' @return a list of 2-element lists, each with element \code{[[1]]} containing
#'   a feedback loop and element \code{[[2]]} containing the loop's type (i.e.,
#'   either "balancing" or "reinforcing"). Balancing loops have an odd number
#'   of links with negative polarity, and reinforcing loops have an even
#'   number of links with negative polarity.
#' @export
#'
#' @details This function applies Johnson's algorithm for finding cycles
#'   in a directed graph. In particular, we adapt for the \code{causalloop}
#'   package Jacobien Carstens' implementation of Johnson's algorithm,
#'   which can be found on GitHub at the link below (see References)
#'
#' @references
#'   Johnson, D. B. (1975). Finding all the elementary circuits of a
#'   directed graph. SIAM Journal on Computing, 4(1), 77-84.
#'
#'   Carstens, J. R implementation of Johnson's algorithm.
#'   \url{https://github.com/queenBNE/DirectedGraphsCycles}
#
#'
#' @examples
#' edges <- tibble::tribble(
#' ~from, ~to, ~polarity,
#' 1,2,1,
#' 1,5,1,
#' 1,8,1,
#' 2,3,-1,
#' 2,7,1,
#' 2,9,1,
#' 3,1,1,
#' 3,2,1,
#' 3,4,1,
#' 3,6,-1,
#' 4,5,1,
#' 5,2,1,
#' 6,4,1,
#' 8,9,-1,
#' 9,8,1
#' )
#' txtedges <- edges %>% dplyr::mutate(from = letters[from], to = letters[to])
#' z <- CLD(txtedges$from, txtedges$to, polarity = txtedges$polarity)
#' getLoops(z)
#'
getLoops <- function(CLD){

  # make:
  #  1) a lookup table from node labels in CLD$edges to integer nodeIDs; and
  #  2) an igraph object with the graph structure (index by the integer nodeIDs)
  if(!causalloop:::allEdfNodesListedInNdf(CLD)) {
    stop("The 'edges' table in the CLD object contains nodes not in the 'nodes' table.")
  }
  nodeIxTable <- dplyr::select(CLD$nodes, node) %>%
    dplyr::group_by(node) %>%
    dplyr::filter(row_number() == 1) %>%
    dplyr::ungroup() %>%
    dplyr::rename(from = node) %>%
    dplyr::mutate(to = from,
                  nodeID = 1:n())

  edf <- dplyr::select(CLD$edges, from, to, polarity) %>%
    dplyr::left_join(nodeIxTable[,c("from","nodeID")], by = "from") %>%
    dplyr::rename(fromID = nodeID) %>%
    dplyr::left_join(nodeIxTable[,c("to","nodeID")], by = "to") %>%
    dplyr::rename(toID = nodeID)

  nodeID_graph <- igraph::graph_from_data_frame(edf[,c("fromID","toID")])


  # This function, adapted from Carstens' R implementation of Johnson's algorithm,
  # computes the "elementary" cycles of a directed graph g.
  get_elementary_cycles <- function(g){
    #Subroutine to obtain the strong component of a graph g with the lowest-numbered
    #vertex in the subgraph of g induced by vertices {s, s+1, ... n}. The algorithm
    #is given in Johnson (1975) (see references).
    get.induced.strong <- function(g, s){
      # Create the induced subgraph on {s, s+1, ..., n} and compute the strong components
      sg <- igraph::induced.subgraph(g, vids=s:igraph::vcount(g))
      sc <- igraph::clusters(sg, mode="strong")

      # Obtain the names for the remaining nodes - this has to be done to make sure that we use
      # the right order of nodes, we want to find the strong component with the least vertex.
      # Igraph always uses ids 1:n' for a graph, so we need to use the names.
      ids <- as.numeric(igraph::get.vertex.attribute(sg, "name", 1:igraph::vcount(sg)))
      order <- sort(ids, index.return=TRUE)$ix

      # Obtain the vertices of the strong component with the least vertex
      others <- c()
      for(v in order)
        if(length(others) <= 1)
          others <- which(sc$membership == sc$membership[v])

      # If there is a strong component with more than 1 vertex, return this component
      if(length(others) > 1)
        return(igraph::induced.subgraph(sg, others))
      # Else return NULL
      else
        return(NULL)
    }

    # Subroutine returning a list where u is unblocked and all vertices in B(u)
    # are unblocked (recursively)
    unblock <- function(u, b, B){
      b[u] <- FALSE
      for(w in B[[u]]){
        B[[u]] <- B[[u]][-which(B[[u]]==w)]
        if(b[w]){
          bB <- unblock(w,b,B)
          b <- bB$b
          B <- bB$B
        }
      }
      return(list(b=b, B=B))
    }

    # Subroutine to produce the circuits (cycles) starting at vertex s
    circuit <- function(s, v, Ak, B, b, f, stack, ids){
      stack <- c(stack, v)
      b[v] <- TRUE
      for(w in igraph::neighbors(Ak, v, mode="out")){
        if(w==s){
          cat(sapply(c(stack,s), FUN=function(i){return(ids[i])}), sep=" ")
          cat("~")
          f = TRUE
        }else if (!b[w]){
          updated <- circuit(s,w,Ak,B,b,f,stack,ids)
          B <- updated$B
          b <- updated$b
          stack <- updated$stack
          if(updated$f)
            f = TRUE
        }
      }
      if(f){
        updated <- unblock(v, b, B)
        b <- updated$b
        B <- updated$B
      }else{for(w in igraph::neighbors(Ak, v, mode="out"))
        if (! v %in% B[[w]])
          B[[w]] <- c(B[[w]], v)
      }
      stack <- stack[-length(stack)]
      return(list(B=B, b=b, f=f, stack=stack, foundCycles=foundCycles))
    }

    b <- rep(FALSE, igraph::vcount(g))
    B <- vector("list", igraph::vcount(g))
    s = 1

    while(s < igraph::vcount(g)){
      Ak <- get.induced.strong(g,s)
      if(!is.null(Ak)){
        ids <- as.numeric(igraph::get.vertex.attribute(Ak, "name", 1:igraph::vcount(Ak)))
        s <- min(ids)
        for(i in ids){
          b[i] <- FALSE
          B[[i]] <- numeric(0)
        }
        s_indx <- which(ids == s)
        circuit(s_indx, s_indx, Ak, B, b, FALSE, numeric(0), ids)
        s <- s + 1
      }else
        s <- igraph::vcount(g)
    }
  }
  nodeID_graph_cycles <- capture.output(get_elementary_cycles(nodeID_graph))
  nodeID_graph_cycles <- as.list(strsplit(nodeID_graph_cycles, "~")[[1]])
  txt2vec <- function(l) strsplit(l, split = " ") %>% unlist() %>% as.numeric()
  nodeID_graph_cycles <- purrr::map(nodeID_graph_cycles, txt2vec)

  nodeIxTable <- dplyr::select(nodeIxTable, nodeID, from) %>%
    dplyr::rename(node = from)

  makeCycleListOutput <- function(l){
    cycle <- tibble::tibble(fromID = l[1:(length(l)-1)],
                            toID   = l[2:length(l)]) %>%
      dplyr::left_join(edf, by=c("fromID","toID")) %>%
      dplyr::select(from, to, polarity)
    type = dplyr::case_when(prod(cycle$polarity) == -1 ~ "Balancing",
                            prod(cycle$polarity) == +1 ~ "Reinforcing")
    return(list(cycle = cycle,
                type = type))
  }
  purrr::map(nodeID_graph_cycles, makeCycleListOutput)
}
getLoops(zz)
