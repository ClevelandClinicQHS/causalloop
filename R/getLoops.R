#' @name getLoops
#' @export getLoops
#' @title Extract, Plot and Manipulate Labels for Feedback Loops in a Causal Loop Diagram
#'
#' @param x a CLD object
#' @param S number of loop traversals (random walks on directed graph) to be simulated,
#'   originating from each variable in the CLD.
#' @param nsteps number of steps for each loop traversal. Setting this to larger values
#'   (e.g., 10 or 15) tends to yield a larger number of identified loops, many of which
#'   involve large numbers of variables. The default value of 6 tends to yield simpler
#'   loop structures (which contain up to a maximum of 6 variables).
#' @param FBL A \code{feedbackLoops} object (i.e., object outputted from \code{getLoops()})
#' @param loopIx Loop index. When \code{getLoops()} is run, each loop is assigned an index.
#'   The function \code{setLoopLabel} will take a \code{loopIx} as input. The loop indices
#'   are always displayed in the \code{plot} method.
#' @param label Label to assign to a feedback loop in \code{setLoopLabel()}.
#'
#' @return a list of 2-element lists, each with element \code{[[1]]} containing
#'   a feedback loop and element \code{[[2]]} containing the loop's type (i.e.,
#'   either "balancing" or "reinforcing"). Balancing loops have an odd number
#'   of links with negative polarity, and reinforcing loops have an even
#'   number of links with negative polarity.
#' @export
#'
#' @details This function applies a particle-based approach to identifying loops
#'   (cycles) within a causal loop diagram.  Random walks are generated, initiated at
#'   each variable, and any cycles that are identified are retained and numbered.
#'
#' @references
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
#' loops <- getLoops(z)
#' loops <- setLoopLabel(loops, loopIx = 5, label = "BCFdE!")
#'
getLoops <- function(x, S = 100, nsteps=6){
  stopifnot("CLD" %in% class(x))
  nodeIxTable <- dplyr::select(x$nodes, node) %>%
    dplyr::group_by(node) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::ungroup() %>%
    dplyr::rename(from = node) %>%
    dplyr::mutate(to = from,
                  nodeID = 1:dplyr::n())

  edf <- dplyr::select(x$edges, from, to, polarity) %>%
    dplyr::left_join(nodeIxTable[,c("from","nodeID")], by = "from") %>%
    dplyr::rename(fromID = nodeID) %>%
    dplyr::left_join(nodeIxTable[,c("to","nodeID")], by = "to") %>%
    dplyr::rename(toID = nodeID)

  nodeID_graph <- igraph::graph_from_data_frame(edf[,c("fromID","toID")])
  L <- purrr::map(nodeIxTable$nodeID, function(x) list(node=x))
  doWalks <- function(l){
    l$walks <- plyr::rlply(S,
                           as.numeric(igraph::random_walk(nodeID_graph,
                                                          start=l$node,
                                                          steps=nsteps)))
  }
  rw <- purrr::map(L, doWalks)
  rw <- unlist(rw, recursive = FALSE)
  #trim random walks to contain only cycles
  trimWalks <- function(v) {
    endIx <- which(v[-1] == v[1])
    if(length(endIx)>0) {
      trimmedWalk <- v[1:(min(endIx)+1)]
      return(trimmedWalk[!duplicated(trimmedWalk)])
    } else return(endIx)
  }
  rw <- purrr::map(rw, trimWalks)
  rw <- rw[purrr::map(rw,length)>0]
  #standardize all loops so that they begin at the lowest numbered node
  rotateLoop <- function(v){
    minIx <- which.min(v)
    if(minIx == 1) return(v) else return(v[c(minIx:length(v), 1:(minIx-1))])
  }
  rw <- purrr::map(rw, rotateLoop)
  rw <- rw[!duplicated(rw)]
  #format the output
  rw <- purrr::map(rw, function(v) c(v, v[1]))
  rw <- purrr::map2(1:length(rw), rw, ~list(loopIx=..1, loop=..2))
  rw <- purrr::map(rw, function(l) tibble::tibble(loopIx = l$loopIx, nodeID = l$loop))
  nodeIDMap <- tibble::tibble(nodeID=1:nrow(nodeIxTable),
                              actualNodeID = as.numeric(attr(igraph::V(nodeID_graph),
                                                             "names")))
  rw <- rw %>% dplyr::bind_rows() %>%
    dplyr::left_join(nodeIDMap, by = "nodeID") %>%
    dplyr::select(-nodeID) %>%
    dplyr::rename(nodeID = "actualNodeID") %>%
    dplyr::left_join(dplyr::select(nodeIxTable, nodeID, from) %>%
                       dplyr::rename(var="from"),
                     by="nodeID")
  rw <- rw %>%
    dplyr::group_by(loopIx) %>%
    dplyr::mutate(from=var, to=lead(var)) %>%
    dplyr::filter(!is.na(to)) %>%
    dplyr::select(loopIx, from, to) %>%
    dplyr::left_join(select(edf, from, to, polarity), by=c("from", "to"))
  rw <- dplyr::left_join(rw, (rw %>% dplyr::summarize(anyNA = max(is.na(polarity)))),
                         by="loopIx") %>%
    dplyr::filter(anyNA == 0) %>%
    dplyr::select(-anyNA) %>%
    dplyr::ungroup() #%>%
  #mutate(lagLoopIx = lag(loopIx), leadLoopIx = lead(loopIx))
  #correct loop indices so that they are consecutive
  loopIxMap <- tibble::tibble(loopIx = sort(unique(rw$loopIx))) %>%
    dplyr::mutate(newLoopIx = 1:n())
  nloops <- nrow(loopIxMap)
  rw <- dplyr::left_join(loopIxMap, rw, by = "loopIx") %>%
    dplyr::select(-loopIx) %>%
    dplyr::rename(loopIx = newLoopIx)
  rw <- split(rw, rw$loopIx)

  tmp <- purrr::map2(as.list(1:nloops), as.list(rep("",nloops)),
                     function(.x, .y) list(loopIx = .x, label = .y))
  makeResult <- function(.x, .y){
    loopIx <- .x$loopIx
    label <- .x$label
    type <- ifelse(prod(.y$polarity) == -1, "Balancing", "Reinforcing")
    loop <- dplyr::select(.y, from, to, polarity)
    list(loopIx = loopIx, label = label, type = type, loop = loop)
  }
  Result <- purrr::map2(tmp, rw, makeResult)
  class(Result) <- "feedbackLoops"
  Result
}

#' @rdname getLoops
#' @export setLoopLabel

setLoopLabel <- function(FBL, loopIx, label){
  stopifnot(class(FBL) == "feedbackLoops")
  FBL <- purrr::map_if(FBL,
                       .p = function(.x) .x$loopIx == loopIx,
                       .f = function(.x) {
                         .x$label <- label
                         .x
                       }
  )
  class(FBL) <- "feedbackLoops"
  return(FBL)
}

#' @rdname getLoops
#' @export plot.feedbackLoops

plot.feedbackLoops <- function(FBL, ...){
  stopifnot(class(FBL) == "feedbackLoops")
  .dots <- list(...)
  if(all(c("nodes","loopIx") %in% names(.dots))){
    stop("Either 'nodes' or 'loopIx' can be specified, but not both.")
  }
  if("nodes" %in% names(.dots)){
    stopifnot(is.character(nodes) | is.numeric(nodes))
    #get table of nodes involved with each loop
    FBL.nodes <- purrr::map(FBL, function(l){
      tibble::tibble(loopIx = l$loopIx, nodes = c(l$cycle$from, l$cycle$to))
    })
    FBL.nodes <- FBL.nodes %>%
      dplyr::bind_rows() %>%
      dplyr::group_by(loopIx, nodes) %>%
      dplyr::filter(dplyr::row_number() == 1) %>%
      dplyr::ungroup()
    selectedLoops <- dplyr::left_join(tibble::tibble(nodes = nodes),
                                      FBL.nodes, by = "nodes") %>%
      dplyr::select(loopIx)
    FBL <- FBL[unlist(purrr::map(FBL, function(.x) .x$loopIx %in% selectedLoops$loopIx))]
  }

  if("loopIx" %in% names(.dots)){
    stopifnot(is.numeric(loopIx))
    FBL <- FBL[unlist(purrr::map(FBL, function(.x) .x$loopIx %in% loopIx))]
  }
  if(length(FBL) == 0){
    stop("No loops found involving the loop indices given in 'loopIx'!")
  }

  #make tibble containing the selected feedback loops
  loopToTibble <- function(l){
    l$cycle %>%
      dplyr::mutate(loopIx = l$loopIx,
                    type   = l$type,
                    label  = paste0(l$loopIx, ": ", l$label)) %>%
      return
  }
  pd <- purrr::map(FBL, loopToTibble) %>% dplyr::bind_rows()
  ggplot2:: ggplot(data=pd) +
    ggplot2::facet_wrap(~ label) +
    ggplot2::theme_minimal() +
    ggplot2::geom_vline(xintercept=0) +
    ggplot2::scale_x_continuous(breaks=c(-999)) +
    ggplot2::theme(axis.title.x=ggplot2::element_blank(),
          axis.text.x=ggplot2::element_blank(),
          axis.ticks.x=ggplot2::element_blank())
}

