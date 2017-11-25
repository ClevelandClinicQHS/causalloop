#' @name addEdge
#' @export addEdge
#' @title Functions to manipulate the nodes and edges within a causal loop diagram
#'
#' @param CLD An object of class CLD (see \code{help(CLD)}
#' @param edgeIndex A numeric vector of edge indices from the CLD object.
#' @param from A vector of type character, providing the variables at the beginning of
#' each (directional) causal link.
#' @param to A vector of type character, containing the variables at the end of each
#' (directional) causal link. Must be the same length as \code{from}.
#' @param polarity A numeric vector containing the edge polarities. Must be the same
#' length as \code{from} and \code{to} and must contain values in \code{c(-1,0,1)}.
#' Defaults to 1 for all edges.
#' @param weight A numeric vector containing the edge weights (e.g., importance
#' weights). Must be the same length as \code{from} and \code{to} and must contain
#' values in \code{1:5}. Defaults to 1 for all edges.
#' @param ... additional arguments to \code{formatEdges()} or \code{formatNodes}. See details.
#'
#' @details The functions \code{setPolarity()} and \code{setWeight()}, accept either a
#' numeric vector of \code{edgeIndex} values or equal-length character vectors
#' \code{from} and \code{to}. Polarity values of 0 represent situations where the
#' direction of the relationship may be different for different subgroups.
#'
#' @return An updated CLD object.
#'
#' @author Jarrod E. Dalton
#'
#' @examples
#' addEdge(1)
addEdge <- function(CLD){
  return(1)
}

#' @rdname addEdge
#' @export addNode

addNode <- function(CLD){
  return(1)
}

#' @rdname addEdge
#' @export mediate

mediate <- function(CLD){
  return(1)
}

#' @rdname addEdge
#' @export removeEdge

removeEdge <- function(CLD){
  return(1)
}

#' @rdname addEdge
#' @export removeNode

removeNode <- function(CLD){
  return(1)
}

#' @rdname addEdge
#' @export setPolarity

setPolarity <- function(CLD, edgeIndex, from, to, polarity){
  return(1)
}

#' @rdname addEdge
#' @export setWeight

setWeight <- function(CLD, edgeIndex, from, to, weight, ...){
  #Error checking
  stopifnot(class(CLD) != "CLD")
  if(missing(edgeIndex)){
    if(missing(from) | missing(to)){
      stop("Either 'edgeIndex' or both of 'from' and 'to' must be specified.")
    }
    newEdges <- processEdgeInput(from, to)
  } else {
    #Check validity of the edge indexes
    # - Numeric vector?
    # - Supplied numbers in the set of available edge index numbers
    #   within the CLD object?)
  }
  return(1)
}

