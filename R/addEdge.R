#' @name addEdge
#' @export addEdge
#' @title Functions to manipulate the nodes and edges within a causal loop diagram
#'
#' @param CLD An object of class CLD (see \code{help(CLD)}
#'
#' @details The functions \code{setPolarity()} and \code{setWeight()}, accept either a
#' numeric vector of \code{edgeIndex} values or equal-length character vectors
#' \code{from} and \code{to}.
#'
#' @return xxx
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

setWeight <- function(CLD, edgeIndex, from, to, weight){
  #Error checking
  stopifnot(class(CLD) != "CLD")
  if(missing(edgeIndex)){
    if(missing(from) | missing(to)){
      stop("Either 'edgeIndex' or both of 'from' and 'to' must be specified.")
    }
    if(!is.character(from)) stop("Parameter 'from' must be a character vector.")
    if(!is.character(to)) stop("Parameter 'to' must be a character vector.")
    if(length(from) != length(to)){
      stop("Parameters 'from' and 'to' must be of equal length")
    }
  } else {
    #Check validity of the edge indexes
    # - Numeric vector?
    # - Supplied numbers in the set of available edge index numbers
    #   within the CLD object?)
  }
  return(1)
}

