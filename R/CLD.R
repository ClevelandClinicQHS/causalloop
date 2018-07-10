#' Create a causal loop diagram (CLD) object
#'
#' @param from A vector of type character, providing the variables at the beginning of
#' each (directional) causal link.
#' @param to A vector of type character, containing the variables at the end of each
#' (directional) causal link. Must be the same length as \code{from}.
#' @param polarity A numeric vector containing the edge polarities. Must be either
#' a scalar or a vector the same length as \code{from} and \code{to}; and must
#' contain values in \code{c(-1,1)}. Defaults to 1 for all edges.
#' @param weight A numeric vector containing the edge weights (e.g., importance
#' weights). Must be the same length as \code{from} and \code{to} and must contain
#' values in \code{1:5}. Defaults to 1 for all edges.
#' @param ... Additional arguments passed to \code{formatEdges()}.
#'
#' @return A CLD object. This is a list with the following elements:
#' \itemize{
#'    \item{\code{edf}} An edge data frame with the following columns:
#'        \itemize{
#'            \item{\code{from}} --- Source variable for the edge.
#'            Class \code{character}.
#'            \item{\code{to}} --- Destination variable for the edge.
#'            Class \code{character}.
#'            \item{\code{polarity}} --- Edge polarities. Class \code{integer}.
#'            \item{\code{weight}} --- Edge weights (e.g., importance weight).
#'            Class \code{integer}.
#'        }
#' }
#'
#' @importFrom tibble tibble
#'
#' @export
#'
#' @examples
#' L <- CLD(from=c("a","a","c","c","d"), to=c("b","c","a","d","a"))
#' L$edges
#'
#'
CLD <- function(from, to, polarity=1, weight=1){
  #set up edge tibble
  edf <- processEdgeInput(from, to)

  if(!(length(polarity) %in% c(1, nrow(edf)))){
    stop("'polarity' must be length 1 or length equal to that of 'from' and 'to")
  }
  if(!(is.numeric(polarity) && all(unique(polarity) %in% c(-1,1)))){
    stop("polarity must contain values in c(-1,1) *only*.")
  }
  edf$polarity <- polarity

  if(!(length(weight) %in% c(1, nrow(edf)))){
    stop("'weight' must be length 1 or length equal to that of 'from' and 'to")
  }
  if(!(is.numeric(weight) && all(unique(weight) > 0))){
    stop("weight must contain positive values *only*.")
  }
  edf$weight <- weight

  #set up default edge and node formats
  efmt <- dplyr::tibble(polarity    = c(1,-1),
                        style       = c("solid","dotted"),
                        color       = c("ForestGreen","red"),
                        arrowhead   = c("vee","box"),
                        penwidthAdj = c(1,2))

  nfmt = dplyr::tibble(group      = "<default>",
                       shape      = "rectangle",
                       style      = "transparent",
                       color      = "transparent",
                       fillcolor  = "transparent",
                       fontname   = "Arial Narrow",
                       fontcolor  = "black",
                       height     = 0.5,
                       width      = 0.7)

  nodes <- sort(unique(c(edf$from, edf$to)))
  ndf <- dplyr::tibble(node=nodes, group="<default>")

  L <- list(edges=edf, nodes=ndf, formats=list(edge=efmt, node=nfmt))
  class(L) <- "CLD"

  return(L)
}

