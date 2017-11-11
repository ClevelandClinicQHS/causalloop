#' Create a causal loop diagram (CLD) object
#'
#' @param from A vector of type character, providing the variables at the beginning of
#' each (directional) causal link.
#' @param to A vector of type character, containing the variables at the end of each
#' (directional) causal link. Must be the same length as \code{from}.
#' @param polarity A numeric vector containing the edge polarities. Must be the same
#' length as \code{from} and \code{to} and must contain values in \code{c(-1,1)}.
#' Defaults to 1 for all edges.
#' @param weight A numeric vector containing the edge weights (e.g., importance
#' weights). Must be the same length as \code{from} and \code{to} and must contain
#' values in \code{1:5}. Defaults to 1 for all edges.
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
#' @export
#'
#' @examples
#' L <- CLD(from=c("a","a","c"), to=c("b","c","a"))
#' L$edf
#'
CLD <- function(from, to, polarity=1, weight=1){
  L <- list(edf = tibble::tibble(from     = from,
                                 to       = to,
                                 polarity = polarity,
                                 weight   = weight),
            ndf = tibble::tibble(node        = character(),
                                 description = character(),
                                 group       = character()))
  class(L) <- "CLD"
  return(L)
}
