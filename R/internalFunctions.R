#' Internal functions in the causalloop package
#'
#' @name internalFunctions
#'
#' @description
#'   \code{processEdgeInput()} checks user-inputted edge specification for
#'   CLD manipulation functions.
#'
#'   \code{allEdfNodesListedInNdf()} checks to ensure that
#'   all nodes listed in the \code{from} and \code{to} columns of the \code{edges} table
#'   within a CLD object are contained in the \code{node} column within the \code{nodes}
#'   table.
#'
#' @param from A vector of type character, providing the variables at the beginning of
#'   each (directional) causal link.
#' @param to A vector of type character, containing the variables at the end of each
#'   (directional) causal link. Must be the same length as \code{from}.
#' @param CLD a causal loop diagram (CLD) object.
#'
#' @details The parameters \code{from} and \code{to} must either be of equal length; or
#'   be of differing lengths, under the restriction that at least one of the two is of
#'   length 1.
#'
#' @return data frame containing the following variables:
#'
#' @examples
#' processEdgeInput(from=c("a","a"), to=c("b")) #duplicates are removed w/ warning
#' processEdgeInput(from="a", to=c("b","c","d")) #replicates as in data.frame()
#' processEdgeInput(from=c("b","c","d"), to="z") #replicates as in data.frame()
#' processEdgeInput(from=c("a","b"), to=c("b","a","c")) #produces error as in data.frame()

processEdgeInput <- function(from, to){
  if(!is.character(from)) stop("Parameter 'from' must be a character vector.")
  if(!is.character(to)) stop("Parameter 'to' must be a character vector.")
  if(length(from) != length(to)){
    if(!(1 %in% c(length(from), length(to)))){
      stop("When different lengths for 'from' and 'to' are given, one must be length 1.")
    }
  }
  edf <- tibble::tibble(from=from, to=to)
  dupIx <- which(!duplicated(edf))
  if(length(dupIx) != nrow(edf)){
    warning("Duplicate edges were specified. These were removed from the output.")
    edf <- edf[dupIx,]
  }
  edf
}

#' @rdname internalFunctions

allEdfNodesListedInNdf <- function(CLD){
  stopifnot(class(CLD) == "CLD")
  edfNodes <- unique(c(CLD$edges$from, CLD$edges$to))
  all(edfNodes %in% CLD$nodes$node)
}
