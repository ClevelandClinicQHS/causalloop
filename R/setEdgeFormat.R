#' Functions to format nodes and edges in a causal loop diagram (CLD)
#'
#' @param CLD An object of class CLD
#'
#' @param property The property of the edge or node being manipulated. Allowable values
#' are provided in the details.
#'
#' @param value Value of the parameter to be assigned. Note that this is not validated
#' against what values are allowed by GraphViz. See the documentation for GraphViz
#' for a list of allowable values.
#'
#' @param polarity Polarity of edges for which the edge format is being manipulated.
#' When this parameter is not specified, \code{setEdgeFormat()} will apply the format
#' change to both positive and negative links (with warning).
#'
#' @param group Character vector of group label(s) of nodes for which the node
#' format is being manipulated.  When this parameter is not specified,
#' \code{setNodeFormat()} applies the formatting to all node groups that are
#' currently included in the \code{CLD} object (under \code{$formats$node$group}).
#'
#' @param ... Format parameters as described in the details section below.
#'
#' @details For \code{setEdgeFormat()}, the following parameters are acted upon (all
#' others are ignored):
#' \itemize{
#'    \item{\code{style}} --- Line style (e.g., \code{"solid"} or \code{"dotted"})
#'    \item{\code{color}} --- Line color
#'    \item{\code{arrowhead}} --- Arrowhead style (e.g., \code{"vee"}, \code{"normal"},
#'    \code{"box"}, or \code{"diamond"}).
#'    \item{\code{penwidthAdj}} ---  Pen width adjustment factor. This gets multiplied
#'    by the edge weight to establish the thickness ("pen width") of each line.
#' }
#' For \code{setNodeFormat()}, the following parameters are acted upon (all
#' others are ignored):
#' \itemize{
#'    \item{\code{shape}} --- Node shape (e.g., \code{"rectangle"}, \code{"oval"},
#'    \code{"box"} or \code{"egg"})
#'    \item{\code{style}} --- Node line style (e.g., \code{"solid"}, \code{"dashed"},
#'    \code{"dotted"} or \code{"bold"})
#'    \item{\code{color}} --- Line color for the node shape
#'    \item{\code{fillcolor}} --- Fill color for the node
#'    \item{\code{fontname}} --- Font for the node label
#'    \item{\code{fontcolor}} --- Font color for the node label
#'    \item{\code{height}} --- Base height of the node (all graphs are plotted with
#'    \code{fixedsize = FALSE}, such that height and width change to accomodate the
#'    node labels)
#'    \item{\code{width}} --- Base width of the node (all graphs are plotted with
#'    \code{fixedsize = FALSE}, such that height and width change to accomodate the
#'    node labels)
#' }
#' Default attributes are adopted unless the attributes are specified through the
#' function.
#'
#' @return An updated CLD object.
#'
#' @examples
#' L <- CLD(from = c("a","a","c"),
#'          to   = c("b","c","a")) %>%
#'   setEdgeFormat("color", "midnightblue", polarity=-1) %>%
#'   setNodeFormat("fillcolor", "yellow")
#' #plot(L)
#' @export setEdgeFormat

setEdgeFormat <- function(CLD, property, value, polarity){
  stopifnot(class(CLD) == "CLD")
  stopifnot(property %in% c("style","color","arrowhead","penwidthAdj"))
  if(missing(polarity)){
    warning("No 'polarity' value was specified. Applying format change to +/- links.")
    CLD$formats$edge[,property] <- value
  } else{
    stopifnot(polarity %in% c(-1,1))
    CLD$formats$edge[polarity,property] <- value
  }
  return(CLD)
}

#' @rdname setEdgeFormat
#' @export setNodeFormat

setNodeFormat <- function(CLD, groups, ...){
  nattribNames <- c("shape","style","color","fillcolor",
                    "fontname","fontcolor","height","width")
  stopifnot(class(CLD) == "CLD")
  dots <- list(...)
  dots <- dots[names(dots) %in% nattribNames]

  if(missing(groups)){
    if(nrow(CLD$formats$node) > 1){
      warning("No 'group' values were specified. Applying format change to all groups.")
    }
    for(nattrib in names(dots)){
      nattribVal <- dots[[nattrib]]
      stopifnot(is.vector(nattribVal))
      if(length(nattribVal) != 1){
        stop(paste("Parameter",nattrib,"must be of length 1 when no groups are specified."))
      }
      CLD$formats$node[,nattrib] <- nattribVal
    }
  } else{
    stopifnot(is.character(groups))
    stopifnot(all(!duplicated(groups)))
    if(!all(groups %in% CLD$formats$node$group)){
      stop('Not all values in "groups" are defined in the CLD object. See ?addNodeGroup')
    }
    for(nattrib in names(dots)){
      nattribVals <- dots[[nattrib]]
      stopifnot(is.vector(nattribVals))
      if(length(nattribVals) == 1){
        if(length(groups) > 1) {
          warning(paste0("Applying ", nattrib, "=", nattribVals,
                         " to all node groups."))
        }
      } else if(length(nattribVals) != length(groups)){
        stop(paste("Parameter", nattrib,
                   "must be length 1 or have length equal to length(groups)."))
      }
      tmp <- dplyr::tibble(group=groups)
      tmp[,nattrib] <- nattribVals
      modifiedGpNodeData <- CLD$formats$node[,names(CLD$formats$node) != nattrib] %>%
        dplyr::filter(group %in% groups) %>%
        dplyr::left_join(tmp, by="group") %>%
        dplyr::select("group", nattribNames)
      otherGpNodeData <- CLD$formats$node %>%
        dplyr::filter(!(group %in% groups))
      CLD$formats$node <- dplyr::bind_rows(modifiedGpNodeData, otherGpNodeData) %>%
        dplyr::arrange(group)
    }
  }
  CLD
}

