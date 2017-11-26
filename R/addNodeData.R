#' Load node data into a CLD object
#'
#' @description This function takes a \emph{node data frame}, consisting of character
#'   variables \code{node} and (optionally) \code{group}, and populates the \code{nodes}
#'   list element in the supplied \code{CLD} object.
#'
#' @param CLD A causal loop diagram (CLD) object.
#'
#' @param ndf A node data frame containing at least a character column \code{node}.
#'   When \code{ndf} is specified, \code{edf2CLD()} loads the node information into the
#'   CLD object and checks the names of the variables contained in \code{edf$from} and
#'   \code{edf$to} for membership in \code{ndf$node}. This data frame may optionally contain
#'   a column named \code{group} to designate individual variables into groups. All other
#'   variables are ignored.
#'
#' @param replace If \code{TRUE}, \code{addNodeData()} will replace the existing
#'   \code{nodes} list element within the supplied \code{CLD} object. If \code{FALSE},
#'   the existing \code{nodes} data will be updated to include any new information
#'   embedded within \code{ndf}. Importantly, when \code{replace == TRUE}, the function
#'   verifies that all nodes represented in the \code{edges} table within the \code{CLD}
#'   object are listed in the supplied \code{ndf} (i.e., in \code{ndf$node}).
#'
#' @param sorted If \code{TRUE}, the \code{nodes} list element in the returned \code{CLD}
#'   object is sorted in order of the node name (column \code{node}).
#'
#' @return An updated CLD object.
#' @export
#'
#' @examples
#' \dontrun{
#' L <- CLD(from=c("a","a","c","c","d"), to=c("b","c","a","d","a"))
#' L$nodes
#' ndat <- tibble(node=c("c","d","e","f"), group=c("I"," ","II","I"))
#' L <- addNodeData(L, ndat)
#' L$nodes #note updating behavior - also that the missing value gets mapped to "<default>"
#' M <- addNodeData(L, ndat, replace=TRUE) #produces error
#' }
addNodeData <- function(CLD, ndf, replace=FALSE, sorted=TRUE){
  stopifnot(class(CLD) == "CLD")
  stopifnot(is.data.frame(ndf))
  stopifnot("node" %in% names(ndf))
  stopifnot(is.character(ndf$node))
  nodes <- ndf$node
  if(any(duplicated(nodes))){
    stop("Duplicate node(s) specified in the 'ndf' table.")
  }
  if("group" %in% names(ndf)){
    stopifnot(is.character(ndf$group))
    gps <- ndf$group
  } else {
    warning("No 'group' variable specified in parameter 'ndf'. Setting to \"default\".")
    gps <- rep("", length(nodes))
  }
  newNodeData <- tibble(node=nodes, group=gps)
  newNodeData$group[newNodeData$group %in% c(""," ")] <- "<default>"
  if(replace) {
    if(!allEdfNodesListedInNdf(CLD)){
      stop("CLD$edges$from and/or CLD$edges$to contain nodes that are absent in CLD$nodes.")
    }
    CLD$nodes <- newNodeData
  } else {
    CLD$nodes <- CLD$nodes %>%
      filter(!(node %in% nodes)) %>%
      bind_rows(newNodeData)
  }
  if(sorted) CLD$nodes <- CLD$nodes %>% arrange(node)
  CLD
}
