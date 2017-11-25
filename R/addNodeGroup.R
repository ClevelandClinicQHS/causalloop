#' Create a format class for a group of nodes in a causal loop diagram.
#'
#' @description This function specifies the node attributes for a group of nodes, allowing
#'   easy format management for node groups.
#'
#' @param CLD A causal loop diagram object
#' @param groups A character vector indicating the group name(s)
#' @param ... Other node attributes passed to \code{setNodeFormat()}.
#'
#' @return An updated CLD object; in particular, with updated \code{$formats$node} dataset.
#' @export
#'
#' @seealso \code{\link{setNodeFormat}}
#'
#' @examples
#' L <- CLD(from = c("a","a","c","c","d"),
#'          to   = c("b","c","a","d","a")) %>%
#'   addNodeData(tibble(node  = c("c","d","e","f"),
#'                      group = c("I"," ","II","I"))) %>%
#'   addNodeGroup(groups=c("I","II"),
#'                fillcolor="yellow",
#'                shape=c("box","ellipse"))
addNodeGroup <- function(CLD, groups, ...){
  stopifnot(class(CLD) == "CLD")
  stopifnot(is.character(groups))
  if(any(duplicated(groups))) stop("Duplicate groups specified.")
  if(any(groups %in% CLD$formats$node$group)){
    stop("Some groups already exist in the supplied CLD object.")
  }
  tmp <- CLD$formats$node %>%
    filter(group == "<default>") %>%
    mutate(dummy=1) %>%
    select(-group)
  newNodeData <- left_join(tibble(group=groups, dummy=1), tmp, by="dummy") %>%
    select(-dummy)
  CLD$formats$node <- bind_rows(CLD$formats$node, newNodeData)
  setNodeFormat(CLD, groups, ...)
}
