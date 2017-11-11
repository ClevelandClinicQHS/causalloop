#' Functions to format nodes and edges in a causal loop diagram (CLD)
#'
#' @param CLD An object of class CLD
#' @param edge.style.pos Style of edge lines for links with positive ("+") polarity
#' @param edge.style.neg Style of edge lines for links with negative ("-") polarity
#' @param edge.color.pos Edge color for "+" links
#' @param edge.color.neg Edge color for "-" links
#' @param edge.arrowhead.pos Arrowhead type for "+" links
#' @param edge.arrowhead.neg Arrowhead type for "-" links
#' @param edge.penwidthAdj.pos Multiplicative adjustment factor to edge pen width
#' for "+" links. This is calculated as \code{CLD$edf$weight *edge.penwidthAdj.pos}.
#' @param edge.penwidthAdj.neg Multiplicative adjustment factor to edge pen width
#' for "-" links. This is calculated as \code{CLD$edf$weight *edge.penwidthAdj.neg}.
#'
#'
#' @return An updated CLD object.
#'
#' @examples
#' @export formatEdges

formatEdges <- function(CLD,
                        edge.style.pos = "solid",
                        edge.style.neg = "dotted",
                        edge.color.pos = "ForestGreen",
                        edge.color.neg = "red",
                        edge.arrowhead.pos = "vee",
                        edge.arrowhead.neg = "box",
                        edge.penwidthAdj.pos = 1,
                        edge.penwidthAdj.neg = 1){
  stopifnot(class(CLD) == "CLD")
  negIx <- which(CLD$edf$polarity == (-1))
  CLD$edf$style     <- edge.style.pos
  CLD$edf$color     <- edge.color.pos
  CLD$edf$arrowhead <- edge.arrowhead.pos
  CLD$edf$penwidth  <- CLD$edf$weight*edge.penwidthAdj.pos
  CLD$edf$style[negIx]     <- edge.style.neg
  CLD$edf$color[negIx]     <- edge.color.neg
  CLD$edf$arrowhead[negIx] <- edge.arrowhead.neg
  CLD$edf$penwidth[negIx]  <- CLD$edf$weight*edge.penwidthAdj.neg
  return(CLD)
}

#' @rdname formatEdges
#' @export formatNodes

formatNodes <- function(ndf){
  ndf$shape <- "rectangle"
  ndf$style <- "transparent"
  ndf$color <- "transparent"
  ndf$fillcolor  <- "transparent"
  ndf$fontname <- "Arial Narrow"
  ndf$fontcolor <- "black"
  ndf$width <- 0.7
  return(ndf)
}

#' @rdname formatNodes
