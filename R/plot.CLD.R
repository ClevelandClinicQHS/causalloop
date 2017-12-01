#' @name plot.CLD
#' @aliases plot.CLD plotCLD
#' @export
#' @method plot CLD
#'
#' @title Plot a causal loop diagram object
#' @description Causal loop diagrams are generated through an interface to the
#'   \code{DiagrammeR} package. Node and edge characteristics can be modified directly
#'   by manipulating the CLD object or by using functions \code{setEdgeFormat()} and
#'   \code{setNodeFormat()}.
#'
#' @param CLD an object of class \code{CLD}
#'
#' @details See 'Sources' for links to additional documentation from the
#'   \code{DiagrammeR} package and the GraphViz website.
#'
#' @author Jarrod Dalton
#'
#' @source
#'   \url{http://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html}\cr
#'   See especially the section on Attributes
#'
#'   \url{http://graphviz.org/}\cr
#'   \url{http://graphviz.org/content/attrs}
#'
#' @examples
#' \dontrun{
#' #* Plots may open in a browser.
#' L <- CLD(from=c("a","a","b","c","d","d"), to=c("b","c","a","d","b","a"),
#'            polarity=c(1,1,-1,-1,1,-1)) %>%
#'   addNodeData(tibble(node="c", group="core")) %>%
#'   addNodeGroup("core", fontcolor="red", color="yellow")
#' plot(L)
#' plot(L, nodes=c("c","e","f"))
#' }

plot.CLD <- function(CLD, nodes=NULL, steps = 1, recolor=TRUE) {
  stopifnot(class(CLD) == "CLD")
  #first, make sure all nodes in the edges table are in the nodes table
  if(!causalloop:::allEdfNodesListedInNdf(CLD)){
    stop("CLD$edges$from/CLD$edges$to contain nodes that are absent in CLD$nodes.")
  }
  if(!all(CLD$nodes$group %in% CLD$formats$node$group)){
    zz <- setdiff(CLD$nodes$group, CLD$formats$node$group)
    stop(paste0("Node group(s) '", paste(zz, collapse="', '"),
                "' do not exist. Use addNodeGroup()."))
  }
  #map node data to what DiagrammeR wants (integer node IDs)
  ndf <- DiagrammeR::create_node_df(n     = nrow(CLD$nodes),
                                    type  = CLD$nodes$group,
                                    label = CLD$nodes$node)
  nodeFmtData <- CLD$formats$node %>% rename(type=group)
  ndf  <- left_join(ndf, nodeFmtData, by="type")
  nodeIDs <- ndf %>% select(id,label)

  edf <- CLD$edges %>%
    left_join(CLD$formats$edge, by="polarity") %>%
    rename(label=from) %>%
    left_join(nodeIDs, by="label") %>%
    select(-label) %>%
    rename(from=id, label=to) %>%
    left_join(nodeIDs, by="label") %>%
    select(-label) %>%
    rename(to=id)

  edf <- DiagrammeR::create_edge_df(from      = edf$from,
                                    to        = edf$to,
                                    polarity  = edf$polarity,
                                    style     = edf$style,
                                    color     = edf$color,
                                    arrowhead = edf$arrowhead,
                                    penwidth  = edf$penwidthAdj*edf$weight)

  if(!is.null(nodes)){
    if(any(!(nodes %in% CLD$nodes$node))){
      warning(paste0("The following nodes were not in the CLD ",
                     "and will be ignored:\n     '",
                    paste0(setdiff(nodes, CLD$nodes$node),
                           collapse="'\n     '")), "'")
      nodes <- nodes[nodes %in% CLD$nodes$node]
    }
    if(length(nodes)==0) stop("No nodes present in the CLD!")
    nodes <- ndf$id[ndf$label %in% nodes]  #map nodes to node indices
    stopifnot(is.numeric(steps) & length(steps)==1)
    steps <- floor(steps)
    inNodes <- outNodes <- origNodes <- nodes
    if(steps>0)  for(i in 1:steps) {
      inNodes  <- unique(c(inNodes, edf$from[which(edf$to %in% inNodes)]))
      outNodes <- unique(c(outNodes, edf$to[which(edf$from %in% outNodes)]))
    }
    nodes <- unique(c(inNodes,outNodes))
    ndf <- ndf %>% filter(id %in% nodes)
    edf <- edf %>% filter(from %in% nodes & to %in% nodes)
    ix <- which(!(edf$from %in% origNodes) & !(edf$to %in% origNodes))
    if(recolor & length(ix)>0) edf$color[ix] <- "gray70"
  }

  g <- DiagrammeR::create_graph(nodes_df=ndf, edges_df=edf) %>%
    DiagrammeR::set_global_graph_attrs(attr      = "overlap",
                                       value     = "false",
                                       attr_type = "graph")
  DiagrammeR::render_graph(g)
}


