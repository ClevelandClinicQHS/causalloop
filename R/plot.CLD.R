#' @name plot.CLD
#' @export
#' @method plot HydeNetwork
#'
#' @title Plot a causal loop diagram object
#' @description Causal loop diagrams are generated through an interface to the
#'   \code{DiagrammeR} package. Node and edge characteristics can be modified directly
#'   by manipulating the CLD object or by using functions \code{setEdgeFormat()} and
#'   \code{setNodeFormat()}.
#'
#' @param x an object of class \code{HydeNetwork}
#' @param nodes (optional) character vector containing the nodes to be plotted. By default,
#'   when this parameter is specified, all nodes having links to the specified nodes are
#'   also visualized (i.e., \code{steps = 1}).
#'   between nodes.
#' @param steps (optional) Maximum number of steps away from the specified \code{nodes} to
#'   be included in the figure. Defaults to 1. Only has an effect when \code{nodes} is
#'   specified.
#' @param ... for the \code{plot} method, additional arguments to be passed to
#'   \code{DiagrammeR::render_graph}.
#'
#' @details See 'Sources' for links to additional documentation from the \code{DiagrammeR}
#'   package and the GraphViz website.
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
#' data(BlackJack, package="HydeNet")
#' plot(BlackJack)
#'
#' HydePlotOptions(variable=list(shape = "rect", fillcolor = "#A6DBA0"),
#'                 determ = list(shape = "rect", fillcolor = "#E7D4E8",
#'                               fontcolor = "#1B7837", linecolor = "#1B7837"),
#'                 decision = list(shape = "triangle", fillcolor = "#1B7837",
#'                                 linecolor = "white"),
#'                 utility = list(shape = "circle", fillcolor = "#762A83",
#'                                fontcolor = "white"))
#' plot(BlackJack)

plot.CLD <- function(x, nodes, steps = 1, ...) {
  if(missing(nodes)){  #plot the whole thing
    ndf <- DiagrammeR::create_node_df(n = nrow(x$nodes), label = x$nodes$node)
  } else {

  }

  node_df <-
    DiagrammeR::create_node_df(n = length(x[["nodes"]]),
                               label = x[["nodes"]])
  #
  # node_df <- data.frame(nodes = x[["nodes"]],
  #                       stringsAsFactors = FALSE)
  if (useHydeDefaults) node_df <- mergeDefaultPlotOpts(x, node_df)

  if (!is.null(customNodes)) node_df <- mergeCustomNodes(node_df, customNodes)

  edge_table <- do.call("rbind",
                        mapply(FUN = mapEdges,
                               x[["nodes"]],
                               x[["parents"]],
                               MoreArgs = list(node_df = node_df)))

  edge_df <- DiagrammeR::create_edge_df(from = edge_table[, 2],
                                        to = edge_table[, 1])

  if (!is.null(customEdges)) mergeCustomEdges(edge_df, customEdges)



  DiagrammeR::create_graph(nodes_df = node_df,
                           edges_df = edge_df,
                           attr_theme = NULL) %>%
    DiagrammeR::render_graph()

}
