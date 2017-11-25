#' Convert edge data frame into a causal loop diagram (CLD) object
#'
#' @param edf An edge data frame containing at least character columns \code{from} and
#' \code{to}. This data frame may optionally contain columns \code{polarity} and/or
#' \code{weight}. See \code{help(CLD)} for restrictions on these variables. All other
#' variables are ignored.
#'
#' @param ndf An optional node data frame containing at least a character column \code{node}.
#' When \code{ndf} is specified, \code{edf2CLD()} loads the node information into the
#' CLD object and checks the names of the variables contained in \code{edf$from} and
#' \code{edf$to} for membership in \code{ndf$node}. This data frame may optionally contain
#' a column named \code{group} to designate individual variables into groups. All other
#' variables are ignored.
#'
#' @return A newly-created CLD object.
#'
#' @details The function searches \code{edf} for columns named \code{polarity} and
#' \code{weight}. If these are found, then these values are passed to \code{CLD()}
#' instead of whatever values may be passed as arguments to this function). See example.
#'
#' For information on formatting nodes separately according to the \code{group} variable,
#' see \code{help(CLD)}.
#'
#' @author Jarrod E. Dalton
#' @seealso \link{CLD()}.
#'
#' @importFrom tibble tibble
#' @export
#'
#' @examples
#' #note precedence in taking the polarity argument here
#' edf2CLD(tibble(from=c("a","a"), to=c("b","c"), polarity=-1), polarity=1)
#' \dontrun{
#'   edf2CLD(data.frame(from="a", to=c("b","c")))  #produces error
#'   edf2CLD(data.frame(from="a", to=c("b","c"), stringsAsFactors=FALSE))
#' }
edf2CLD <- function(edf, ndf, ...){
  dots <- list(...)
  stopifnot(is.data.frame(edf))
  if(!all(c("from","to") %in% names(edf))){
    stop("Parameter 'edf' must contain columns 'from' and 'to'")
  }

  polarity <- 1
  if("polarity" %in% names(dots)) polarity = dots$polarity
  if("polarity" %in% names(edf)) polarity = edf$polarity

  weight <- 1
  if("weight" %in% names(dots)) weight = dots$weight
  if("weight" %in% names(edf)) weight = edf$weight

  z <- CLD(edf$from, edf$to, polarity = polarity, weight = weight)
  if(!missing(ndf)) z <- addNodeDataToCLD(z, ndf)
  z
}
