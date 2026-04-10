#' @title Split Data Vertically Into Subsets
#'
#' @description Splits data into unequal and disjoint groups of columns (i.e. vertical splits)
#'
#' @param data a "data.table" class data frame or convertible to "data.table" class
#' @param splits integer, length 1, of value <= \code{ncol(data)}. The number of disjoint
#'   selections (i.e. vertical splits). When \code{splits = 0}, there are no splits
#'
#' @details The smaller the \code{splits} value, the wider the column groups. Column order from
#' source data is not preserved
#'
#' @returns An exhaustive \link[listenv]{listenv} of disjoint column groups from original data
#'
#' @keywords splitting
#'
#' @export
#'
#'@examples
#'
#' if (interactive()) {
#'
#' # 1. Split iris data vertically
#'
#' data(iris)
#' a = splitV(iris, splits = 3)     # split data in 3 column groups
#' class(a)                         # listenv, environment
#' print(as.list(a))                # list
#'
#'}


splitV = function(data, splits) {
                dt = if (!is.data.table(data)) as.data.table(data) else data
              l.dt = length(dt); n.dt = names(dt)
            rename = match.fun(rename, descend = FALSE)
               stopifnot('splits value is larger than total number of columns' = splits <= l.dt)
                 y = data.table(x = n.dt, s = if (splits > 0) 1:l.dt %% splits + 1L else 1L)[order(s)]
                ll = listenv()
            for (i in unique(y$s)) {
              nmes = y[s == i]$x
                 j = as.call(c(quote(list), eval(rename(nmes), list(nmes = I(nmes)))))
           ll[[i]] = dt[, j, env = list(j = j)]}
           return(ll)
 }
