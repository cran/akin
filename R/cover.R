#'@title Calculate The Number Of Combinations Of String Covering
#'
#'@description Calculates the number of subset family (\emph{covering}) combinations for common substrings
#' identified by function [fcommon].
#'
#'@param x integer, length = 1 or character vector, length > 1, or character string
#'@param wplot logical, FALSE. When TRUE, displays a plot of combinations versus number of characters in substring
#'
#'@details This function helps in deciding the parameters for parallel tasks of function [fcommon]. It finds the
#' total, the minimum and the maximum of number of combinations processed by [fcommon] for individual substring covering.
#'
#' Since at peak workload each worker receives by default 1e6 combinations for processing (see argument \code{...} in [fcommon]
#' documentation), the number of workers in a \link[future]{plan} is effectively decided taking the maximum number of
#' combinations as reference. The \code{min:max} range relates to workers idling outside the peak: setting a plan with too
#' many workers would render many of them idle during [fcommon]'s parallel run. On another hand, setting it with too few, may
#' challenge the default workload limit (see argument \code{maxSize} in [fcommon] documentation).
#'
#' When \code{x} is an integer, it must equal either the \code{nchar(string)} or the \code{length(character)}.
#'
#'@returns A prettified named vector showing the total, minimum and maximum number of combinations for respective
#' string, along with a plot (\code{wplot = TRUE}).
#'
#'@seealso [fcommon]
#'
#'@keywords Proteomics
#'
#'@export
#'
#'@examples
#'
#' if (interactive()) {
#'
#' # 1. x is a character
#'
#' # 1.1 A string
#' x = 'tyrvvsvltvlhqdwlngkeykck'
#'
#' a = cover(x)
#' print(a)
#'
#' # 1.2 A vector
#' x = strsplit(x, '')[[1]]
#'
#' b = cover(x)
#' print(b)
#'
#' # 2 An integer
#'
#' n = length(x)
#' c = cover(n, TRUE)
#' print(c)
#'
#' }
#'
#'

cover = function(x, wplot = FALSE) {
         N = if (is.character(x)) {
             if (length(x) == 1L && isTRUE(nzchar(x)) && nchar(x) > 1L) {nchar(x)}
             else if (length(x) > 1L) {length(x)}
         } else if (is.numeric(x)) {as.integer(x)}
         xa = 2:(N-1)
         a = sapply(xa, \(i) as.double(comboCount(N, i)))
         if (wplot) {eval(plotish)}
         b = structure(c(sum(a, na.rm = TRUE), min(a, na.rm = TRUE) , max(a, na.rm = TRUE)), names = c('total', 'min', 'max'))
        format(b, scientific = TRUE, justify = 'none', digits = 3)
}
