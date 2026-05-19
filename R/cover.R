#'@title Calculate The Number Of Combinations Of String Covering
#'
#'@description Calculates the cardinality of substring family (\emph{the covering}) and returns valid elements.
#'
#'@param x integer, length = 1 or character vector, length > 1, or character string
#'@param valid. logical, default, FALSE. When TRUE, valid substrings are returned. Requires character or string
#'@param wplot logical, FALSE. When TRUE, displays a plot of combinations versus number of characters in string
#'
#'@details This function finds the total, the minimum and the maximum number of combinations of individual string
#' covering or returns a subset of valid substrings (i.e. elements of the string) by retaining even transpositions
#' inside the combinations matrix. Very long strings may result in memory allocation error when \code{valid. = TRUE}.
#' Use [fcommon] instead.
#'
#' NOTE 1: Herein, the \emph{covering} represents the substring family that preserves the order of characters inside
#' the string. Hence, the use of combinations instead of partial permutations, \code{n!/(n-r)!}.
#'
#' NOTE 2: When \code{x} is an integer associated with an existing string, \code{x} must equal either \code{nchar(string)}
#' or \code{length(character)}.
#'
#'@returns When \code{valid. = FALSE} a prettified named vector showing the total, minimum and maximum number of
#' combinations for respective string covering. When \code{valid. = TRUE}, a character vector of valid substrings
#' longer than 2 characters each. In both cases, a plot is returned when \code{wplot = TRUE}.
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
#' # 2. An integer
#'
#' n = length(x)
#' c = cover(n)
#' print(c)
#'
#' # 3. Valid substrings with plot
#'
#' d = cover(x, valid. = TRUE, wplot = TRUE)
#' print(head(d, 30))
#'
#' }
#'
#'

cover = function(x, valid. = FALSE, wplot = FALSE) {
         N = if (is.character(x)) {
             if (length(x) == 1L && isTRUE(nzchar(x)) && nchar(x) > 1L) {nchar(x)}
             else if (length(x) > 1L) {length(x)}
         } else if (is.numeric(x)) as.integer(x)
         xa = 2:(N-1)
          a = lapply(xa, \(i) comboGeneral(N, i))
         xx = sapply(a, nrow)
         on.exit(if (wplot) eval(plotish))
         if (!valid.) {
          b = structure(c(as.double(sum(xx))
                        , min(xx)
                        , max(xx))
                        , names = c('total', 'min', 'max'))
             format(b, scientific = TRUE, justify = 'none', digits = 3)
         } else {
           if (is.numeric(x)) stop('numeric quantities are not validated!', call. = FALSE)
            xv = if (length(x) == 1L && nchar(x) > 1L) strsplit(x, '')[[1L]] else x
             v = x; mn = NULL
          for (n in seq(along=a)) {
                      mn = a[[n]][c(1L, eval(cond)),]
                       v = c(v, apply(eval(cmm), 1L, paste0, collapse = ''))
          }
      unique(v[nchar(v) > 1L]) |> sort()
    }
}
