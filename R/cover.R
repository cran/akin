#'@title Calculate The Number Of Combinations Of String Covering
#'
#'@description Calculates the cardinality of substring family (\emph{the covering}) and returns valid elements.
#'
#'@param x integer, length = 1 or character vector, length > 1, or character string. White space is removed
#'@param valid. logical, default, FALSE. When TRUE, valid substrings are returned. Requires x as character or string
#'@param wplot logical, FALSE. When TRUE, displays a plot of combinations versus number of characters in string covering
#'
#'@details This function finds the total, the minimum and the maximum number of combinations of individual string
#' covering or returns a subset of valid substrings (i.e. elements of the string) by retaining even transpositions
#' with \emph{sequential} values inside the combinations matrix. Very long strings may result in memory allocation
#' error when \code{valid. = TRUE}. Use [fcommon] instead.
#'
#' NOTE: Herein, the \emph{covering} represents the substring family - of min. 2 characters each - that preserves the
#' order of characters inside the string. Hence, the use of combinations instead of partial permutations. Valid
#' substrings form a (lower cardinality) subset having elements in sequence (see Example 3).
#'
#'@returns When \code{valid. = FALSE} a prettified named vector showing the total, minimum and maximum number of
#' combinations for respective string covering. When \code{valid. = TRUE}, a character vector of valid substrings
#' having min. 2 characters each. In both cases, a plot is returned when \code{wplot = TRUE}.
#'
#'@seealso [fcommon], \link[RcppAlgos]{comboCount}, \link[RcppAlgos]{comboGeneral}
#'
#'@keywords Proteomics
#'
#'@export
#'
#'@examples
#'
#' if (interactive()) {
#'
#' # 1.1 A string
#'  x = 'tyrvvsvltvlhqdwlngkeykck'
#'
#' # 1.2 A vector
#'  y = strsplit(x, '')[[1]]
#'
#' # 1.3 An integer
#'  n = length(y)
#'
#' ll = list(str = x, char = y, int = n)
#' print(t(
#'        sapply(ll, cover)
#'                         ))
#'
#' # 2. Valid substrings with plot
#'
#' d = cover(x, TRUE, TRUE)
#' print(head(d, 30))                         # first 30 substrings
#'
#' # 3. Valid set versus covering
#'
#' # The plot shows 3 combinations of 2-character groups while e
#' # contains 2 sequences of 2 characters each, substring "ac"
#' # not being a sequence.
#'
#' x = letters[1:3]
#' e = cover(x, TRUE, TRUE)
#' print(e)                                   # valid combinations
#'
#' }
#'
#'

cover = function(x, valid. = FALSE, wplot = FALSE) {
           if (any(!nzchar(x))) stop('\nstring should not be empty!\n', call. = FALSE)
         x = if (any(grepl(' ', x)) && length(x) == 1L) gsub(' ', '', x) else x[!x %in% ' ']
         N = if (is.character(x)) {
             if (length(x) == 1L && nchar(x) > 1L) nchar(x) else if (length(x) > 1L) length(x)
         } else if (is.numeric(x)) as.integer(x)
         if (N <= 2L) stop('string is too short!', call. = FALSE) else xa = 2:(N-1)
         if(wplot && N > 2L) on.exit(eval(plotish), add = TRUE)
         xx = sapply(xa, \(i) as.double(RcppAlgos::comboCount(N, i)))
         if (!valid.) {
          b = structure(c(sum(xx), min(xx), max(xx)), names = c('total', 'min', 'max'))
         return(format(b, scientific = TRUE, justify = 'none', digits = 3L))
         } else {
         if (is.numeric(x)) stop("no validation for numeric!", call. = FALSE)
         xv = if (length(x) == 1L && nchar(x) > 2L) {strsplit(x, '')[[1L]]
              } else if (length(x) > 2L) x
          a = lapply(xa, \(i) RcppAlgos::comboGeneral(N, i))
          v = if(length(x) > 2L) paste0(x, collapse = '') else x
         mn = listenv()
         for (n in seq(along=xa)) {
                          mn = a[[n]][c(1L, eval(cond)),]
                           v = c(v, apply(eval(cmm), 1L, paste0, collapse = ''))
           }
         return(sort.int(unique(v)))
    }
}
