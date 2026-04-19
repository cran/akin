#'@title Find Substring Combinations
#'
#'@description
#'Internal function for "fcommon" - the fast version of function "common".
#'
#'@param u character, length 1. A string
#'@param from,to integer length 1 each. Range of string character lengths to be identified. Set internally
#'@param rows integer length 1. Default 100. The number of rows (combinations) in each combinations matrix.
#'   Set internally
#'@param ... not used
#'
#'@keywords internal
#'@noRd
#'

core = function(u, from, to, rows = 50, ...) {
        b = if (isTRUE(nzchar(u)) && isTRUE(nchar(u) > 1L) && isTRUE(length(u) == 1L)) strsplit(u, '')[[1L]]
        w = from:to
  listenv = match.fun(listenv, descend = FALSE)
      cbp = listenv()
        m = character()
  for (i in seq(along=w)) {
      A = comboIter(v = b
                    , m = w[i]
                    , FUN = function(x, chain = u, ...) {
                      y = paste0(x, collapse = '')
                      if (eval(cond)) y
                    }
                    , FUN.VALUE = list(1L)
                    , ...
      )
      N = ceiling(as.double(comboCount(b, w[i]))/rows)
      for (j in 1:N) {
        P = A$nextNIter(rows)
        m <- unique(c(P, m))
        j = j + 1
      }
      A@startOver(); P <- NULL
    cbp[[i]] = m; m <- NULL
  }
  unlist(cbp)
}
