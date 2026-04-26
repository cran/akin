#'@title Find Substring Combinations
#'
#'@description
#'Internal function for \"fcommon\" - the fast version of function \"common\".
#'
#'@param u character, length 1. A string
#'@param rows integer length 1. Default 100. The number of rows (combinations) in each
#'  combinations matrix
#'@param ... not used
#'
#'@keywords internal
#'@noRd
#'

core = function(u, rows = 100, ...) {
        b = strsplit(as.character(u), '')[[1L]]
       nc = nchar(u)
        w = 2L:(nc - 1L)
  listenv = match.fun(listenv, descend = FALSE)
      cbp = listenv()
        m = character()
  for (i in seq(along=w)) {
      A = comboIter(v = b
                    , m = w[i]
                    , FUN = function(x, chain = u, ...) {
                            y = paste0(x, collapse = '', ...)
                            if (eval(cond)) y else NULL
                    }
                    , FUN.VALUE = list(1L)
                    , ...
      )
        N = ceiling(as.double(comboCount(b, w[i]))/rows)
      for (j in 1:N) {
        P = A$nextNIter(rows)
        m <- unique(c(m, P[lengths(P) > 0L])); P <- NULL
        j = j + 1
      }
      A@startOver()
       cbp[[i]] = m; m <- NULL
  }
  unlist(cbp)
}
