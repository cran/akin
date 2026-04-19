#' @title Fast Identify Common Substrings In A Pair Of Strings
#'
#' @description Fast version of [common]. Experimental.
#'
#' @param x,y character, length = 1 each: a string, such as a protein chain. \code{y} can be missing.
#' @param ... reserved for internal argument \code{rows} which has a default value of 50
#'
#' @details This utility is experimental and will replace [common] in near future. It identifies all-length
#'  substrings common to the pair of strings.
#'
#' @returns A closure (see Example) which in turn returns a character vector of common substrings of all lengths. When \code{y}
#' is missing from call, a character vector of all valid substrings in \code{x}.
#'
#' @keywords Proteomics
#'
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'
#' # 1. A pair of strings
#'
#'  x = 'alpdxoipoyloiekladxoipoylyl'
#'  y = 'kdxoipoylyydxoipoylopldxoipoylac'
#'
#' # 1.1 Equivalent to "common(..., from = 2, to = 9)"
#'  g = fcommon(x, y)
#'  system.time( a <- g() )
#'  print(a)                                              # common substrings set
#'
#'  }
#'

fcommon = function(x, y, ...) {
                    if (missing(y)) y <- x
                    x = as.character(x); y = as.character(y)
                   fo = list(x, y)
                  m.l = which.min(c(nchar(fo[[1L]]), nchar(fo[[2L]])))
                   xv = strsplit(fo[[m.l]], split = '')[[1L]]
                   yv = if (missing(y)) xv else strsplit(fo[[-m.l]], split = '')[[1L]]
                  fo <- NULL
                   xv = append(xv, rep(NA_character_, abs(length(yv) - length(xv))))
                    z = zz = yv
                    m = cbind(xv, z, zz)
                      function() {
                               core = match.fun(core, descend = FALSE)
                                  k = list(); kk = list()
                                  N = max(length(xv), length(yv))
                           for (i in 1:N) {
                                        z <<- shift(z, -1L)
                                       zz <<- shift(zz, 1L)
                                        m <<- cbind(xv, z, zz)
                                       ii = which(m[,1L] == m[,2L], arr.ind = TRUE)
                                       jj = which(m[,1L] == m[,3L], arr.ind = TRUE)
                                   k[[i]] = m[eval(findSeqUp), 1L]; kk[[i]] = m[eval(findSeqDn), 1L]
                                }
                                kl = c(k, kk) |> unique()
                                kl = kl[lengths(kl) > 0L]
                               rez = sapply(kl, paste0, collapse = '', simplify = TRUE); kl <- NULL
                               rez = rez[which(sapply(rez, grep, x, simplify = TRUE) > 0L)]
                               rez = Reduce(function(x,y) union(x,y)
                                          , c(rez
                                          , sapply(rez
                                          , \(i) if(nchar(i) > 2L) core(i, from = 2L, to = nchar(i) - 1L, ...) else if (nchar(i) == 2L) i, simplify = TRUE))
                                     )
               rez[order(nchar(rez))]
            }
  }


