#' @title Fast Identify Common Substrings In A Pair Of Strings
#'
#' @description Checks and identifies substrings that are common to a pair of strings.
#'
#' @param x,y character, length 1 each: a string, such as a protein chain. \code{y} can be missing
#'
#' @details This utility identifies common substrings in the \code{x}, \code{y} pair of strings by isolating \emph{sequences} of
#' identical characters in both strings which then, are packed into substrings and validated. All one-character substrings are removed.
#' When \code{y} is missing, \code{x} is cleaved at each letter producing all substrings longer than 2 characters.
#'
#' @returns A sorted character vector of common substrings of length >= 2 characters each. When \code{y} is missing from call, a sorted
#' character vector of valid substrings in \code{x} of length >= 2 characters each.
#'
#' @keywords Proteomics
#'
#' @seealso [cover]
#'
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'
#'  # 1. Check for common substrings in the pair below
#'
#'  x = 'dvvmtqsplslpvtpgepasiscrssqslaktyrvvsvltvlhqdwlngkeykckvv'
#'  y = 'mtqspltyrvvsvltvlhqdwlngkeykcksnkalpapiektisk'
#'
#' # 1.1 Common substrings
#'  system.time(a <- fcommon(x, y))
#'  print(head(a, 30))
#'
#' # 1.2 Cleaving (slow on very long strings!)
#'  system.time(aa <- fcommon(x))
#'  system.time(bb <- fcommon(y))
#'
#' # 1.3 Identical results
#'  A = sort(intersect(aa, bb))                                # common substrings
#'  identical(a, A)                                            # TRUE
#'
#' # 2. Different methods for valid substrings
#'
#' x = 'tyrvvsvltvlhqdwlngkeykck'
#'
#' # 2.1. Combinations matrix (slower!)
#' system.time(am <- cover(x, valid. = TRUE))                  # valid substrings
#'
#' # 2.2 String cleaving
#' system.time(ac <- fcommon(x))                               # valid substrings
#'
#' identical(am, ac)                                           # TRUE
#'
#'}
#'

fcommon = function(x, y) {
                 frec = match.fun(frec, descend = FALSE)
                 seqv = match.fun(seqv, descend = FALSE)
              on.exit(rez <- NULL, add = TRUE)
              if (missing(y)) {
               if (!nzchar(x)) stop('\nstring should not be empty!\n', call. = FALSE)
                            rez = do.call(frec, list(x))
                            rez = sapply(rez, paste0, collapse = '', USE.NAMES = FALSE); outl <- NULL
                            rez = rez[which(sapply(rez, grepl, x, useBytes = TRUE, simplify = TRUE))]
                } else {
                    x = as.character(x); y = as.character(y)
              if (!nzchar(x) || !nzchar(y)) stop('\nno string should be empty!\n', call. = FALSE)
                   fo = list(x, y)
                  m.l = which.min(c(nchar(fo[[1L]]), nchar(fo[[2L]])))
                   xv = strsplit(fo[[m.l]], split = '')[[1L]]
                   yv = strsplit(fo[[-m.l]], split = '')[[1L]]; fo <- NULL
                   xv = append(xv, rep(NA_character_, abs(length(yv) - length(xv))))
                    z = zz = yv
                    m = cbind(xv, z, zz)
                   k0 = m[seqv(eval(ii)), 1L]; kk0 = m[seqv(eval(jj)), 1L]
                   kl = function() {
                                  k = list(); kk = list()
                                  N = max(length(xv), length(yv))
                           for (i in 1:N) {
                                        z <<- shift(z, -1L)
                                       zz <<- shift(zz, 1L)
                                        m <<- cbind(xv, z, zz)
                                     k[[i]] = m[seqv(eval(ii)), 1L]
                                    kk[[i]] = m[seqv(eval(jj)), 1L]
                                    }
                                kl = c(k0, kk0, k, kk) |> unique()
                                xv = yv = m = k0 = k = kk0 = kk <- NULL
                                kl[lengths(kl) > 1L]
                                }
                            outl = kl()
                              zu = sapply(outl, paste0, collapse = '', USE.NAMES = FALSE); outl <- NULL
                              zv = sapply(zu, frec)
                              zv = unlist(zv) |> unique()
                             rez = intersect(
                                      zv[which(sapply(zv, grepl, x, useBytes = TRUE, simplify = TRUE))]
                                    , zv[which(sapply(zv, grepl, y, useBytes = TRUE, simplify = TRUE))]
                            )
                      }
            sort(rez)
      }

