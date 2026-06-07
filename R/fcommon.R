#' @title Identify Common Substrings In A Pair Of Strings
#'
#' @description Checks and identifies substrings that are common to a pair of strings.
#'
#' @param x,y character, length 1 each: a string, such as a protein chain. \code{y} can be missing. White space is removed
#'
#' @details This utility identifies common substrings in the \code{x, y} pair of strings by isolating \emph{sequences} of
#' identical characters in both strings which then, are packed into substrings and validated. All one-character substrings
#' are removed. When \code{y} is missing, \code{x} is cleaved at each letter producing all substrings longer than 1 character.
#' Example 1.3 shows that \emph{all} existing common substrings - of min. 2 characters each - are identified.
#'
#' @returns A sorted character vector of common substrings of min. 2 characters each. When \code{y} is missing from call, a sorted
#' character vector of valid substrings in \code{x} of min. 2 characters each.
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
#' # 1.3 Complete identification of common substrings
#'  A = sort(intersect(aa, bb))                                # common substrings
#'  identical(a, A)                                            # TRUE
#'
#' # 2. Different methods for valid substrings
#'
#' x = 'tyrvvsvltvlhqdwlngkeykck'
#'
#' # 2.1. Combinations matrix (limited by character length)
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
                x = if (grepl(' ', x)) gsub(' ', '', x) else x
                            rez = do.call(frec, list(x))
                            rez = sapply(rez, paste0, collapse = '', USE.NAMES = FALSE)
                            rez = rez[which(sapply(rez, grepl, x, useBytes = TRUE, simplify = TRUE))]
                } else {
                    x = as.character(x); y = as.character(y)
              if (!nzchar(x) || !nzchar(y)) stop('\nno string should be empty!\n', call. = FALSE)
                   fo = lapply(list(x, y), \(i) `<-`(i, if(any(grep(' ', i))) gsub(' ', '', i) else i))
                  m.l = which.min(c(nchar(fo[[1L]]), nchar(fo[[2L]])))
                   xv = strsplit(fo[[m.l]], split = '')[[1L]]
                   yv = strsplit(fo[[-m.l]], split = '')[[1L]]
                    N = max(length(xv), length(yv))
                   xv = append(xv, rep(NA_character_, length(yv) - length(xv)))
                    z = zz = yv
                    m = cbind(xv, z, zz)
                   k0 = list(m[seqv(eval(ii)), 1L]); kk0 = list(m[seqv(eval(jj)), 1L])
                   kl = function() {
                                  k = list(); kk = list()
                                  eval(shifty)
                                 kl = unique(c(k0, kk0, k, kk))
                                kl[lengths(kl) > 1L]
                              }
                            outl = kl()
                              zu = if (length(outl)) {vapply(outl, paste0, character(1L), collapse = '', USE.NAMES = FALSE)}
                                    else stop('only max. 1-character substrings found!', call. = FALSE)
                              xv = yv = fo = m = k0 = k = kk0 = kk = outl <- NULL
                              zv = sapply(zu, frec, USE.NAMES = FALSE)
                              zv = unique(unlist(zv))
                             rez = intersect(
                                    zv[vapply(zv, grepl, logical(1L), x, useBytes = TRUE)]
                                  , zv[vapply(zv, grepl, logical(1L), y, useBytes = TRUE)]
                              )}
            sort.int(rez)
      }

