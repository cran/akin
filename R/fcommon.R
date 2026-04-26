#' @title Fast Identify Common Substrings In A Pair Of Strings
#'
#' @description Checks and identifies substrings that are common in a pair of strings.
#'
#' @param x,y character, length 1 each: a string, such as a protein chain. \code{y} can be missing
#' @param strategy character, length 1. Strategy for parallel processing within \link[future]{plan}. Choices "multisession",
#'   "multicore" and "cluster". Default NULL, which corresponds to sequential plan
#' @param workers integer, length 1. Number of workers in \link[future]{plan}. Default, NULL which, when \code{strategy != NULL}
#' selects all available logical CPUs (not always recommended)
#' @param maxSize integer, length 1. Size of object sent to each worker during parallel processing. Default, NULL
#'   corresponding to 500.0 MiB according to \link[future]{future.globals.maxSize}
#' @param ... reserved for internal arguments \code{rows}, default value 100, representing the number of
#'  combinations matrix rows sent to iterator during sequential processing and \code{brows}, default value 1e6
#'  representing maximum number of combinations matrix rows sent to each logical CPU during parallel processing. These
#'  arguments should always be named
#'
#' @details This utility identifies all common substrings by looking for sequences of identical elements in the \code{x}, \code{y}
#' pair of strings. This, smaller set lowers the overhead for combinatorial operations which, next search for substring elements
#' forming the \emph{covering} of each common substring. Filtering operations retain those covering sub-substrings that are also
#' elements of the common substring (i.e. valid sub-substrings, \emph{truncations} or otherwise). All one-character substrings are removed.
#'
#' Common substrings of up to 20 characters length are processed sequentially. Longer substrings can be processed in parallel
#' when values for \code{strategy} and \code{workers} are set indicating that a \emph{local} parallel \link[future]{plan} is in place.
#' In the absence of such plan, existing long substrings are returned as such along with a message.
#'
#' \code{maxSize}. By default, the size of objects sent to each logical CPU during parallel processing is set at 500.0 MiB. This
#' value corresponds to \code{maxSize = NULL}. Parallel processing of strings of or more than 30 characters length may
#' challenge this limit if the number of workers set in \link[future]{plan} is small in relation to the length of these substrings.
#' To decrease worker's load, a recommended approach is to increase the number of \code{workers} or lower the number of \code{brows}
#' in the \code{...} list (resulting in a longer run time).
#' Otherwise, check \link[future]{future.globals.maxSize} option and set a value for \code{maxSize} as suggested there.
#'
#' @returns A sorted character vector of common substrings of various lengths. When \code{y} is missing from call, a sorted
#' character vector of all valid substrings in \code{x}.
#'
#' @seealso  \link[RcppAlgos]{comboIter}, \link[RcppAlgos]{comboGeneral}, \link[RcppAlgos]{comboCount}, \link[future]{plan},
#'   \link[future]{future.globals.maxSize}
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
#' # 1.1 Equivalent to "common(x, y, from = 2, to = 9)"
#'  system.time(a = fcommon(x, y))
#'  print(a)                                              # common substrings set
#'
#'  }
#'

fcommon = function(x, y, strategy = NULL, workers = NULL, maxSize = NULL, ...) {
                 core = match.fun(core, descend = FALSE)
                 delayedAssign('early.', 20L)
                    if (missing(y)) y <- x
                    x = as.character(x); y = as.character(y)
                   fo = list(x, y)
                  m.l = which.min(c(nchar(fo[[1L]]), nchar(fo[[2L]])))
                   xv = strsplit(fo[[m.l]], split = '')[[1L]]
                   yv = if (missing(y)) xv else strsplit(fo[[-m.l]], split = '')[[1L]]; fo <- NULL
                   xv = append(xv, rep(NA_character_, abs(length(yv) - length(xv))))
                    z = zz = yv
                    m = cbind(xv, z, zz)
                   kl = function() {
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
                                kl = c(k, kk) |> unique(); xv = yv = m <- NULL
                                kl[lengths(kl) > 1L]
                        }
                              outl = kl()
                               rez = sapply(outl, paste0, collapse = '', USE.NAMES = FALSE); outl <- NULL
                               rez = rez[which(sapply(rez, grep, x, useBytes = TRUE, simplify = TRUE) > 0L)]
                               rez = unique(rez)
                               rez = c(rez
                                     , sapply(rez
                                            , \(i) {
                                                ni = nchar(i)
                                            if (ni == 2L) i else if (2L < ni && ni <= early.) {
                                              core(i, ...)} else if (!is.null(strategy) && ni > early.) {
                                              corePar = match.fun(corePar, descend = FALSE)
                                              corePar(i, tpe = strategy, wo = workers, optMax = maxSize)}
                                            } , simplify = TRUE, USE.NAMES = FALSE)) |> unlist() |> unique()
            if (isTRUE(max(nchar(rez)) > early.) && is.null(strategy)) message('sequential plan detected: brief output!')
               sort(rez)
      }

