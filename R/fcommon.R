#' @title Fast Identify Common Substrings In A Pair Of Strings
#'
#' @description Checks and identifies substrings that are common to a pair of strings.
#'
#' @param x,y character, length 1 each: a string, such as a protein chain. \code{y} can be missing
#' @param strategy character, length 1 or symbol. Strategy for parallel processing. Choices are "multisession", "multicore" and
#' "cluster". Default NULL, which corresponds to sequential processing
#' @param workers integer, length 1. Number of workers in \link[future]{plan}. Default, NULL which, when \code{strategy != NULL}
#'   selects all available logical CPUs (not always recommended). Requires \code{strategy != NULL}
#' @param maxSize integer, length 1. Size of object sent to each worker during parallel processing. Default, NULL
#'   corresponding to 500.0 MiB according to \link[future]{future.globals.maxSize}. Requires \code{strategy != NULL}
#' @param ... reserved for internal arguments \code{rows}, default value 100, representing maximum number of combinations matrix rows
#'  sent to the iterator during sequential processing and \code{brows}, default value 1e6 representing maximum number of combinations
#'  matrix rows sent to each logical CPU during parallel processing. These arguments should always be named
#'
#' @details This utility identifies all common substrings in the \code{x}, \code{y} pair of strings by isolating \emph{sequences} of
#' identical characters in both strings, which then are packed into substrings and validated. This set of common, shorter than original,
#' strings lowers the combinatorial overhead which, next, searches for elements of the subset family (i.e. the \emph{covering}) of each
#' common substring. Further filtering validates sub-substrings that are elements of each common substring (\emph{truncations} or otherwise).
#' Finally, only a fraction of all combinations generate the set of common substrings. All one-character substrings are removed.
#'
#' Common substrings up to 20 characters length are processed sequentially and longer common substrings are returned, when found,
#' with a message. Longer substrings can be processed in parallel by setting values for \code{strategy} and \code{workers} which set
#' a \emph{local} \link[future]{plan}, triggering the parallel processing mode. Function [cover] helps adjusting the \link[future]{plan}.
#'
#' \code{maxSize}. By default, the size of objects sent to each logical CPU during parallel processing is set at 500.0 MiB. Parallel
#' processing of strings of more than 30 characters length may challenge this limit if the number of workers set in \link[future]{plan}
#' is small in relation to the length of these substrings. To decrease worker's load, a recommended approach is to increase the number
#' of \code{workers} or, to lower the number of \code{brows} in the \code{...} list (which may result in a longer processing time).
#' Otherwise, check \link[future]{future.globals.maxSize} option and set a value for \code{maxSize} as suggested there.
#'
#' @returns A sorted character vector of common substrings longer than 2 characters each. When \code{y} is missing from call, a sorted
#' character vector of valid substrings in \code{x} longer than 2 characters each.
#'
#' @seealso [cover], \link[future]{plan}, \link[future]{future.globals.maxSize}, \link[RcppAlgos]{comboIter}, \link[RcppAlgos]{comboGeneral},
#'          \link[parallel]{detectCores}
#'
#' @keywords Proteomics
#'
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'
#'  # Check for common substrings in the pair below
#'
#'  x = 'dvvmtqsplslpvtpgepasiscrssqslaktyrvvsvltvlhqdwlngkeykckvv'
#'  y = 'mtqspltyrvvsvltvlhqdwlngkeykcksnkalpapiektisk'
#'
#' # 1. Sequential Run
#'
#' # 1.1 Brief output
#'  system.time(a <- fcommon(x, y))
#'  print(a)                                              # output and message
#'
#' # 1.2 Long substring discovered above
#' z = 'tyrvvsvltvlhqdwlngkeykck'
#'
#' # 1.3 Check the workload in parallel processing
#' cover(z, TRUE)                                         # covering combinations
#'                                                        # and plot
#' # The "max" value suggests that 3 workers suffice
#'
#' # 2. Parallel run
#'\dontrun{
#'  system.time(b <- fcommon(x, y, multisession, 3))      # the plan is set
#'  print(b)                                              # extended output
#' }
#' }
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
                   k0 = m[eval(findSeqUp), 1L]; kk0 = m[eval(findSeqDn), 1L]
                   kl = function() {
                                  k = list(); kk = list()
                                  N = max(length(xv), length(yv))
                           for (i in 1:N) {
                                        z <<- shift(z, -1L)
                                       zz <<- shift(zz, 1L)
                                        m <<- cbind(xv, z, zz)
                                   k[[i]] = m[eval(findSeqUp), 1L]; kk[[i]] = m[eval(findSeqDn), 1L]
                                }
                                kl = c(k0, kk0, k, kk) |> unique()
                                xv = yv = m = k0 = k = kk0 = kk <- NULL
                                kl[lengths(kl) > 1L]
                        }
                              outl = kl()
                               rez = sapply(outl, paste0, collapse = '', USE.NAMES = FALSE); outl <- NULL
                               rez = rez[which(sapply(rez, grepl, x, useBytes = TRUE, simplify = TRUE))]
                               rez = unique(rez)
                               rez = c(rez
                                     , lapply(rez
                                            , \(i) {
                                                ni = nchar(i)
                                            if (ni == 2L) i else if (2L < ni && ni <= early.) {
                                              core(i, ...)} else if (!is.null(strategy) && ni > early.) {
                                              corePar = match.fun(corePar, descend = FALSE)
                                              corePar(i, tpe = strategy, wo = workers, optMax = maxSize)}
                                            })) |> unlist() |> unique()
            if (isTRUE(max(nchar(rez)) > early.) && is.null(strategy)) message('sequential plan: brief output!')
            sort(rez)
      }

