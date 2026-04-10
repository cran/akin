#' @title Identify Common Substrings In A Pair Of Strings
#'
#' @description Checks and identifies substrings that are common in a pair of strings.
#'
#' @param X,Y character, length = 1 each: a string, such as a protein chain. \code{Y} can be \code{NULL}.
#' @param from,to integer length 1 each. Range of string number of characters to identify. When \code{to} is missing from call,
#'    only substrings of number of characters equal to \code{from} are sought
#' @param lower,upper integer, length 1 each, default NULL: all combinations. Otherwise, only a subset of all combinations starting
#'    with the \code{lower}-th and ending with the \code{upper}-th combinations. See [RcppAlgos::comboGeneral]
#' @param outlist logical. Default, FALSE, return a character vector of common substrings. Otherwise, return valid
#'    substrings found in each chain
#' @param rows integer length 1. Default 1000. The number of rows in each iteration (i.e. combinations) matrix
#' @param wait integer length 1. Default 100. Duration of background process polling. Unit: milliseconds.
#'    During this time, the main process is being blocked.
#' @param ... not used
#'
#' @details Using brute combinatorial approach, this utility splits each of the two \code{chain}s into all possible
#' combinations of substrings with lengths inside the \code{from}-\code{to} window. Each combination is filtered for
#' uniqueness and appartenance to its original chain (i.e. for validity). Search time increases severely with the number
#' of combinations: \code{chain} and substring lengths, and search window width. Slight performance improvement comes
#' from variations of \code{rows} values as the larger the number of \code{rows}, the smaller the number of iterations.
#' The default value is a satisfactory starting point. Chains of hundreds of characters, such as long protein chains,
#' may take hours on some machines. In such cases, \code{rows} can be set to the order of 1e6. During search, the
#' partitioning of chains runs asynchronously when \code{Y != NULL}.
#'
#' \code{lower} and \code{upper} limits. Setting any or both of these limits reduces the search time without guaranteeing
#' the completeness of common substrings list. It helps establishing the existence of \emph{any} common substrings.
#'
#' NOTE: This utility uses background processing. Check "Security Considerations" in \strong{callr} package documentation.
#'
#' @returns A character vector or list of substrings found in both chains. Otherwise, "character(0)". When \code{Y = NULL},
#' a list of all valid substrings within the \code{from}-\code{to} window.
#'
#' @seealso \link[RcppAlgos]{comboIter}, \link[RcppAlgos]{comboGeneral}, \link[RcppAlgos]{comboCount}
#'
#' @keywords Proteomics
#'
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'
#' # 1. A set of chains
#'
#'  X = 'alpdxoipoyloiekladxoipoylyl'
#'  Y = 'kdxoipoylyydxoipoylopldxoipoylac'
#'
#' # 1.1 Identify all 4-character common substrings
#'  system.time(a <- common(X, Y, 4))
#'  print(a)
#'
#'  # 1.2 Check if substrings in "a" are common
#'  chain = list(chain1 = 'alpdxoipoyloiekladxoipoylyl',
#'               chain2 = "kdxoipoylyydxoipoylopldxoipoylac")
#'
#'  b = sapply(a, findLoc, chain, TRUE, TRUE, TRUE, simplify = FALSE)
#'  print(b)                                              # a named list
#'
#'
#'  any(lengths(b) == 0L)                                 # FALSE
#'  identical(length(a), length(b))                       # TRUE
#'
#' }
#'



common = function(X, Y = NULL, from, to, lower = NULL, upper = NULL, outlist = FALSE, rows = 1000, wait = 100, ...) {
  X = as.character(X); if (!is.null(Y)) Y = as.character(Y)
  on.exit(ca <- NULL, add = TRUE); on.exit(cb <- NULL, add = TRUE)
  delayedAssign('N', ceiling(as.double(comboCount(b, w[i]))/rows))
  b = if (isTRUE(nzchar(X)) && isTRUE(nchar(X) > 1L) && isTRUE(length(X) == 1L)) strsplit(X, '')[[1L]] else stop('X and Y must be strings', call. = FALSE)
  if (missing(to)) to <- from
  w = from:to
  if (!is.null(Y)) {
  prc = r_bg(function(w, listenv, comboIter, comboGeneral, rows, comboCount, sift, Y, cond, lower, upper, ...) {
         bp = if (isTRUE(nzchar(Y)) && isTRUE(nchar(Y) > 1L) && isTRUE(length(Y) == 1L)) strsplit(Y, '')[[1L]]
              else stop('X and Y must be strings', call. = FALSE)
         delayedAssign('N', ceiling(as.double(comboCount(bp, w[i]))/rows))
                   listenv = match.fun(listenv, descend = FALSE)
                       cbp = listenv()
                         m = character()
                  for (i in seq(along=w)) {
                    if (is.null(lower) && is.null(upper)) {
                                     A = comboIter(v = bp
                                                 , m = w[i]
                                                 , FUN = function(x, chain = Y, ...) {
                                                               y = paste0(x, collapse = '')
                                                               if (eval(cond)) y
                                                 }
                                                 , ...
                                                 )
                                    for (j in 1:N) {
                                               P = A$nextNIter(rows)
                                               m <- unique(c(P, m))
                                               j = j + 1
                                              }
                                   A@startOver(); P <- NULL
                    } else {
                      A = comboGeneral(v = bp
                                       , m = w[i]
                                       , FUN = sift
                                       , FUN.VALUE = list(1L)
                                       , chain = Y
                                       , lower = lower
                                       , upper = upper
                                       , ...
                      )
                      m <- unique(A)
                    }
          cbp[[i]] = m; m <- NULL
    }
     unlist(cbp)
  }, args = list(w, listenv, comboIter, comboGeneral, rows, comboCount, sift, Y, cond, lower, upper, ...), user_profile = FALSE, supervise = TRUE)}
  cb = listenv()
   m = character()
           for (i in seq(along=w)) {
             if (is.null(lower) && is.null(upper)) {
                              A = comboIter(v = b
                                          , m = w[i]
                                          , FUN = function(x, chain = X, ...) {
                                                        y = paste0(x, collapse = '')
                                                        if (eval(cond)) y
                                                   }
                                          , ...
                                          )
                             for (j in 1:N) {
                                       P = A$nextNIter(rows)
                                       m <- unique(c(P, m))
                                       j = j + 1
                                      }
                            A@startOver(); P <- NULL
             } else {
               A = comboGeneral(v = b
                                , m = w[i]
                                , FUN = sift
                                , FUN.VALUE = list(1L)
                                , chain = X
                                , lower = lower
                                , upper = upper
                                , ...
               )
               m <- unique(A)
             }
                  cb[[i]] = m; m <- NULL
           }
  cb = unlist(cb)
  if(!is.null(Y)) {ca = if('ready' %in% prc$poll_io(wait)['process']) {prc$get_result()} else {
                                                                            prc$wait()
                                                                            prc$get_result()
                                                                            }}
  if (!outlist && !is.null(Y)) intersect(ca, cb) else if (!is.null(Y)) list('X' = cb, 'Y' = ca) else cb
}
