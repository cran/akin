#' @title Identify Common Substrings In A Pair Of Strings
#'
#' @description Checks and identifies substrings that are common in a pair of strings.
#'
#' @param X,Y character, length = 1 each: a string, such as a protein chain. When \code{Y = NULL},
#'    all arguments coming after \code{X} should be named
#' @param from,to integer length 1 each. Range of string character lengths to be identified. When \code{to}
#'    is missing from call, only substrings of length equal to \code{from} value are sought
#' @param lower,upper integer, length 1 each, default NULL: spans all substring combinations. Otherwise, spans
#'    only a subset of combinations starting with the \code{lower}-th and ending with the \code{upper}-th
#'    combinations. See [RcppAlgos::comboGeneral]
#' @param outlist logical. Default, FALSE, return a character vector of common substrings. TRUE, return a list
#'    of valid substrings found in each chain
#' @param rows integer length 1. Default 1000. The number of rows (combinations) in each combinations matrix
#' @param wait integer length 1. Default 100. Duration of the background process polling. Unit: milliseconds.
#'    During this time, the main process is blocked.
#' @param ... not used
#'
#' @details Using brute combinatorial approach, this utility partitions each \code{chain} in the pair into all possible
#' combinations of substrings with lengths inside the \code{from}-\code{to} window. Each combination is filtered for
#' uniqueness and appartenance to its original chain (i.e. for validity), yielding a fraction of common substrings out of all
#' substrings. Inside the valid set, each substring represents the \emph{truncation} in a sequence of longer substrings.
#'
#' The overhead causes severe search time increase with the total number of combinations which, depend on \code{chain} and
#' substring lengths, and search window width. Slight performance improvement comes from variations in \code{rows} value
#' i.e. by balancing the number of combinations matrix \code{rows} with the total number of combinations matrices. The default
#' value is an acceptable starting point. Chains of several hundreds of characters, such as long protein chains, may take hours
#' on some machines. In such cases, \code{rows} value can be risen toward the order of 1e6. When \code{Y != NULL}, the partitioning
#' of chains runs asynchronously.
#'
#' The \code{lower} and \code{upper} limits. Setting any or both of these arguments reduces the search time and only establishes
#' whether \emph{any} common substrings exist at all, without guaranteeing complete results. In case of very long chains, setting
#' these limits too wide apart may lead to memory allocation error. Checking the number of combinations before run is
#' recommended, see [RcppAlgos::comboCount], [utils::combn] etc.
#'
#' NOTE: This utility uses background processing. Check "Security Considerations" in \strong{callr} package documentation.
#'
#' @returns A character vector of common substrings or a list of valid substrings found in both chains. When \code{Y = NULL}, a list
#' of all valid substrings in \code{X} within the \code{from}-\code{to} window.
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
#'  print(a)                                              # common substrings set
#'
#'  # 1.2 Check the commonality of substrings in "a"
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
#' # 2. Combinatorial Subset
#'
#'  c = common(X, Y, from = 4, to = 9, lower = 5, upper = 100, outlist = TRUE)
#'  print(c)                                              # named list
#'
#' # 3. Single Chain
#'
#'  d = common(X, from = 3, to = 5)
#'  print(d)                                              # valid substrings set
#'
#' }
#'

common = function(X, Y = NULL, from, to, lower = NULL, upper = NULL, outlist = FALSE, rows = 1000, wait = 100, ...) {
  X = as.character(X); if (!is.null(Y)) Y = as.character(Y)
  on.exit(ca <- NULL, add = TRUE); on.exit(cb <- NULL, add = TRUE)
  b = if (isTRUE(nzchar(X)) && isTRUE(nchar(X) > 1L) && isTRUE(length(X) == 1L)) {strsplit(X, '')[[1L]]
      } else {stop('X and Y must be strings!', call. = FALSE)}
  if (missing(to)) to <- from
  w = from:to
  if (!is.null(Y)) {
  prc = r_bg(function(w, listenv, comboIter, comboGeneral, rows, comboCount, Y, cond, lower, upper, ...) {
         bp = if (!is.null(Y) && isTRUE(nzchar(Y)) && isTRUE(nchar(Y) > 1L) && isTRUE(length(Y) == 1L)) {strsplit(Y, '')[[1L]]
              } else {stop('X and Y must be strings!', call. = FALSE)}
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
                                                 , FUN.VALUE = list(1L)
                                                 , ...
                                                 )
                                    N = ceiling(as.double(comboCount(bp, w[i]))/rows)
                                    for (j in 1:N) {
                                               P = A$nextNIter(rows)
                                               m <- unique(c(P, m))
                                               j = j + 1
                                              }
                                   A@startOver(); P <- NULL
                    } else {
                      A = comboGeneral(v = bp
                                       , m = w[i]
                                       , FUN = function(x, chain = Y, ...) {
                                         y = paste0(x, collapse = '')
                                         if (eval(cond)) y
                                       }
                                       , FUN.VALUE = list(1L)
                                       , chain = Y
                                       , lower = lower
                                       , upper = upper
                                       , ...
                                       )
                      m <- unique(A); A <- NULL
                    }
          cbp[[i]] = m; m <- NULL
            }
     unlist(cbp)
  }, args = list(w, listenv, comboIter, comboGeneral, rows, comboCount, Y, cond, lower, upper, ...), user_profile = FALSE, supervise = TRUE)}
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
             } else {
               A = comboGeneral(v = b
                                , m = w[i]
                                , FUN = function(x, chain = X, ...) {
                                  y = paste0(x, collapse = '')
                                  if (eval(cond)) y
                                }
                                , FUN.VALUE = list(1L)
                                , chain = X
                                , lower = lower
                                , upper = upper
                                , ...
               )
               m <- unique(A); A <- NULL
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
