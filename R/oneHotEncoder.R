
#' @title One-hot Encoder And Decoder Of Variables
#'
#' @description Encodes logical, categorical, integer and double type variables.
#'
#' @param x a (named) vector or list for encoding. Missing data are removed. For decoding, a dense or sparse
#'   matrix (preferably, the result of encoding) representing a single source data column
#' @param type symbol. Choices: \strong{encode} - one-hot encoding, \strong{decode} - revert to original
#' @param omc character length 1. \strong{O}utput \strong{m}atrix \strong{c}lass. Default, 'dgCMatrix', other
#'   option, 'matrix'
#' @param verbose logical, default TRUE, display messages
#'
#' @details This utility one-hot encodes when \code{type = encode} and verifies the encoded result (or any
#' matrix of encodings obtained with [getEV] extractor) when \code{type = decode}. It detects illicit states.
#'
#' @returns Encoding returns a matrix of \code{length(x)} rows and \code{length(unique(x))} columns or a warning.
#' Decoding returns a (named) vector or a warning. List vectors are returned unlisted. Integer(ish) vectors,
#' converted to integer, character vectors - to factor, double or logical vector types remain unchanged.
#'
#'@keywords conversion
#'
#'@export
#'
#'@examples
#'
#' if (interactive()) {
#'
#' # 1. Encode type "double"
#'
#' x = runif(9)                           # numeric, length 9
#' names(x) = letters[1:9]                # named
#' typeof(x)
#' a = oneHot(x, encode)                  # a sparse matrix of "dgCMatrix" class
#' b = oneHot(a, decode)                  # a type "double" named numeric, length 9
#' isTRUE(all.equal(x, b))                # TRUE
#' typeof(b)
#' print(x); print(b)
#'
#' # 2. Type "logical" with missing values
#'
#' y = c(TRUE, TRUE, NA, FALSE, TRUE, NA) # logical, length 6 with missing values
#' typeof(y)
#' a = oneHot(y, encode, 'matrix')
#' print(a)                               # a dense matrix
#' b = oneHot(a, decode)                  # revert
#' all.equal(y, b)                        # missing values in y removed
#' typeof(b)
#' print(x); print(b)
#'
#' # 3. iris data
#'
#' data(iris)
#' a = lapply(iris, oneHot, encode)       # encode entire data
#' b = as.data.frame(
#'          lapply(a, oneHot, decode)     # revert
#'      )
#' identical(iris, b)                     # TRUE. Now, replace iris data with
#'                                        # mtcars data!
#'
#' # 4. Illicit states in one-hot encoding
#'
#' `3.41` = c(1,0,0,1,1,0,0,1)            # encoded type "double"
#' `0.12` = c(0,1,0,0,0,1,1,0)
#'  a = cbind(`3.41`, `0.12`)             # form encoded matrix
#'  print(a)                              # matrix resembling one-hot encoding
#'  x = oneHot(a, decode)                 # illicit state detected
#'  print(x)                              # list with 2 different data types
#'
#'}
#'

oneHot = function(x, type, omc = 'dgCMatrix', verbose = TRUE) {
                     type = substitute(type)
                     type = match.arg(as.character(type), choices = c('encode', 'decode'))
                      omc = match.arg(omc, choices = c('matrix', 'dgCMatrix'))
               switch(type
                    , encode = {
                     if (!is.null(dim(x))) {
                      stop('data should have one dimension only!', call. = FALSE)}
                     else {
                       delayedAssign('sumsetc', sum(m) == n.x && all(rowSums(m) == 1L))
                      if (anyNA(x) && verbose) cat('\nall missing values were removed!\n')
                        x = if (is.list(x)) unlist(x)[!is.na(x)] else x[!is.na(x)]
                      if(length(x) == 0L && verbose) {cat('\nno remaining values in input!!\n\n')
                    } else if (is.double(x) && verbose) {cat('encoding type \"double\"!...\n')
                    } else if(verbose) {cat('encoding type \"integer(ish)\"...\r')}
                     rn.x = names(x)
                        x = fdroplevels(if (is.factor(x)) x else as.factor(x))
                      n.x = length(x); ll = levels(x); n.l = length(ll)
                        m = eval(M)
                     for (i in 1:n.l) m[, i] = fmatch(x, ll[i], 0L, NULL)
                     if (isTRUE(sumsetc)) return(as(m, omc)) else warning('ambiguous encoding!\n', call. = FALSE)
                }}
                   , decode = {
                    if (is.null(dim(x))) {stop('data must have at least 2 columns', call. = FALSE)
                    } else {
                     delayedAssign('sms', rowSums(x) != 1L)
                     delayedAssign('any0len', any(sapply(out, length) == 0L))
                     delayedAssign('nolenout', length(out) != nrow(x))
                    if (any(sms) && verbose) cat('\nillicit state at row(s) ', which(sms), ' in input!\n')
                    rn.x = dimnames(x)[[1L]]
                     n.x = dimnames(x)[[2L]]
                       g = match.fun(g, descend = FALSE)
                  if (eval(tof) && verbose) {cat('decoding type \"double\"...\n')
                  } else if (verbose) (cat('decoding type \"integer(ish)\"...\r'))
                     out = apply(x, 1L, g, n.x, simplify = TRUE)
                     out = structure(type.convert(out, as.is = FALSE, numerals = 'no.loss'), names = rn.x)
                     if (nolenout || any0len && verbose) cat('ambiguous decoding!\n')
                  return(out)
              }}, character(0))
          }
