#' @title Extract Encoded Variables From Encoded Split Or Tiled Data
#'
#' @description Extracts a single encoded variable from a \link[base]{list} or \link[listenv]{listenv}
#' of encoded matrices containing multiple encoded variables
#'
#' @param en a (named) list, or \link[listenv]{listenv} of matrices or a single matrix, all containing multiple
#'   encoded variables. See [oneHot] decoder for lists and matrices containing single encoded variables
#' @param name character, length 1. Column name as found in source data
#' @param ... default, empty. Used to convert the class of extracted matrix to 'dgCMatrix' or 'matrix'
#'
#' @details This function includes code from package "Matrix.utils" v 0.9.8, published under GPL-3
#' license, currently removed from CRAN. With thanks to the package Author!
#'
#' NOTE 1: If \code{name} is a source data column name that appears inside other column names, the extracted matrix
#' will combine all encoded matrices having this name inside their column names. Although the extracted matrix is a
#' proper matrix of encodings, it no longer represents \emph{a single} encoded data column. As result, upon decoding,
#' the [oneHot] decoder will report ambiguous decoding.
#'
#' NOTE 2: a warning reading either \emph{"single-column encoded matrix for ..."} or \emph{"number of columns of
#' result is not a multiple of vector length (arg 2) ..."} may appear when extracting an encoded categorical variable
#' from a list of encoded matrices. Most likely, this happens with low cardinality encoded variables. The warning
#' signals that most encoded matrices associated with respective variable contain subsets of only one category (level)
#' when, ideally, most of these matrices should contain a mixture of two or more categories or levels; thus, allowing
#' matrix row-binding by category's label. One or more of the following suggestions will solve the issue: a) shuffle
#' the data before encoding, b) increase the number of \code{rows} in data chunks when encoding, c) if memory allows,
#' opt for [tileHot] encoding single matrix output, as shown in Example 2.1, solution c.
#'
#' @returns A dense or sparse matrix of single encoded variable which can be decoded with the [oneHot] decoder.
#'
#' @seealso \link{oneHot}, \link{tileHot}
#'
#' @keywords extractor
#'
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'
#' # 1. mtcars data have all columns type "double"
#'
#' data(mtcars)
#' a = lapply(mtcars, oneHot, encode)                       # encode mtcars data
#' print(a)                                                 # list of sparse matrices
#' b = getEV(a, 'cyl')                                      # extract encoded "cyl" column
#' print(b)                                                 # a 32x3 sparse matrix
#' c = oneHot(b, decode)                                    # revert
#' identical(mtcars$cyl, c)                                 # FALSE. 'mtcars$cyl' is type "double"
#' isTRUE(all.equal(mtcars$cyl, c))                         # TRUE
#'
#' # 2. Warnings associated with low cardinality categorical variable
#'
#' # See tileHot() Examples for full decoding of a dataset
#'
#' # 2.1 Make 'csv' file
#' data(iris)                                               # low cardinality "Species"
#' tempf = tempfile(fileext = '.csv')
#' write.table(iris, tempf , sep = ',', row.names = FALSE, quote = FALSE)
#'
#' A = tileHot(readpath = tempf, rows = 14, splits = 3)     # encoded tiles list
#' print(A[[11]][[5]])                                      # e.g. one-column matrix
#' a = getEV(A, 'Species')                                  # warning
#' colSums(a)                                               # incorrect!
#'
#' # solution b
#' B = tileHot(readpath = tempf, rows = 60, splits = 3)     # increase number of rows
#' b = getEV(B, 'Species')                                  # still warning
#' colSums(b)                                               # incorrect!
#'
#' # Solution b) could work in combination with solution a)
#'
#' # solution c
#' C = tileHot(tempf, rows = 14, splits = 3, orn = TRUE)    # encoded matrix
#' c = getEV(C, 'Species')                                  # no warning
#' colSums(c)                                               # correct!
#'
#' unlink(tempf)
#'
#' # 2.2 Shuffled 'csv' file
#' tempf = tempfile(fileext = '.csv')
#' iris22 = iris[{ set.seed(327); sample.int(150) },]      # shuffled iris data
#' write.table(iris22, tempf , sep = ',', row.names = FALSE, quote = FALSE)
#'
#' A = tileHot(readpath = tempf, rows = 14, splits = 3)    # same as above
#'
#' #solution a
#' a = getEV(A, 'Species')                                 # no warning
#' colSums(a)                                              # correct!
#'
#' unlink(tempf)
#'
#' }
#'
#'

getEV = function(en, name, ...) {
                    on.exit(x <- NULL, add = TRUE); on.exit(r <- NULL, add = TRUE); on.exit(rr <- NULL, add = TRUE)
                   if(is.data.table(en) || is.data.frame(en)) {stop('data should be matrix or list only!', call. = FALSE)
                  } else if (is.matrix(en) || inherits(en, 'dgCMatrix')) {
                      nm = grepl(name, colnames(en))
                       x = if (...length() == 0L) en[, nm] else as(en[, nm], ...)
            if (isTRUE(sum(nm) > 1L)) {
            colnames(x) <- gsub(paste0(name, '\\.', ''), '', colnames(x))}}
                  else if (inherits(en, 'listenv') || is.list(en)) {
                    uen = unlist(en)
                    scm = sapply(seq(along=uen), \(i) dim(uen[[i]])[[2L]] == 1L)
                     tt = grepl(name, sapply(uen[which(scm == TRUE)], colnames)); uen <- NULL
                       if (isTRUE(sum(tt) > 0L)) warning('single-column encoded matrix for ', name, '!', call. = FALSE)
                       if (!is.null(names(en))) {
                          nm = grepl(name, names(en))
                           x = rBind.fill(en[nm], ...)}
                       else {
                           r = rapply(as.list(en), f = getEV, how = 'unlist', name = name)
                          rr = r[lengths(r) > 0L]
                           x = if (...length() == 0L) rBind.fill(rr) else as(rBind.fill(rr), ...)}}
                      return(x)
                  }
