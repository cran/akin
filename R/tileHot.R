#' @title One-hot Encoder Of Tiled Data
#'
#' @description One-hot encodes tiled data.
#'
#' @param readpath character, length 1. Path to source data that is readable with [data.table::fread]
#' @param rows integer length 1. Number of rows in each data subset. Internally, it determines the total
#'   number of subsets before the vertical split
#' @param splits integer, length 1. Number of vertical data splits in each subset, see [splitV]. Recommended for
#'   very wide data frames. When \code{splits = 0}, no vertical splitting occurs
#' @param omc character length 1. \strong{O}utput \strong{m}atrix \strong{c}lass. Default, "dgCMatrix". Other option:
#' "matrix"
#' @param ... reserved for [splitH] function arguments, such as \code{dropcols} or \code{orn = TRUE} which is needed for
#' single matrix output
#'
#' @details This utility reads the data in disjoint subsets, tiles them and then one-hot encodes each tile. Encoded
#' tiles are returned as nested list of matrices, as a single matrix, as data frame or as a two-component data frame
#' and sparse matrix list, decided through combinations of \code{dropcols}, \code{omc} and \code{orn} values.
#'
#' NOTE 1: traceability is assured by assembling the data as character names and values from columns marked for encoding.
#' As side effect, at run time the encoding is reported as being applied to "integer(ish)" values only with no loss in
#' accuracy. Empty source data columns gain the "NA" suffix and become single-column, single-valued matrices.
#'
#' NOTE 2: this utility implements background, processing. Check "Security Considerations" in \strong{callr}
#' package documentation.
#'
#' @returns
#'   * When \code{orn = FALSE}, an unnamed \link[listenv]{listenv} of sparse matrices. Recommended for very large
#' source data files. Before proceeding with list output, read NOTE 2 in [getEV] documentation. See Examples 1 and 2.
#'
#'   * When \code{orn = TRUE}, a matrix.
#'
#' NOTE 3: In this case, row and column binding operations were avoided to prevent situations described in NOTE 2, [getEV]
#' documentation. As result, the output matrix is gradually populated instead of being gradually expanded.
#'
#' While \code{orn = TRUE} and \code{dropcols != NULL}:
#'
#'   * When \code{omc = 'matrix'}, a \link[data.table]{data.table} containing encoded, as well as unencoded, dropped
#' columns placed in the leftmost positions.
#'
#'   * When \code{omc = 'dgCMatrix'}, a two-component \link[listenv]{listenv}: a data table containing dropped,
#' unencoded columns and a sparse matrix containing the encoded columns. The row order in both components is identical.
#' See Examples below, and Example 2 in [getEV] documentation.
#'
#' NOTE 4: In all above cases, specific encoded variables can be obtained with [getEV] extractor. When \code{orn = TRUE},
#' [oneHot] \emph{decoded} variables extracted from matrix outputs return named vectors having row numbers as names.
#'
#' @seealso [splitH], [splitV], [oneHot], \link[listenv]{listenv}, \link[Matrix]{Matrix}
#'
#' @keywords conversion
#'
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'
#' # 1. Shuffled data
#'
#' tempf = tempfile(fileext = '.csv')
#' data(iris)
#' iris22 = iris[{ set.seed(327); sample.int(150) },]          # shuffled iris data
#' rownames(iris22) <- NULL                                    # remove shuffled row names
#' write.table(iris22, tempf, sep = ',', row.names = FALSE, quote = FALSE)
#'
#' # 1.1 Output as List
#' # In most cases, list output requires shuffled data!
#'
#' A = tileHot(readpath = tempf
#'           , rows = 14, splits = 3, print = FALSE)           # encoded data tiles
#' print(A)                                                    # a listenv
#' print(A[[1]])                                               # a snapshot
#'
#' # 1.2 Retrieve iris22 data from encoded list output
#' X = sapply(names(iris22), \(n) getEV(A, n))                 # extract all encoded columns
#' Y = lapply(
#'          lapply(X, oneHot, decode)
#'                                , unname)                    # decoded columns are named vectors!
#' d = as.data.frame(Y)
#' identical(iris22, d)                                        # TRUE
#'
#' unlink(tempf)
#'
#' # 2. Unshuffled data
#'
#' # Make unshuffled data 'csv' file
#' tempf = tempfile(fileext = '.csv')
#' write.table(iris, tempf, sep = ',', row.names = FALSE, quote = FALSE)
#'
#' # 2.1 Output as list
#' # List output fails low cardinality variables on unshuffled data.
#'
#' E = tileHot(readpath = tempf
#'               , rows = 14, splits = 3, print = FALSE)      # same as above
#'
#' # 2.2 Retrieve iris data from encoded list output
#' V = sapply(names(iris), \(n) getEV(E, n))                  # warning
#' W = lapply(
#'          lapply(V, oneHot, decode)
#'                                , unname)                   # decoded columns are named vectors!
#' dd = as.data.frame(W)
#' identical(iris, dd)                                        # FALSE
#' all.equal(iris, dd)                                        # low cardinality "Species"
#'
#' # 2.3 Output as matrix
#' # Matrix output handles low cardinality variables. No data shuffling required.
#'
#' m = tileHot(readpath = tempf                               # low cardinality "Species"
#'           , rows = 14
#'           , splits = 3
#'           , orn = TRUE,                                    # needed for matrix output
#'           , print = FALSE)
#' print(m)                                                   # 150x126 sparse matrix
#'
#' # 2.4 Retrieve iris data from encoded matrix output
#' P = sapply(names(iris), \(n) getEV(m, n))                  # extract encoded columns
#' Q = lapply(
#'            lapply(P, oneHot, decode)
#'                               , unname)                    # decoded columns are named vectors!
#' R = as.data.frame(Q)
#' identical(iris, R)                                         # TRUE
#'
#' # 2.5 Output as "data.table" class
#' D = tileHot(readpath = tempf
#'           , rows = 14
#'           , splits = 3
#'           , omc = 'matrix'                                 # encoded dense matrix
#'           , dropcols = c('Petal.Width', 'Petal.Length')    # unencoded columns
#'           , orn = TRUE                                     # needed for matrix output
#'           , print = FALSE)
#' print(head(D, 10))                                         # a "data.table" class
#' dim(D)                                                     # 150x63
#'
#' # 2.6 Output as a 2-component list
#' Dl = tileHot(readpath = tempf
#'           , rows = 14
#'           , splits = 3
#'           , omc = 'dgCMatrix'                              # the default class
#'           , dropcols = c('Petal.Width', 'Petal.Length')    # unencoded columns
#'           , orn = TRUE                                     # needed for matrix output
#'           , print = FALSE)
#' print(Dl)                                                  # 2-component listenv
#' print(Dl[[1]])                                             # unencoded columns
#' print(Dl[[2]])                                             # encoded sparse matrix
#'
#' # iris data can be retrieved from the Dl list in similar fashion described above
#'
#' unlink(tempf)
#'
#' }
#'

tileHot = function(readpath, rows, splits, omc = 'dgCMatrix', ...) {
                    encode = as.symbol('encode')
                  readpath = if (file.exists(readpath)) {normalizePath(readpath)
                             } else {stop('no file at source location!', call. = FALSE)}
                  delayedAssign('rn.x', 1:n.x )
                      info = match.fun(info, descend = FALSE)
                      ptot = r_bg(function(info, readpath) {
                                                         info(readpath, show.info = FALSE)[[1L]]
                               }, args = list(info, readpath), user_profile = FALSE, supervise = TRUE)
                  on.exit(M <- NULL, add = TRUE); on.exit(rez <- NULL, add = TRUE); on.exit(en <- NULL, add = TRUE)
                       evd = if (...length() > 0L) {
                                    as.list(match.call(expand.dots = FALSE)$...)}
                       dropcols = eval(evd$dropcols)
                       orn = eval(evd$orn)
                       omc = match.arg(omc, choices = c('matrix', 'dgCMatrix'))
                     prest = if (!is.null(dropcols) && isTRUE(orn)) {
                                                r_bg(function(fread, readpath, dropcols) {
                                                              fread(readpath, select = dropcols)
                              }, args = list(fread, readpath, dropcols), user_profile = FALSE, supervise = TRUE)}
                         r = splitH(readpath)
                       n.x = if('ready' %in% ptot$poll_io(500)['process']) {
                                                                             ptot$get_result()}
                                                                      else {
                                                                             ptot$wait()
                                                                             ptot$get_result()}
                       if (!is.null(dropcols)) message('- columns dropped:', sQuote(dropcols), '\n')
                         v = listenv(); en = listenv(); idx = double(); n = structure(character(), names = NULL)
                   subsets = ceiling(n.x/rows)
                     for (i in 1:subsets) {
                       if (splits > 0L) {
                            v[[i]] = do.call(splitV, list(r(rows, ...), splits))
                            for (j in seq(along=v[[i]])) {
                              if (exists('srn', as.environment(v[[i]][[j]]))) {
                                                      idx = v[[i]][[j]]$srn
                                                      v[[i]][[j]][, let('srn', NULL)]}}
                            v[[i]] = lapply(v[[i]], \(k) k[, lapply(names(.SD), \(i) paste0(i, '.', .SD[[i]]))])
                    } else if (splits == 0L) {
                                  v[[i]] = r(rows, ...)
                             if (exists('srn', as.environment(v[[i]]))) {
                                                     idx = v[[i]]$srn
                                                     v[[i]][, let(srn, NULL)]}
                                  v[[i]] = v[[i]][, lapply(names(.SD), \(i) paste0(i, '.', .SD[[i]]))]}
                            n = c(n, unique(rapply(v[[i]], f = \(i) Reduce(\(x,y) `c`(x,y), i), how = 'unlist')))
                      en[[i]] = rapply(object = v[[i]], f = oneHot, how = 'unlist', type = encode)
                       v[[i]] <- NULL
                      if (isTRUE(orn)) {
                        lapply(seq(along=en[[i]]), \(j) dimnames(en[[i]][[j]])[[1L]] <- idx)
                   }}
                   if (isTRUE(orn)) {
                           ll = unique(n); n.l = length(ll); n <- NULL
                            M = eval(M)
                           iU = unlist(en, TRUE, TRUE)
                     for (i in iU) {
                            v = as(i, omc)
            try(M[fmatch(rownames(v), rownames(M), 0L, NULL), fmatch(colnames(v), colnames(M), 0L, NULL)] <- v)}
                  if (!is.null(dropcols)) {
                         rest = if('ready' %in% prest$poll_io(500)['process']) {
                                                                                 prest$get_result()}
                                                                          else {
                                                                                 prest$wait()
                                                                                 prest$get_result()}
                  message('\n\nstructuring the output may take a while ...\n')
                                if (omc %in% 'matrix') {
                                  rez = data.table(rest, M)}
                                else {
                                  rez = listenv(rest, M)}
                                    r = idx = rest <- NULL
                                cat('\nFinished!\n\n')
                                return(rez)} else return(M)
                   } else return(en)}

