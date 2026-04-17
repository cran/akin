#' @title Tile And Write Tiled Data To Disk
#'
#' @description Splits long and wide data files in lists of disjoint tiles for further processing.
#'
#' @param readpath character length 1. Full path to the source file
#' @param writepath character length 1. Full path to the destination file
#' @param rows integer length 1. Number of rows in each subset. Internally, it determines the total
#'    number of subsets before the vertical split
#' @param splits integer, length 1. Number of vertical data splits in each above subset. See [splitV]
#' @param ... extra arguments to [splitH] e.g. \code{dropcols} for columns dropped from source data
#'
#' @details Facilitates local operations on small size tiles by partitioning the data horizontally and vertically.
#' The list of tiles can be written to disk as "rds"" file when a \code{writepath} destination is given. The written
#' data can then be read entirely or in subsets (see Example 2).
#'
#' NOTE: This utility uses background processing. Check "Security Considerations" in \strong{callr} package documentation.
#'
#' @returns A \link[listenv]{listenv} of "data.table" class tiles. When \code{writepath} is given, it produces
#' a "rds" file containing data tiles.
#'
#' @seealso [splitH], [splitV], [tileHot], \link[base]{readRDS}
#'
#' @keywords splitting
#'
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'
#' # Make a 'csv' file
#'
#' data(iris)
#' tmpf = tempfile(fileext = '.csv')
#' write.table(iris, tmpf , sep = ',', row.names = FALSE, quote = FALSE)
#'
#' # 1. Tile data
#'
#' a = tileData(tmpf, rows = 10, splits = 3)     # 10x2 and 10x1 tiles
#' class(a)                                      # listenv, environment
#' str(a)                                        # nested list
#'
#' tmpf1 = tempfile(fileext = '.rds')            # new location
#'
#' # 2. Write tiled data
#'
#' tileData(tmpf, tmpf1, rows = 10, splits = 3)
#' a = readRDS(tmpf1)[[1]]                  # partial read from new location
#' print(a)                                     # list component
#'
#' unlink(tmpf)
#' unlink(tmpf1)
#' }
#'
#'

tileData = function(readpath, writepath = NULL, rows, splits, ...) {
              on.exit(w <- NULL, add = TRUE)
                v = listenv()
         readpath = if (file.exists(readpath)) {normalizePath(readpath)
                    } else {stop('no file at source location!', call. = FALSE)}
             info = match.fun(info, descend = FALSE)
             dims = r_bg(function(info, readpath) {
                                          info(readpath, show.info = FALSE)
                            }, args = list(info, readpath), user_profile = FALSE, supervise = TRUE)
                r = splitH(readpath)
              if('ready' %in% dims$poll_io(100)['process']) {
                           tot = dims$get_result()[[1L]]
                          nmes = dims$get_result()[[2L]]} else {
                                                                 dims$wait()
                                                                 tot = dims$get_result()[[1L]]
                                                                nmes = dims$get_result()[[2L]]}
          subsets = ceiling(tot/rows)
           for (i in 1:subsets) v[[i]] = splitV(r(rows, ...), splits)
                w = lapply(v, as.list)
           if (!is.null(writepath) && is.character(writepath) && length(writepath) == 1L) {
                     invisible(saveRDS(w, writepath))
             message('\nfile saved to', path.expand(writepath), '\n')
           } else return(w)
     }
