#' @title Read Or Write Subsets Of Data Files From Or To Disk
#'
#' @description Reads or writes data files from/to disk in disjoint subsets. This is a two-stage function (see Examples).
#'
#'
#' @param readpath character length 1. Full path to the source file
#' @param writepath character length 1. Full path to the destination file
#'
#' @details
#' Above arguments apply to Stage 1 only. The arguments for Stage 2 function, which is the output of Stage 1,
#' are the following:
#'
#' \code{rows} integer, length 1. Number of rows per subset. When rows = Inf, the data can be either
#'     copied \emph{as is} or moved to a new location
#'
#' \code{seq} logical, default TRUE: read discrete subsets. Otherwise, progressively appended subsets
#'     from first to current
#'
#' \code{dropcols} character of \code{length < ncol(data)}. Columns to drop. Works only when \code{rows} is
#'     finite. Replaces argument \code{select} from [data.table::fread]
#'
#' \code{how} symbol. Works only when \code{rows = Inf} and \code{writepath} location is given.
#'     Options: \code{how = scp}, data file is copied \emph{as is} to \code{writepath} location;
#'     \code{how = mv}, data file is moved to \code{writepath} location
#'
#' \code{print} logical, default TRUE, each subset written to disk is shown in console. Setting print to
#'     FALSE could increase writing speed
#'
#' \code{orn} logical, default FALSE. When TRUE, the \strong{o}riginal data \strong{r}ow \strong{n}umbers
#'     are shown in each subset
#'
#'
#' The main purpose of this utility is to bring manageable subsets from very large data into the working
#' environment for further processing when \code{writepath = NULL}. When \code{orn = TRUE}, each subset
#' receives a new column named "srn" showing \strong{s}ource data \strong{r}ow \strong{n}umbers. This column
#' is absent from subsets written to disk regardless of \code{orn} value. The source data file can be any
#' type of file readable by [data.table::fread].
#'
#' At the first stage:
#'
#'   * the utility retrieves information about source data without loading them into memory and also provides
#' the new function which, in the second stage:
#'
#'   * reads source data in successive disjoint subsets (\code{rows < Inf}) and brings them into the work
#' environment (writepath = NULL), \strong{or}
#'
#'   * writes subsets to \code{writepath} location appending them automatically to the destination file.
#' During writing, if (\code{print = TRUE}) the displayed subsets are just printouts (class "NULL"). When
#' \code{writepath = NULL}, displayed subsets are objects.
#'
#' There is a functional difference between \code{rows = Inf} and \code{rows = nrow(data)}:
#'
#'   * when \code{rows = Inf}, the size of source data is irrelevant. They can be either copied (\code{how = scp})
#' or moved (\code{how = mv}) to \code{writepath} destination without being loaded into memory.
#'
#'   * when \code{rows} has finite value, the size of source data is relevant and data columns can be dropped.
#'
#' @returns At stage 1, displayed information and a function (a closure). At stage 2, a "data.table" class subset
#' of data or a printout of said subset when written on disk.
#'
#' @references Part of internal code for Stage 1 was inspired by
#' \href{https://github.com/Rdatatable/data.table/issues/7169}{data.table Issue# 7169}
#'
#' @seealso Linux commands \strong{scp} and \strong{mv}
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
#' data(mtcars)
#' tmpf = tempfile(fileext = '.csv')
#' write.table(mtcars,tmpf , sep = ',', row.names = FALSE, quote = FALSE)
#'
#' # 1. Read data file step by step
#'
#' # 1.1 Get information on data
#' r = splitH(readpath = tmpf)                             # stage 1
#' class(r)                                                # function
#'
#' # 1.2 Read data iteratively                             # stage 2
#' a = r(rows = 11, dropcols = c('am', 'vs'))              # iter1 no original row numbers
#' b = r(rows = 11, dropcols = c('am', 'vs'), orn = TRUE)  # iter2 w. original row numbers
#' c = r(rows = 11, dropcols = c('am', 'vs'))              # iter3 the last subset
#' d = r(rows = 11, dropcols = c('am', 'vs'))              # iter4 stop! Return to stage 1
#' print(list(a, b, c))
#'
#' # 2. Read data file completely
#'
#' r = splitH(readpath = tmpf)                             # stage 1
#' n = ceiling(32/13)                                      # read 13 rows at a time
#' a = replicate(n, r(rows = 13), simplify = FALSE)        # read file
#' class(a)                                                # list
#' print(a)                                                # a list of tables
#'
#' tmpf1 = tempfile(fileext = '.csv')                      # new location
#'
#' # 3. Iteratively write to new location
#'
#' r = splitH(readpath = tmpf, writepath = tmpf1)          # stage 1
#' n = ceiling(32/11)                                      # 11 rows each time
#' invisible(
#'  replicate(n, r(rows = 11) , simplify = FALSE)          # write to new location
#'  )
#' a = data.table::fread(tmpf1)                            # check result
#' dim(a)
#' print(head(a))
#'
#' unlink(tmpf1)
#'
#' tmpf2 = tempfile(fileext = '.csv')                      # new location
#'
#' # 4. Move file from tmpf to another location
#'
#' r = splitH(readpath = tmpf, writepath = tmpf2)          # stage 1
#' r(rows = Inf, how = mv, print = FALSE)                  # move to new location
#' a = data.table::fread(tmpf2)                            # check result
#' print(head(a))
#'
#' unlink(tmpf)
#' unlink(tmpf2)
#'
#' }
#'

splitH = function(readpath, writepath = NULL) {
                      rows = 'how' = tot = i = nmes = j = '<<-' = r = NULL
                  on.exit(i <- 0L, add = TRUE)
                  readpath = if (file.exists(readpath)) {normalizePath(readpath)
                             } else {stop('no file at source location!', call. = FALSE)}
                      info = match.fun(info, descend = FALSE)
                      dims = info(readpath)
                       tot = dims[[1L]]; nmes = dims[[2L]]
                         j = as.call(c(quote(list), list(srn = srn), eval(rename(nmes))))
                         r = data.table(NULL)
                function(rows, seq = TRUE, dropcols = NULL, how, print = TRUE, orn = FALSE) {
                      skip = eval(jump)
                        dt = if (skip > tot || is.infinite(rows) && i > 0L) {
                                stop('data reading was completed once!\n', call. = FALSE)
                            } else {
                                   if (isTRUE(print)) cat('\n\nsubset', i + 1L, ':\n')
                             fread(file = readpath, skip = skip, nrows = rows, col.names = nmes
                                 )[, j, env = list(j = j)] }
                     if (!is.null(dropcols)) dt[, let(dropcols, NULL), env = list(dropcols = substitute(dropcols))]
                     if (isTRUE(seq)) dt else dt = local({r <<- rbindlist(list(r, dt), use.names = TRUE)})
                        i <<- i + 1L
                     if (!is.null(writepath)) {
                         if (isTRUE(print)) print(dt)
                         if (is.infinite(rows)) {
                               how = substitute(how)
                              flag = if (how == 'scp') '-p' else '-b'
                         system2(how, c(flag, '-v', readpath, path.expand(writepath)), stdout = TRUE)
                         } else {
                           if (!seq) {
                              i <<- i - 1L
                    stop('when writing data, \"seq\" argument must be TRUE', call. = FALSE)}
                        if (isTRUE(orn)) dt[, let('srn', NULL)]
                        invisible(fwrite(dt, writepath, append = TRUE))
                    on.exit(message('\n\ndata have been written to ', writepath, '\n'), add = TRUE)
                 }} else return(dt)
            }}

