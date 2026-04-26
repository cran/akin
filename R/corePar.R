#'@title Find Substring Combinations In Parallel
#'
#'@description
#'Internal function for \"fcommon\" - the parallelised version of function \"core\".
#'
#'@param chain character, length 1. A string
#'@param brows integer, length 1. Number of combinations matrix rows to be sent
#' to each worker (logical cpu). Default 1e6
#'@param tpe character, length 1. Strategy for the multiprocess plan. Choices: 'multisession',
#' 'multicore' and 'cluster'. Default 'sequential' corresponding to \code{tpe = NULL}
#'@param wo integer, length 1. Number of workers in the multiprocess plan
#'@param optMax integer, length1. Limit for size of objects sent to each worker. Default 500 MiB
#'
#'
#'@keywords internal
#'@noRd
#'


corePar = function(chain, brows = 1e6, tpe = strategy, wo = workers, optMax = maxSize) {
             'strategy' = 'workers' = 'maxSize' = NULL
            if (!is.null(tpe)) {with(future::plan(strategy = tpe, workers = wo), local = TRUE)}
            if(!is.null(optMax)) {
              ops = options(future.globals.maxSize = optMax)
                    on.exit(options(ops), add = TRUE)
            }
             sift = match.fun(sift, descend = FALSE)
                b = strsplit(as.character(chain), '')[[1L]]
               nc = nchar(chain)
                w = 2L:(nc - 1L)
                m = listenv()
              for (i in seq(along=w)) {
                  tot = comboCount(b, w[i])
                   rg = seq(1L, tot, by = brows)
    m[[i]] = future_sapply(rg, \(lower) {
             end = min(lower + brows - 1L, tot)
         comboGeneral(b
                    , m = w[i]
                    , FUN = sift
                    , lower = lower
                    , upper = end
                    , chain = chain
                    )
         }, simplify = TRUE, USE.NAMES = FALSE)

   m[[i]] = m[[i]][nchar(m[[i]]) > 1L] |> unique()
              }
          unlist(m) |> unique()
}
