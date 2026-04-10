#'@title Extract A Proportional Stratified Sample From A Data Set
#'
#'@description Obtains a proportional stratified sample from any data convertible to "data.table"
#' class containing categorical variables.
#'
#'@param X any data array convertible to "data.table" class
#'@param target character length 1. The name of column considered to be the root stratum. For example, the name of the
#' 'target' categorical column in a classification training set. This argument should always have a value
#'@param stratum character of length <= \code{ncol(data) - 1}. Default, NULL. Names of other categorical data
#'  columns which contribute to stratification deepening
#'@param size integer length 1. Default, none. Value set by User. In this case, it is upper-bounded by the size of the
#'  thinnest stratum having more than one row. Setting \code{size} value above this bound requires sampling with
#'  replacement
#'@param thresh integer, length 1. Default, none. An automatic switch between sample size calculation formulae.
#'  Can be set when \code{size} is missing from call. It can take as value any of the stratum thicknesses shown in the
#'  output message
#'
#'  NOTE: it is recommended that \emph{both} \code{size} and \code{thresh} values be missing from call until
#'  information on stratification becomes available after first run
#'@param seed integer length 1. Seed value for output reproducibility
#'@param indx logical. Default TRUE, returns the sample row index only. FALSE, returns the sampled data
#'@param dis symbol. Default NULL. One of the density or function \link[stats]{distributions} used for
#'  generating probability vectors for probabilistic sampling
#'@param args list of arguments required by distributions as described in \link[stats]{distributions} documentation.
#'  Default, none. \strong{NB} The list should \emph{never} include the first argument (\strong{x} or \strong{n})
#'  required in documentation, as it is collected internally from each stratum
#'
#'  NOTE: Even if \code{seed} is set, the sample row index changes if either the distribution in \code{dis} or the
#'  values in \code{args} is changed
#'@param ext logical, default FALSE. When TRUE, expands the output sampled data with the following extra columns:
#'  \strong{row} - sample rows, \strong{strat} - stratum, \strong{n} - stratum total rows (i.e. thickness)
#'  and \strong{size} - the sample size extracted from each stratum. Requires \code{indx = FALSE}
#'@param replace logical, default FALSE. When TRUE, sampling with replacement if \code{size} is present in call
#'  and its value is larger than the thickness of the thinnest stratum with more than one row
#'@param verbose logical, default TRUE, display messages
#'
#'@details This utility was designed to find a true sample representation of the data under current stratification
#' by matching closely the proportionality of strata as long as argument \code{size} is missing from call.
#' Each distinct combination of \code{target} and \code{stratum} levels defines a stratum. For minimal
#' stratification, argument \code{target} must always have a value present in call. All one-row strata, when
#' formed, are simply appended to the compounded output.
#'
#' \code{size}. As column in the extended output, it represents the size of the sample extracted from each
#' stratum, internally derived proportional to each stratum thickness hence, unbounded by the thinnest
#' stratum with more than one row. Deep stratification along with high cardinality and imbalance may severely
#' restrict the size of the compounded output which is the sum of all strata sizes plus the number of one-row
#' strata. The sampling occurs at stratum level except for one-row strata for which \code{size = 0} is interpreted
#' as "no sampling".
#'
#' As function argument, it is interpreted as the largest sample size without replacement that can be requested,
#' being bounded by the thinnest stratum with more than one row. The presence of \code{size} in call alters
#' the proportionality since each stratum - except one-row strata - contributes equally to the output size, equal
#' to the number of strata times the \code{size} value plus the number of one-row strata.
#'
#' \code{thresh}. Automatic switch that modifies stratum sample size calculation method based on the extreme stratum
#' thickness values, stratification depth and total data rows. Internally, it searches for the formula that finds
#' at least one sample size accommodating the thinnest stratum with more than one row. Messages are displayed at runtime
#' although, in most cases the formula is found at first iteration. When \code{thresh >= nrow(data)}, each stratum is sampled
#' proportional with the ratio between thinnest and thickest strata, which may lead to a relatively small size output.
#' All other \code{thresh} values compromise slightly between output size and proportionality (see Example 3).
#'
#' ## Probabilistic Sampling
#'
#' \code{dis}. The \code{prob} argument in [base::sample] cannot be used as required since the length of probability vector
#' varies with stratum thickness. Herein, strata probability vectors are determined by the distribution specified in
#' argument \code{dis} which associates each stratum with a probability vector of thickness length. When \code{args} is
#' missing from call, \code{dis} uses the default argument values for respective distribution. An error is thrown when the
#' probability vector has insufficient number of non-zero values. See package \strong{stats}, "Distributions" documentation.
#'
#' NOTE: Random variate generators i.e. the \strong{r*} version of \link[stats]{distributions}, generate vectors of absolute
#' \emph{random deviate} values which play the role of pseudo-probabilities conformant with the requirements listed in
#' [base::sample] documentation.
#'
#'@returns A proportional or non-proportional stratified sample (depending on whether \code{size} is absent or present
#' in call), either as row index or as sampled data, compounded from random or probability samples taken from each
#' stratum. Informative messages are displayed. Existing data row names are preserved in the output case in which, the sampled
#' data output gains the column named "rn".
#'
#'@seealso \link[base]{sample}, \link[stats]{distributions}
#'
#'@keywords sampling
#'
#'@export
#'
#'@examples
#'
#' if (interactive()) {
#'
#' # 1. Row index for sampling
#'
#' data(mtcars)
#' rowID = stratify(mtcars
#'                , target = 'cyl'
#'                , stratum = c('vs', 'am')
#'                , seed = 314)                                  # display information
#' print(rowID)                                                  # integer
#'
#' # 2. Sampled data with extra-columns
#'
#' smp = stratify(mtcars
#'             , 'cyl'
#'             , c('vs', 'am')
#'             , seed = 314
#'             , indx = FALSE
#'             , ext = TRUE)                                     # extra columns
#' print(smp)
#' identical(rowID, smp$row)                                     # TRUE
#'
#' # 3. Impact of "thresh" value on output size
#'
#' sl = list()
#' thresholds = c(2, 4, 12, 32)                                  # stratum thicknesses
#'
#' for (t in seq(along=thresholds)) {
#'                   sl[[t]] = stratify(mtcars
#'                                   , 'cyl'
#'                                   , c('am', 'vs')
#'                                   , thresh = thresholds[t]
#'                                   , seed = 314
#'                                   , indx = FALSE, ext = TRUE)
#'                 }
#' names(sl) = quote(thresholds)
#' print(sl)                                                     # stratified samples
#'                                                               # of various sizes
#'
#' # 4. Probabilistic sampling
#'
#' rowIDn = stratify(mtcars
#'              , 'cyl'
#'              , c('vs', 'am')
#'              , seed = 314
#'              , dis = pnorm                                    # Normal distribution
#'              , args = c(mean = 1, sd = 3))                    # no first argument!
#' rowIDb = stratify(mtcars
#'              , 'cyl'
#'              , c('vs', 'am')
#'              , seed = 314                                     # same seed
#'              , dis = pbeta                                    # Beta distribution
#'              , args = c(shape1 = 1, shape2 = 3))              # no first argument!
#'
#' # Same seed but changing the distribution changes the sample row index
#' identical(rowIDn, rowIDb)                                     # FALSE
#'
#'}
#'

stratify = function(X, target, stratum = NULL, size, thresh, seed = NULL, indx = TRUE
                  , dis = NULL, args = list(), ext = FALSE, replace = FALSE, verbose = TRUE) {
                   n = strat = Mx = j = NULL
                   stopifnot('\n \"target\" should not be part of \"stratum\"!\n'= isTRUE(!target %in% stratum))
              if (!is.data.table(X)) data = as.data.table(X, keep.rownames = TRUE) else data = X; X <- NULL
                 strata = if (is.null(stratum)) quote(target) else quote(c(target, stratum))
                      s = match.fun(s, descend = FALSE)
         checkCondition = match.fun(checkCondition, descend = FALSE)
                 sample = match.fun(sample, descend = FALSE)
                 delayedAssign('Mx', max(dtt$n, na.rm = TRUE)); delayedAssign('m', min(dtt[n > 1L]$n, na.rm = TRUE))
                 delayedAssign('M', sum(dtt$n, na.rm = TRUE)); delayedAssign('med', ceiling(median(dtt[n > 1L]$n, na.rm = TRUE)))
                data[, let(c('row', 'strat', 'n'), list(.I, .GRP, .N)), by = eval(strata), env = list(strata = strata)]
                    n.d = names(data)
              if (missing(size)) {
                    dtt = unique(data, by = 'strat')
                 thresh = if (missing(thresh)) 1L else thresh
                    dtv = dtt[, list(size = fifelse(n == 1L, 0L, s(n, m, med, Mx, M, thresh)), strat = strat, n = n)]
                     se = dtv$size; ss = unique(se); ne = dtv$n
              condition = any(se > ne) || all(ss > m)
                 if (isTRUE(condition)) {
                   message(cat('\n\nfound inadequate sample size, switching ...\n'))
                    dtv <- NULL
                    dtt = do.call(checkCondition, list(m, med, M, dtt))}
               else dtt = dtv
                     ss = unique(dtt$size)
               if (verbose) cat('\n- strata sample sizes are:', sort(ss),
                       '\n- median and thickest strata have', med,'and', Mx,'rows each\n')
                   data = dtt[data, on = 'strat', allow.cartesian = TRUE][, let(grep('i\\.', names(.SD)), NULL)]
                   setcolorder(data, n.d)
               if (verbose) cat('- maximum sample size without replacement is', m, 'under current stratification\n\n')
              } else size
                     ss = m = med = M = Mx = dtv <- NULL
             if(!is.null(seed)) set.seed(seed)
             if (!is.null(dis)) args = c(quote(row), args)
                     id = data[, if(any(n == 1L)) row else j, by = strat, env = list(j = CALLs)]$id
            return(eval(retz))
   }
