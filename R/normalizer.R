#' @title Scaling And Thresholding Of Numeric Variables
#'
#' @description Implements classical methods for data scaling: range, z-score normalization, location and location-scale
#' normalization, as well as data thesholding through the simplest form of ReLU rectifier. Missing values are removed in
#' all cases.
#'
#' @param x numeric vector, length > 1. Variable to be scaled or filtered
#' @param how symbol. Choices are \strong{range}, \strong{stdev} or \strong{relu}
#' @param filter character, length 1. Default NULL. Choices "positive", "negative". Requires \code{how = relu}
#' @param ... list reserved for User input of paired values, statistic or otherwise. The list uses individual
#' ellipsis arguments therefore, order of values needs be respected at all times e.g. when \code{how = range}  min value
#' \emph{first}, max value \emph{second}. When \code{how = stdev}, mean value \emph{first}, standard deviation \emph{second}.
#'
#' @details
#' Normalization (scaling) can be applied locally on subsets of \code{x} when User inputs the values in \code{...} list.
#' Otherwise, the scaling is global i.e. it is applied to \code{x} as a whole. No assumptions regarding the underlying distribution
#' of \code{x} were made.
#'
#' \code{how != relu}. When \code{...} is empty, the function uses the sample statistics of \code{x} e.g. the mean, range
#' or standard deviation. Otherwise, it uses values inputted by User case in which, \emph{location-scale} normalization
#' requires \code{how = stdev} and the \code{...} list filled as follows: the \code{min(x)} \emph{or} \code{max(x)}, or any other
#' value first and \code{sd(x) > 0} or any other positive value second. In particular, \emph{location} normalization works similarly
#' but with the second value = 1. Other location types, e.g. x/max(x), are obtainable.
#'
#' NOTE: when \code{...} is populated with custom values, all other argument values must be present in call (see Examples).
#'
#' \code{how = relu}. This option acts as numeric thresholding locally as well as globally. It stands for \strong{re}ctified
#' \strong{l}inear \strong{u}nit and involves no statistics. It applies to numeric types that have ordering property (double,
#' integer). On return, all \code{x} attributes are dropped.
#'
#' When \code{filter = 'positive'}, all negative values are set to zero while all other values remain unchanged. Alternatively,
#' when \code{filter = 'negative'}, all negative values remain unchanged while all other values are set to zero. The "negative"
#' option was added for symmetry.
#'
#' @returns Numeric. When missing \code{...} and \code{how != relu}, scaled values using \code{x} own sample statistics.
#' Otherwise, scaling is based on values inputted by User. When \code{how = relu}, \code{x >= 0} or \code{x <= 0}, depending
#' on \code{filter} setting.
#'
#' @references \href{https://en.wikipedia.org/wiki/Ancillary_statistic}{Ancillary
#'   Statistic} for location and location-scale distributions
#'
#'@keywords thresholding
#'
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'
#' # 1. ReLU thresholding
#'
#' x = { set.seed(223); sort(runif(10, -3, 3)) }
#' y = score(x, relu, 'positive'); y
#' z = score(x, relu, 'negative'); z
#'
#' # 1.1 ReLU Plot
#' olp = par(no.readonly = TRUE)
#' par(list(mar = c(1,1,1,1), mgp = c(0,0,0), tcl = -0.01, pty = 's'))
#' plot(x, y, type = 'l', col = 'steelblue', lwd = 2 ,
#'        xlim = c(min(x), max(x)), ylim = c(min(x), max(x))
#'      , ylab = expression(ReLU(x)), xaxs = 'i', yaxs = 'i', axes = FALSE, cex.lab = 0.7)
#' axis(1, pos = 0, cex.axis = 0.6) ; axis(2, pos = 0, cex.axis = 0.6)
#' points(x, z, type = 'l', col = 'orangered', lwd = 2)
#' legend('topleft', legend = c('positive', 'negative'),
#'        col = c('steelblue', 'orangered'), pch = 'l', lwd = 2, cex = 0.6, bty = 'n')
#' par(olp)
#'
#' # 2. Location and location-scale
#'
#' # 2.1 Location (e.g. "x - max(x)")
#' x = 1:10
#' M = max(x)
#' std = 1
#' a = score(x, stdev, NULL, M, std); a
#'
#' # 2.2 Location (e.g. "x/max(x)")
#' m = 0                                                            # the mean
#' M = max(x)                                                       # or any value
#' b = score(x, range, NULL, m, M); b
#'
#' # 2.3 Location-scale (e.g. "(x - max(x))/sd(x)")
#' M = max(x)                                                       # or any value
#' std = sd(x)                                                      # or any value > 0
#' c = score(x, stdev, NULL, M, std); c
#'
#' # m, M and std above can be replaced with any values decided by User
#'
#' # 3. Classical normalization
#'
#' # 3.1 Range
#' d = score(x, range); d
#'
#' # 3.2 z-score
#' e = score(x, stdev); e
#'
#' # 4. Local vs. global z-score normalization
#'
#' data(mtcars)
#'   x = mtcars$wt
#'   m = mean(x)
#' std = sd(x)
#'
#' ll = split(x, f = as.factor(mtcars$cyl))        # partitioned x
#'
#' # 4.1 Local scaling
#' aa = lapply(ll, score, stdev, NULL, m, std)     # filled ... list
#' na = unlist(aa, FALSE, FALSE)
#'
#' # 4.2 Global scaling
#' nb = score(x, stdev)
#'
#' # 4.3 Local as well as global hold
#' identical(sort(na), sort(nb))                   # TRUE
#'
#' }
#'

score = function(x, how, filter = NULL, ...) {
                       r = NULL
                     how = substitute(how)
                     how = match.arg(as.character(how), choices = c('range', 'stdev', 'relu'))
               stopifnot('x must be numeric vector of length > 1' = is.numeric(x) && length(x) > 1L)
               if(anyNA(x)) {
                            warning('\nall missing values were removed!', call. = FALSE)
                     x = x[!is.na(x)]}
                   n.x = length(x)
                     M = call('max', x, na.rm = TRUE); m = call('min', x, na.rm = TRUE)
                  mean = call('mean', x, na.rm = TRUE); sd = call('sd', x, na.rm = TRUE)
              switch(how
                 , range = {
                   if (...length() == 0L) {
                         M = eval(M)
                         m = eval(m)
                   stopifnot('zero range found!'= max(m, M) > min(m, M))
                   } else {
                        m = ..1
                        M = ..2
                    if (any(!is.numeric(m), !is.numeric(M))) stop('quantities must be numeric!', call. = FALSE)
                         }
                       if(!is.null(m) && !is.null(M)) r = max(m, M) - min(m, M)
                       x[1:n.x] <- (x - min(m, M))/r
                       }
                , stdev = {
                   if (...length() == 0L) {
                     mean = eval(mean)
                       sd = eval(sd)
                  stopifnot('zero standard deviation found!' = sd(x) > 0)
                  } else {
                     mean = ..1
                       sd = ..2
                    if (any(!is.numeric(mean), !is.numeric(sd))) stop('quantities must be numeric!', call. = FALSE)
                       }
                     x[1:n.x] <- (x - mean)/sd
                   }
                , relu = {
               filter = match.arg(filter, choices = c('positive', 'negative'))
                 if (is.null(filter)) stop('argument \"filter\" should be either \"positive\" or \"negative\"!', call. = FALSE)
                    x <- if (filter %in% 'positive') pmax.int(0, x, na.rm = TRUE) else pmin.int(0, x, na.rm = TRUE)
                }
                , character(0))
              return(x)
            }
