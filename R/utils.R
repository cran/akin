#' @title Condition For common Function
#'
#' @keywords internal
#' @noRd
#'

cond = substitute(expr = isTRUE(grep(y, chain, useBytes = TRUE, ...) > 0L))

#' @title Object For oneHot Function, decode
#' @description Logical expression
#'
#' @keywords internal
#' @noRd
#'

 tof = substitute(expr = isTRUE(
         any(sapply(n.x, \(n) is.double(type.convert(n, as.is = TRUE, numerals = 'no.loss')))))
         )


#'@title The j-side Of \"data.table\" Sample Calculation
#'
#'@keywords internal
#'@noRd
#'

 CALLs = as.call(
   c(quote(list), id = substitute(sample(row, if(length(size) > 0L) size = unique(size) else size = size, replace = replace, prob = if (!is.null(eval(dis))) abs(do.call(dis, args)))
                , env = list(size = quote(size), replace = quote(replace), args = quote(args), dis = quote(dis))))
   )

#'@title Return Value For Sample Calculation
#'
#'@description The \"return()\" Code Part Of \code{stratify} Function
#'@noRd
#'

retz = substitute(
         expr = if(isTRUE(indx)) id else if(isTRUE(ext)) data[id][] else {
                   if (missing(size)) data[id][, let(c('size', 'row', 'strat', 'n'), NULL)][]
                   else data[id][, let(c('row', 'strat', 'n'), NULL)][]
                })


#'@title Switch Formulas For Sample Size Calculation
#'
#'@keywords internal
#'@noRd
#'

s = function(n, m, med, Mx, M, thresh) {
            if(is.numeric(thresh) && is.double(thresh)) ceiling(thresh) else thresh
            if(thresh < M) {
               if(thresh <= m) {ceiling(sqrt(n)*log(n))
               } else if (m < thresh & thresh <= med) {ceiling(sqrt(n)*log(n, base = 4))
               } else ceiling(n*(med/Mx))
            } else ceiling(n*(m/Mx))
}

#'n@title Check If Calculated Sample Size Acommodates The Thinnest Stratum
#'
#'@keywords internal
#'@noRd
#'

checkCondition = function(m, med, M, dtt) {
                        n = strat = Mx = NULL
                        on.exit(return(dtt), add = TRUE)
                        for (t in c(m, med, M)) {
                                dtt = dtt[, list(size = fifelse(n == 1L, 0L, s(n, m, med, Mx, M, thresh = t)), strat = strat, n = n)]
                                 ss = unique(dtt$size)
                             if (any(0L < ss && ss <= m)) {
                              message('\nfound valid formula for ', t, ' rows\n')
                              break
                          } else message('\nSwitching again ...\n')}
                     }

#'@title Internal Function For \code{splitH}
#'
#'@keywords internal
#'@noRd
#'

 rename = function(nmes) call('lapply', X = setNames(nm = nmes), FUN = as.name)

#'@title Other Objects for splitH
#'@noRd
#'

  srn = substitute(expr = if (is.infinite(rows)) NULL else if (orn && is.finite(rows)) .I + i*rows)
 jump = substitute(expr = if (is.finite(rows)) i*rows + 1L else 0L)


#' @title Conversion Utility
#'
#' @description
#' Internal function used in decoding a matrix of encoded data
#'
#' @param x,y sparse or dense vectors, the result of encoding
#'
#' @returns decoded vector
#'
#' @keywords internal
#' @noRd
#'
#'

  g = function(x, y) {
                       z = fifelse(x == 1L, y, NA)
   return(type.convert(z[!is.na(z)], as.is = FALSE, numerals = 'no.loss', tryLogical = FALSE))
          }


  #' @title Display Brief Information On A Data File
  #'
  #' @param readpath character, length 1. Full path to data file
  #' @param show.info logical. Default TRUE: shows information on data file
  #'
  #' @references The code was inspired by
  #' \href{https://github.com/Rdatatable/data.table/issues/7169}{Issue# 7169}
  #'
  #' @details Provides data [base::dim] and [base::names] without loading the
  #' entire file into memory.
  #'
  #' @returns Prints info and invisibly returns the number of rows, and data file
  #' column names for internal use.
  #'@keywords internal
  #'
  #' @noRd
  #'


  info = function(readpath, show.info = TRUE) {
         tot = as.integer(strsplit(system2("wc", c("-l", readpath), stdout = TRUE, wait = FALSE), " ", fixed = TRUE)[[1L]][1L]) - 1L
        nmes = strsplit(system2("head", c("-n1", readpath), stdout=TRUE, wait = FALSE), ",", fixed=TRUE)[[1L]]
      nonmes = which(nmes %in% ""); l.no = length(nonmes)
     dupnmes = which(duplicated(nmes)); l.dup = length(dupnmes)
        if (show.info) {
          if (isTRUE(any(l.no || l.dup ))) {
          stop('Source contains ', l.no,'\\', l.dup, ' empty\\duplicated column names!!\n', call. = FALSE)}
      message("\nFound file containing: ", tot, " rows and ", length(nmes), " columns named\n\n", sQuote(nmes), "\n\n")
    }
    return(invisible(list(tot, nmes)))
  }

#'@title Get Unexported "Mmatrix" From Package "Matrix"
#'
#'@keywords internal
#'@noRd
#'

M = substitute(expr = {
         Mmatrix = utils::getFromNamespace('Mmatrix', 'Matrix')
        as(.External(Mmatrix
                   , data = 0L
                   , nrow = n.x
                   , ncol = n.l
                   , byrow = FALSE
                   , dimnames = list(rn.x, ll)
                   , mnrow = FALSE
                   , mncol = FALSE), omc)
})

#'
#'@title Row-bind Sparse Matrices
#'
#'@keywords internal
#'
#'@description [base::rbind]s a list of [Matrix::Matrix] or [base::matrix]
#' objects, filling in missing columns.
#'
#' NOTE 1: This code was taken as is from package 'Matrix.utils' v 0.9.8 published
#' under GPL-3 license and removed from CRAN. With thanks to the package Author!
#' NOTE 2: In this copy, [base::match] function was replaced with [fastmatch::fmatch]
#'
#' Extract from function's documentation:
#'
#'@param x, ...	objects to combine. If the first argument is a list and ... is
#'   unpopulated, the objects in that list will be combined.
#'@param fill value with which to fill unmatched columns
#'@param out.class the class of the output object. Defaults to the class of x.
#'   Note that some output classes are not possible due to R coercion
#'   capabilities, such as converting a character matrix to a Matrix.
#'@noRd
#'

rBind.fill = function (x, ..., fill = NULL, out.class = class(rbind(x, x))[1]) {
      if (is.list(x) && !is.data.frame(x) && missing(...)) {
      Reduce(function(x, y) rBind.fill.internal(x, y, fill, out.class), x)
                     }
         else {
              Reduce(function(x, y) rBind.fill.internal(x, y, fill,  out.class), list(x, ...))
      }
}


rBind.fill.internal<-function(x,y,fill,out.class)
{
  out.class<-force(out.class)
  fillMissing<-is.null(fill)
  if(fillMissing)
    fill<-if(is(x,'Matrix')) 0 else NA
  if (is.null(nrow(x)))
    x<-matrix(x,nrow=1,dimnames=list(NULL,names(x)))
  if (is.null(nrow(y)))
    y<-matrix(y,nrow=1,dimnames=list(NULL,names(y)))

  nullNames<-FALSE
  if(is.null(colnames(x)))
    colnames(x)<-make.names(colnames(y)[1:ncol(x)],unique = TRUE)
  if(is.null(colnames(y)))
    colnames(y)<-make.names(colnames(x)[1:ncol(y)],unique = TRUE)
  if(is.null(colnames(x)))
  {
    nullNames<-TRUE
    colnames(x)<-1:ncol(x)
    colnames(y)<-1:ncol(y)
  }
  ymiss<-colnames(x)[which(is.na(fmatch(colnames(x),colnames(y))))]
  ybind<-rsparsematrix(nrow=nrow(y),ncol=length(ymiss),0)
  colnames(ybind)<-ymiss
  if(!fillMissing)
    ybind[seq_along(ybind)]<-fill
  xmiss<-colnames(y)[which(is.na(fmatch(colnames(y),colnames(x))))]
  xbind<-rsparsematrix(nrow=nrow(x),ncol=length(xmiss),0)
  colnames(xbind)<-xmiss
  if(!fillMissing)
    xbind[seq_along(xbind)]<-fill
  if (ncol(xbind>0))
    x<-cbind2(x,xbind)
  if(ncol(ybind)>0)
    y<-cbind2(y,ybind)
  y<-as(y,out.class)
  x<-as(x,out.class)
  result<-rbind2(x,y[,order(fmatch(colnames(y),colnames(x)))])
  if(nullNames)
    colnames(result)<-NULL
  return(result)
}

#' @title Regular Expressions And Objects For \"locateMod\" function
#' @keywords internal
#' @noRd
#'

.ARGS = quote(c('-', '+', ',', ';', ':', '=', '.', '[:digit:]', '[:alpha:]', '\\w+', ' '))
.plac = quote(rx() %>% rx_either_of(brackets) %>% rx_one_or_more())
  .uu = quote(paste0(wrap, brackets[which(brackets %in% awrap)], collapse = ''))
  .uv = quote(rx() %>% rx_either_of(awrap, wrap))
   .u = quote(rx() %>% rx_any_of(paste0(awrap, abracket, collapse = '')) %>% rx_anything_but(uu) %>% rx_any_of(paste0(inbracket, wrap, collapse = '')) %>% rx_one_or_more())
   .w = quote(rx() %>% rx_either_of(setdiff(ARGS, except)))
