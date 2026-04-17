
#' @import data.table
#' @import RVerbalExpressions
#' @import RcppAlgos
#' @import methods
#' @importFrom utils type.convert getFromNamespace
#' @importFrom fastmatch fmatch
#' @importFrom stats median setNames
#' @importFrom callr r_bg
#' @importFrom Matrix Matrix rowSums rsparsematrix
#' @importFrom listenv listenv
#'

    .onLoad = function(libname, pkgname) {
                         Mmatrix = getFromNamespace('Mmatrix', 'Matrix')
                   rsparsematrix = getFromNamespace('rsparsematrix', 'Matrix')
                }
