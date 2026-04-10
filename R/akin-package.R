#'
#' @name akin-package
#' @title Functional Utilities For Data Processing
#'
#' @description
#' The intent of this package is utilitarian which explains the relatively large number of informative messages
#' displayed by most functions. Designed for large and very large data files, the package employs data.table,
#' list environments, sparse matrices and background processing in few places. Nevertheless, Users should
#' consider thread optimization, memoisation or parallel processing, as these techniques were outside the scope.
#'
#' It covers several areas of data processing: subset splitting, reading and writing of large data files, data tiling
#' (horizontal and vertical splitting) - suited for data conversion operations with local as well as global hold such
#' as one-hot encoding - stratified, proportional, random or probabilistic data sampling, data normalization and
#' thresholding, substring location inside strings e.g. peptides inside protein chains, identification of substrings
#' that are common in two strings and location-tabulation of amino acids, modifications or their associated
#' monoisotopic masses inside modified peptides for which, various representations of protein mass spectrometry data
#' were considered with no pretense of exhaustiveness.
#'
#' Comments and suggestions addressing tricky situations are provided however brief. Examples should be run individually
#' in R console.
#'
#' @docType package
#' @keywords internal
#'

 "_PACKAGE"
 NULL
