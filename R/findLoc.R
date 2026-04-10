#' @title Find Substring Locations Inside A String
#'
#' @description Finds all locations of a known character substring inside a character string.
#'
#' @param subchain character, length 1, e.g. a peptide sequence
#' @param chain (named) character, length 1 or a (named) list of such characters such as a
#'   list of protein chains obtained from a \emph{fasta} file
#' @param outlist logical. Default, FALSE, the output is a (named) integer vector of locations. Otherwise,
#'   it is a (named) list of location vectors, each corresponding to a chain in a list of chains
#' @param named logical. Default, FALSE. Output is not named. Otherwise, the output is named
#' @param all. logical, default FALSE, returns the leftmost or the rightmost location inside the chain.
#'   When TRUE, returns all locations inside the chain for each chain in a list
#' @param which symbol. Location to report. Default, \strong{min}. Requires \code{all. = FALSE}. \code{which = min}
#'   returns the leftmost location inside the chain, e.g. closest to the N-terminus of a protein chain. \code{which = max}
#'   returns the rightmost location, e.g. closest to the C-terminus of a protein chain. Overwritten when \code{all. = TRUE}
#' @param ignore.case,perl,fixed,useBytes arguments to [base::gregexpr]
#'
#' @details Wrapper to [base::gregexpr], the function scans all chains in a list of chains to find subchain locations.
#' The location is defined as the position inside the chain relative to the left end of the chain of the first subchain character.
#'
#' @returns A (named) integer or a (named) list of integer vectors of \code{subchain} locations inside the \code{chain}.
#'
#' @seealso \link[base]{gregexpr}
#'
#' @keywords Proteomics
#'
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'
#' # 1. List of chains
#'
#' chain = list(chain1 = 'alpdxoipoyloiekladxoipoylyl',
#'              chain2  = 'kdxoipoylyydxoipoylopldxoipoylac')
#' subchain = 'DXOIPOYL'                                                  # ignoring the case
#'
#' findLoc(subchain, chain, outlist = TRUE, named = TRUE, all. = TRUE)    # named list
#' findLoc(subchain, chain, named = TRUE, all. = TRUE)                    # named integer
#' findLoc(subchain, chain, outlist = TRUE, named = TRUE)                 # the leftmost positions
#' findLoc(subchain, chain, which = max, named = TRUE)                    # the rightmost positions
#'
#' # 2. Single chain
#'
#' chain = chain[[1]]
#'
#' findLoc(subchain, chain, all. = TRUE)
#' findLoc(subchain, chain, which = max)
#' findLoc(subchain, chain)                                               # default location
#' findLoc(subchain, chain, which = max, ignore.case = FALSE)             # not ignoring the case
#'
#' }
#'

 findLoc = function(subchain, chain, outlist = FALSE, named = FALSE, all. = FALSE
                  , which = min, ignore.case = TRUE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {
                       nc = if (isFALSE(named)) NULL else names(chain)
                    chain = if (isTRUE(all(nzchar(chain)))) as.character(chain)
                 subchain = if (isTRUE(nzchar(subchain))) as.character(subchain)
                 if (isTRUE(nchar(chain) < nchar(subchain))) stop('the chain is too short!', call. = FALSE)
                 else{
                 which = substitute(which)
                 which = match.fun(match.arg(as.character(which), choices = c('min', 'max')), descend = FALSE)
                     w = listenv(); v = listenv()
                     v = gregexpr(subchain, chain, ignore.case, perl, fixed, useBytes)
              names(v) = nc
                     w = lapply(v, \(i) `[`(i, i > 0L)); v <- NULL
                     w[lengths(w) == 0L] <- NULL
                 if(isFALSE(all.)) {
                   if(isFALSE(outlist)) vapply(w, \(i) which(i), integer(1L)) else lapply(w, \(i) which(i))
                 } else {
                    if(isFALSE(outlist)) unlist(w, TRUE, TRUE) else w}}
        }
