#' @title Locate And Extract Modifications Or Monoisotopic Masses From A Modified Peptide
#'
#' @description Finds and tabulates amino acid sites and extracts respective modifications or monoisotopic masses
#' from a modified peptide.
#'
#' @param string character, length 1. Modified or unmodified peptide, or NULL
#' @param wrap character, length 1. The closing (right-hand) side of any of the bracket types ']', ')', '\}' that wrap
#'   the modifications, such as in protein mass spectrometry data representation of modified peptides. Default, ']'
#' @param inbracket character, length 1. Same as above for brackets used inside modification wrappings. Default, ')'
#' @param except character, length >= 1. Default, NULL. Punctuation marks or characters that appear along modifications
#'   and are needed to remain present in the output: '-', '+', ',', ';', ':', '=', '.', `[:digit:]`, `[:alpha:]`, '\\w+', ' '
#' @param rmve character, length 1. Default, NULL. Regular expression. Digits or extra characters that need to be removed from
#'   the output (see Examples)
#'
#' @details Although capable of handling most situations, it is recommended that the \code{wrap}ping bracket type
#' remains consistent throughout and the \code{inbracket} type \emph{be different} from \code{wrap}ping type.
#' No extra characters are removed from result when \code{except = rmve = NULL}.
#'
#' This utility covers most data representation styles for modified peptide. However, clean data results are not guaranteed.
#' The template for letter casing accepted for modified peptide and for modifications should match those presented in Examples:
#' upper case for peptide and mixed case for modifications.
#'
#' @return A 'data.table' class data frame containing the unmodified peptide, the modified peptide, the modification site
#' (i.e. the amino acid code letter and location inside the peptide) and the associated modification(s). In case of monoisotopic
#' mass extraction, monoisotopic mass values populate column "Modification" as "character" types. Multiple modifications (identical
#' or not) found at the same site are listed as many times as they appear at that site. Unmodified, endogenous peptides are listed
#' with no other information. Empty strings are listed as such with a warning.
#'
#' @seealso \link[base]{regex}
#'
#' @keywords Proteomics
#'
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'
#' # Completely made-up modified peptides:
#'
#' # 1. Modifications
#'
#' # 1.1 Default brackets
#' string = 'K[Prop_A][Met][Prop (C)]PSSABCELR[Prop][Prop][Prop]FQC[Carba (C)]GQQ[Met +44]TARP'
#'
#' a = locateMod(string)
#' print(a)                                                              # with extra-characters
#' b = locateMod(string, except = '\\w+', rmve = '(\\(.*\\)|_[A-Z]|[0-9])')
#' print(b)                                                              # without extra-characters
#'
#' # In this example argument "rmve" contains the default in-brackets
#'
#' # 1.2 Alternative bracketing
#'
#' string = 'K{Prop_A}{Met}{Prop [A]}PSSABCELR{Prop +15}{Prop}{Prop}FQC{Carba [C]}GQQ{Met +44}TARP'
#'
#' c = locateMod(string, '}', ']')
#' print(c)
#' d = locateMod(string, '}', ']', except = '\\w+', rmve = '(\\[.*\\]|_[A-Z]|[0-9])')
#' print(d)
#'
#' # In this example argument "rmve" contains the alternative in-brackets
#'
#' # 2. Empty string
#'
#' empty = locateMod(""); print(empty)
#'
#' # 3. Monoisotopic masses
#'
#' string = 'TAAC[+57.021464]PPC[+57.021464]PAPPAPS[+162.052824]VFLTLMISR'
#' e = locateMod(string)
#' print(e)                                                             # with extra-characters
#' f = locateMod(string, rmve = '[[:punct:]]')$Modification
#' print(f)                                                             # incorrect values
#' g = locateMod(string, rmve = '\\+')$Modification
#' print(g)                                                             # correct!
#' class(g)                                                             # character
#' }
#'

locateMod = function(string, wrap = "]", inbracket = ')', except = NULL, rmve = NULL) {
                     string = if (isTRUE(nzchar(string) && !identical(' ', string))) as.character(string)
                              else warning('empty string!', call. = FALSE, noBreaks. = TRUE)
                       wrap = match.arg(wrap, choices = c("]", ")", "}"))
                   brackets = as.character(c('}','{',']','[',')','('))
                      awrap = brackets[which(brackets %in% wrap) + 1L]
                   abracket = brackets[which(brackets %in% inbracket) + 1L]
                    if (!exists('plac', mode = 'character', inherits = FALSE)) plac = eval(.plac)
                         br = regmatches(string, gregexpr(plac, string))[[1L]]
               if (length(br) == 0L) {
                         dt = data.table(
                                             'Peptide' = string
                                           , 'Modified Sequence' = string
                                           , 'AA' = NA_character_
                                           , 'Location' = NA_integer_
                                           , 'Modification' = NA_character_
                                        )
                  } else {
                         uu = eval(.uu); u = eval(.u); uv = eval(.uv)
                      modif = regmatches(string, gregexpr(u, string, perl = TRUE))[[1L]]
                      modif = gsub(uv, '', modif)
                      delayedAssign('pep', paste0(
                                          regmatches(string, gregexpr(u, string, perl = TRUE), invert = TRUE)[[1L]]
                                        , collapse = ''))
                      if (!is.null(except)) {
                      if (!exists('ARGS', mode = 'character', inherits = FALSE)) ARGS = eval(.ARGS)
                                 except = match.arg(except, arg = ARGS, several.ok = TRUE)
                                      w = eval(.w)
                                  modif = gsub(w, '', modif)}
                      if (!is.null(rmve)) modif = gsub(rmve, '', modif)
                      delayedAssign('n.mod', length(modif))
                         sp = strsplit(string, split = '', FALSE, FALSE, FALSE)[[1L]]
                         wp = grep(wrap, brackets)
                     startx = paste0('(', '\\', brackets[wp + 1L], ')', '+', collapse = '')
                       endx = paste0('(', '\\', brackets[wp], ')', '+', collapse = '')
                      start = grep(startx, sp) - 1L
                        end = grep(endx, sp); l.en = length(end)
             if (any(diff(end) == 1L)) {
                               diffs = c(FALSE, diff(end) == 1L)
                              wnodif = which(!diffs)
                               start = start[wnodif]; end = end[wnodif]}
                        end = c(0L, end[-l.en])
                      AAloc = cumsum(start - end)
                    AAmodif = sp[start]
                       site = which(AAmodif %in% wrap); l.s = length(site)
          while (l.s) {
             AAmodif[site] <- AAmodif[site - 1L]
                       l.s <- l.s - 1L}
                         dt = try(data.table(
                                             'Peptide' = rep(pep, n.mod)
                                           , 'Modified Sequence' = rep(string, n.mod)
                                           , 'AA' = AAmodif
                                           , 'Location' = AAloc
                                           , 'Modification' = modif
                                        ))
            }
             return(dt)
       }


