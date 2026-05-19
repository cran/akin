#'@title Plot For Function "cover"
#'@keywords internal
#'@noRd
#'

plotish = substitute(expr = {
                   olp = par(no.readonly = TRUE)
                   par(list(mar = c(4,4,3,1), tcl = -0.1, ylog = TRUE))
                   plot(xa, xx, xaxt = 'n', yaxt = 'n', type = 'b', pch = 20, xlab = 'Characters', ylab = ''
                        , main = 'Covering Combinations', cex.main = 0.8)
                   axis(1, cex.axis = 0.7, xaxs = 'i', las = 1, at = seq(min(xa), max(xa), by = 4)
                        , labels = format(seq(min(xa), max(xa), by = 4)), cex.lab = 0.8)
                   axis(2, at = seq(min(xx), max(xx), by = 5e+5), , cex.axis = 0.7, yaxs = 'i', las = 1
                        , ylim = c(min(xx), max(xx)*1.05), labels = format(seq(min(xx), max(xx), by = 5e+5)
                        , scientific = TRUE, digits = 2))
                   par(olp)}
                  )
