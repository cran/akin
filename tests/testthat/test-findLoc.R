test_that("returns named list", {

                chain = list(chain1 = 'alpdxoipoyloiekladxoipoylyl',
                            chain2  = 'kdxoipoylyydxoipoylopldxoipoylac')
                           subchain = 'DXOIPOYL'
                   ll = findLoc(subchain, chain, outlist = TRUE, named = TRUE, all. = TRUE)
                   expect_identical(is.list(ll), TRUE)
                   expect_named(ll, c('chain1', 'chain2'))

})
