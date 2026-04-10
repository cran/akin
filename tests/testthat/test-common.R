test_that("common strings are found", {

                          chain1 = 'alpdxoipoyloiekladxoipoylyl'
                         chain2  = 'kdxoipoylyydxoipoylopldxoipoylac'

                               a = common(chain1, chain2, 3)

                              expect_length(a, 8)
  })
