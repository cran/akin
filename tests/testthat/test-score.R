test_that("normalization-threshlding", {
                                  x = { set.seed(223); sort(runif(10, -3, 3)) }
                                  y = score(x, relu, 'positive')
                             expect_identical(isTRUE(all(y >= 0)), TRUE)
                                  z = 1:9
                                 zz = z/max(z)
                                  A = score(z, range, NULL, 0, 9)
                             expect_equal(A, zz)
})
