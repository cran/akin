test_that("matrix output", {
                  data(iris)
                     tempf = tempfile(fileext = '.csv')
                     write.table(iris,tempf , sep = ',', row.names = FALSE, quote = FALSE)
                  skip_on_os('mac')
                     C = tileHot(tempf, rows = 14, splits = 3, orn = TRUE, print = FALSE)
                     c = getEV(C, 'Species', 'matrix')
                  expect_identical(class(C)[1L], "dgCMatrix")
                  expect_identical(class(c), c("matrix", "array"))
                  expect_identical(colnames(c), c("setosa", "versicolor", "virginica"))
                  unlink(tempf)

})
