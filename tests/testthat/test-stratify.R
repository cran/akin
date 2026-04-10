test_that("data sampling pays", {
                                  data(mtcars)
                             ID = stratify(mtcars
                                         , 'cyl'
                                         , c('vs', 'am')
                                         , seed = 314)
                             rowID = stratify(mtcars
                                            , 'cyl'
                                            , c('vs', 'am')
                                            , seed = 314
                                            , indx = FALSE
                                            , ext = TRUE)$row
                             expect_equal(ID, rowID)
})
