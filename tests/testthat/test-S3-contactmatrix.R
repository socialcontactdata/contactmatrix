cm2d <- new_contactmatrix(
  from  = c("[0,5)", "[5,10)", "[5,10)"),
  to    = c("[0,5)", "[10,15)", "[15,20)"),
  value = c(0.32, 0.46, 0.72),
  symmetric = TRUE
)

test_that("tidy.contactmatrix()", {

  res <- expect_no_condition(tidy(cm2d))

  expect_equal(
    dim(res),
    c(length(cm_get_groupings(cm2d))^2, 3L),
    ignore_attr = TRUE
  )

  expect_identical(
    res$contact,
    c(cm2d)
  )

})
