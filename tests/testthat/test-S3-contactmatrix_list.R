cm1 <- new_contactmatrix(
  from  = c("[0,5)", "[5,10)", "[5,10)"),
  to    = c("[0,5)", "[10,15)", "[15,20)"),
  value = c(0.32, 0.46, 0.72)
)

cm2 <- new_contactmatrix(
  from  = c("[5,10)", "[0,5)", "[5,10)"),
  to    = c("[15,20)", "[10,15)", "[10,15)"),
  value = c(0.27, 0.09, 0.32)
)

cml <- new_contactmatrix_list(cm1, cm2)

test_that("tidy.contactmatrix_list()", {

  res <- expect_no_condition(tidy(cml))

  expect_equal(
    dim(res),
    c(length(cm_get_groupings(cml))^2 * length(cml), 4),
    ignore_attr = TRUE
  )

  expect_identical(
    res$contact,
    unlist(lapply(cml, c))
  )

})
