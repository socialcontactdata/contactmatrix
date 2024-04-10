test_that("new_contactmatrix() generates correct class and format", {
  cm2d <- new_contactmatrix(
    from  = c("00_05", "05_10", "05_10"),
    to    = c("00_05", "10_15", "15_20"),
    value = c(0.32   , 0.46   , 0.72   )
  )

  expect_s3_class(cm2d, "contactmatrix")
  # matrix class is conserved for 2d arrays
  expect_s3_class(cm2d, "matrix")
  expect_s3_class(cm2d, "array")
  expect_identical(unname(dim(cm2d)), c(4L, 4L))

  # Multiple groupings; gender & case
  cm3d <- new_contactmatrix(
    from  = list(
      age = c("young", "young", "old"),
      gender = c("male", "female", "female")
    ),
    to    = list(
      age    = c("old", "old", "young"),
      gender = c("female", "female", "female")
    ),
    value = c(1, 2, 2)
  )

  expect_s3_class(cm3d, "contactmatrix")
  expect_s3_class(cm3d, "array")
  expect_identical(unname(dim(cm3d)), c(2L, 2L, 2L, 2L))
  expect_match(names(dimnames(cm3d)), "(from|to)$")

})
