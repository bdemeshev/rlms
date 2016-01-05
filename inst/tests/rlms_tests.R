test_that("extract wave/level/sample from filename", {
  expect_equal(rlms_fileinfo("r06hall23.sav")$wave, 6)
  expect_equal(as.character(rlms_fileinfo("r06hall23.sav")$level), "household")
  expect_equal(as.character(rlms_fileinfo("r06hall23.sav")$sample), "all")
})

