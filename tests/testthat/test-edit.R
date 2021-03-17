context("Editing fitness files.")

test_that("fix_hr", {
  hr_base <- 130
  max_seconds <- 28*60

  test_data <- readLines("tests/test.gpx")
  
  # Test that hr_ceiling makes second observation not affected
  d <- fix_hr(hr_base = 130, hr_ceiling = 150, max_minutes = 10, 
              infile = "tests/test.gpx",
              return_output = TRUE)
  expect_equal(trimws(d[grepl("gpxtpx:hr", d)][[2]]), "<gpxtpx:hr>140</gpxtpx:hr>")

  # Test that time limit omits the third observation
  d <- fix_hr(hr_base = 130, hr_ceiling = 150, max_minutes = 10, 
         infile = "tests/test.gpx",
         return_output = TRUE)
  expect_equal(trimws(d[grepl("gpxtpx:hr", d)][[3]]), "<gpxtpx:hr>190</gpxtpx:hr>")

  # Test that null time limit fixes the whole file
  d <- fix_hr(hr_base = 130, hr_ceiling = 150, 
              infile = "tests/test.gpx",
              return_output = TRUE)
  expect_equal(trimws(d[grepl("gpxtpx:hr", d)][[3]]), "<gpxtpx:hr>130</gpxtpx:hr>")
  
})
