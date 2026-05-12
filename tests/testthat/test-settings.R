# Tests for md_settings() and md_settings_validate()


# --- md_settings: construction -----------------------------------------------

test_that("md_settings returns a list of class 'md_settings'", {
  s <- md_settings("/tmp/test.xlsx")
  expect_true(inherits(s, "md_settings"))
  expect_true(is.list(s))
})

test_that("md_settings: required keys are present", {
  s <- md_settings("/tmp/test.xlsx")
  required <- c("data_path","colinfo_sheet","data_sheet","data_skip",
                "data_na","colinfo_na","output_dir","right_truncation")
  expect_true(all(required %in% names(s)))
})

test_that("md_settings: data_path is stored correctly", {
  s <- md_settings("/some/path/data.xlsx")
  expect_equal(s$data_path, "/some/path/data.xlsx")
})

test_that("md_settings: defaults are applied", {
  s <- md_settings("/tmp/test.xlsx")
  expect_equal(s$colinfo_sheet,    "colinfo")
  expect_equal(s$data_sheet,       "data")
  expect_equal(s$data_skip,        1L)
  expect_equal(s$data_na,          "*")
  expect_equal(s$colinfo_na,       "NA")
  expect_equal(s$output_dir,       "./output")
  expect_null(s$right_truncation)
})

test_that("md_settings: data_skip is coerced to integer", {
  s <- md_settings("/tmp/test.xlsx", data_skip = 2)
  expect_type(s$data_skip, "integer")
  expect_equal(s$data_skip, 2L)
})

test_that("md_settings: right_truncation is stored as numeric", {
  days <- 365 * 3 + 4 * 7
  s    <- md_settings("/tmp/test.xlsx", right_truncation = days)
  expect_equal(s$right_truncation, days)
})

test_that("md_settings: custom core parameters override defaults", {
  s <- md_settings("/tmp/test.xlsx",
                    colinfo_sheet = "meta",
                    data_sheet    = "raw",
                    data_skip     = 0L,
                    data_na       = "NA",
                    output_dir    = "/results")
  expect_equal(s$colinfo_sheet, "meta")
  expect_equal(s$data_sheet,    "raw")
  expect_equal(s$data_skip,     0L)
  expect_equal(s$data_na,       "NA")
  expect_equal(s$output_dir,    "/results")
})


# --- md_settings: extras via ... ---------------------------------------------

test_that("md_settings: extra named args are stored in the list", {
  s <- md_settings("/tmp/test.xlsx",
                    fn_supplemental = "suppl.xlsx",
                    cohort_label    = "NUCR019002")
  expect_equal(s$fn_supplemental, "suppl.xlsx")
  expect_equal(s$cohort_label,    "NUCR019002")
})

test_that("md_settings: multiple extras are all stored", {
  # Avoid single-letter names that partially match formal parameters
  s <- md_settings("/tmp/test.xlsx", key_a = "1", key_b = "2", key_cc = "3")
  expect_equal(s$key_a,  "1")
  expect_equal(s$key_b,  "2")
  expect_equal(s$key_cc, "3")
})


# --- md_settings: error on missing data_path ---------------------------------

test_that("md_settings: missing data_path raises error", {
  expect_error(md_settings(), "data_path")
})

test_that("md_settings: empty data_path raises error", {
  expect_error(md_settings(""), "data_path")
})

test_that("md_settings: whitespace-only data_path raises error", {
  expect_error(md_settings("   "), "data_path")
})


# --- md_settings_validate ----------------------------------------------------

test_that("md_settings_validate: returns settings invisibly on success", {
  s   <- md_settings("/tmp/test.xlsx")
  out <- md_settings_validate(s, check_files = FALSE)
  expect_identical(out, s)
})

test_that("md_settings_validate: raises error for non-settings input", {
  expect_error(md_settings_validate(list(data_path="/x.xlsx")), "md_settings")
})

test_that("md_settings_validate: raises error when data file absent", {
  s <- md_settings("/tmp/no_such_file_xyzzy.xlsx")
  expect_error(md_settings_validate(s, check_files = TRUE), "not found")
})

test_that("md_settings_validate: passes when check_files=FALSE and file absent", {
  s <- md_settings("/tmp/no_such_file_xyzzy.xlsx")
  expect_no_error(md_settings_validate(s, check_files = FALSE))
})

test_that("md_settings_validate: raises error for invalid data_skip", {
  s              <- md_settings("/tmp/test.xlsx")
  s$data_skip    <- -1L
  expect_error(md_settings_validate(s, check_files = FALSE), "data_skip")
})

test_that("md_settings_validate: raises error for non-positive right_truncation", {
  s                   <- md_settings("/tmp/test.xlsx", right_truncation = -10)
  expect_error(md_settings_validate(s, check_files = FALSE), "right_truncation")
})

test_that("md_settings_validate: NULL right_truncation passes validation", {
  s <- md_settings("/tmp/test.xlsx")
  expect_no_error(md_settings_validate(s, check_files = FALSE))
})


# --- print.md_settings -------------------------------------------------------

test_that("print.md_settings produces output without error", {
  s <- md_settings("/tmp/test.xlsx", right_truncation = 1120,
                    extra_key = "extra_val")
  expect_output(print(s), "makedatar settings")
  expect_output(print(s), "data_path")
  expect_output(print(s), "extra_key")
})
