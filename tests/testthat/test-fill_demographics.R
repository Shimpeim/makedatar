# Tests for md_fill_demographics()

# --- Fixture: a long-format clinical data.frame ----------------------------
# Two patients (P01, P02), three visits each.
# birth_date, gender, height, weight are only filled on the first visit row.
# visit_date is NA on the registration row; ic_date is always filled.

.fixture_long_data <- function() {
  data.frame(
    study_id   = c("P01","P01","P01", "P02","P02","P02"),
    visit_name = c("Reg","W4","W12",  "Reg","W4","W12"),
    visit_date = c(NA,   "2023-02-01","2023-03-01",
                   NA,   "2023-04-01","2023-05-01"),
    ic_date    = c("2023-01-01","2023-01-01","2023-01-01",
                   "2023-03-01","2023-03-01","2023-03-01"),
    birth_date = c("1980-01-01", NA, NA,
                   "1975-06-15", NA, NA),
    gender     = c("M", NA, NA,
                   "F", NA, NA),
    height     = c(170, NA, NA,
                   158, NA, NA),
    weight     = c(70,  NA, NA,
                   55,  NA, NA),
    fvc        = c(80, 78, 75,   65, 67, 63),
    stringsAsFactors = FALSE
  )
}


# --- Broadcasting (vars) tests -----------------------------------------------

test_that("broadcast fills NA visit rows from the non-NA row of same patient", {
  df  <- .fixture_long_data()
  out <- md_fill_demographics(df, vars = c("birth_date","gender","height","weight"),
                               fill_from = NULL)
  # All P01 rows should now have P01's birth_date
  expect_true(all(out$birth_date[out$study_id == "P01"] == "1980-01-01"))
  expect_true(all(out$birth_date[out$study_id == "P02"] == "1975-06-15"))
})

test_that("broadcast fills gender and height correctly", {
  df  <- .fixture_long_data()
  out <- md_fill_demographics(df, vars = c("gender","height"), fill_from = NULL)
  expect_equal(out$gender[out$study_id == "P01"], c("M","M","M"))
  expect_equal(out$height[out$study_id == "P02"], c(158, 158, 158))
})

test_that("broadcast does not change fvc (not in vars)", {
  df  <- .fixture_long_data()
  out <- md_fill_demographics(df, vars = c("birth_date"), fill_from = NULL)
  expect_equal(out$fvc, df$fvc)
})

test_that("broadcast: entirely-NA variable produces all-NA column", {
  df  <- .fixture_long_data()
  df[["gender"]] <- NA_character_
  out <- md_fill_demographics(df, vars = "gender", fill_from = NULL)
  expect_true(all(is.na(out$gender)))
})

test_that("broadcast: absent variable is silently skipped", {
  df  <- .fixture_long_data()
  expect_no_error(
    md_fill_demographics(df, vars = c("birth_date", "nonexistent_col"),
                          fill_from = NULL)
  )
})

test_that("broadcast: multiple non-NA rows per patient — first non-NA wins", {
  df  <- .fixture_long_data()
  # Give P01 a second non-NA birth_date at W4
  df$birth_date[df$study_id == "P01" & df$visit_name == "W4"] <- "1999-01-01"
  out <- md_fill_demographics(df, vars = "birth_date", fill_from = NULL)
  # First non-NA for P01 is "1980-01-01" (the Reg row)
  expect_true(all(out$birth_date[out$study_id == "P01"] == "1980-01-01"))
})

test_that("broadcast on a per-visit column propagates first value to all rows", {
  # fvc is a per-visit column, not a constant — broadcasting intentionally
  # overwrites later visits with the first (Reg) value.  This test documents
  # the expected behaviour: callers should only pass constant-per-patient
  # columns (birth_date, gender, etc.) to vars.
  df      <- .fixture_long_data()
  df$fvc  <- as.numeric(df$fvc)
  out <- md_fill_demographics(df, vars = "fvc", fill_from = NULL)
  expect_true(all(out$fvc[out$study_id == "P01"] == df$fvc[df$study_id == "P01"][1]))
  expect_true(all(out$fvc[out$study_id == "P02"] == df$fvc[df$study_id == "P02"][1]))
})

test_that("broadcast: data.frame dimensions are unchanged", {
  df  <- .fixture_long_data()
  out <- md_fill_demographics(df, fill_from = NULL)
  expect_equal(dim(out), dim(df))
})

test_that("broadcast: column order is preserved", {
  df  <- .fixture_long_data()
  out <- md_fill_demographics(df, fill_from = NULL)
  expect_equal(colnames(out), colnames(df))
})


# --- fill_from tests ---------------------------------------------------------

test_that("fill_from fills NA visit_date from ic_date", {
  df  <- .fixture_long_data()
  out <- md_fill_demographics(df, vars = character(0),
                               fill_from = c(visit_date = "ic_date"))
  # Reg rows had NA visit_date; should now equal ic_date
  reg_rows <- out$visit_name == "Reg"
  expect_true(all(!is.na(out$visit_date[reg_rows])))
  expect_equal(out$visit_date[reg_rows], out$ic_date[reg_rows])
})

test_that("fill_from does not overwrite already-filled values", {
  df  <- .fixture_long_data()
  out <- md_fill_demographics(df, vars = character(0),
                               fill_from = c(visit_date = "ic_date"))
  # Non-Reg visit rows already had visit_date; should be unchanged
  non_reg <- out$visit_name != "Reg"
  expect_equal(out$visit_date[non_reg], df$visit_date[non_reg])
})

test_that("fill_from = NULL disables date fill", {
  df  <- .fixture_long_data()
  out <- md_fill_demographics(df, vars = character(0), fill_from = NULL)
  # visit_date NAs should remain
  expect_true(any(is.na(out$visit_date)))
})

test_that("fill_from: absent target column is silently skipped", {
  df  <- .fixture_long_data()
  expect_no_error(
    md_fill_demographics(df, vars = character(0),
                          fill_from = c(no_such_col = "ic_date"))
  )
})

test_that("fill_from: absent source column is silently skipped", {
  df  <- .fixture_long_data()
  expect_no_error(
    md_fill_demographics(df, vars = character(0),
                          fill_from = c(visit_date = "no_such_source"))
  )
  # visit_date NAs should remain unchanged
  out <- md_fill_demographics(df, vars = character(0),
                               fill_from = c(visit_date = "no_such_source"))
  expect_equal(out$visit_date, df$visit_date)
})

test_that("fill_from: multiple pairs all applied", {
  df  <- .fixture_long_data()
  df[["extra_target"]] <- NA_character_
  df[["extra_source"]] <- "filled"
  out <- md_fill_demographics(df, vars = character(0),
                               fill_from = c(visit_date  = "ic_date",
                                             extra_target = "extra_source"))
  expect_true(all(!is.na(out$visit_date[out$visit_name == "Reg"])))
  expect_true(all(out$extra_target == "filled"))
})


# --- id_var error ------------------------------------------------------------

test_that("missing id_var raises an informative error", {
  df <- .fixture_long_data()
  expect_error(
    md_fill_demographics(df, id_var = "nonexistent_id"),
    "id_var"
  )
})


# --- Combined default call ---------------------------------------------------

test_that("default call fills all four vars and visit_date", {
  df  <- .fixture_long_data()
  out <- md_fill_demographics(df)   # all defaults
  # Demographics filled
  expect_true(all(!is.na(out$birth_date)))
  expect_true(all(!is.na(out$gender)))
  # visit_date Reg rows filled from ic_date
  expect_true(all(!is.na(out$visit_date[out$visit_name == "Reg"])))
})
