# Tests for mf.* transform functions (trans_functions.R)
#
# All functions accept `data` (first, named) + positional DSL args + `id_cols`.
# Tests use minimal in-memory data.frames; no global state is accessed.


# --- Shared fixture ----------------------------------------------------------

.make_df <- function() {
  data.frame(
    study_id   = c("P01","P01","P01","P02","P02","P02"),
    visit      = c("Reg","W4","W12","Reg","W4","W12"),
    height     = c(170, 170, 170, 160, 160, 160),
    weight     = c( 70,  72,  74,  55,  56,  57),
    age_days   = c(14600, NA, NA, 18250, NA, NA),
    cig_per_day = c(20, 20, 20, 0, 0, 0),
    smk_years   = c(10, 10, 10, 0, 0, 0),
    smoking_hist = c("1","1","1","2","2","2"),
    fvc        = c(80, 78, 75, 65, 67, 63),
    fvc_change = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
    stringsAsFactors = FALSE
  )
}


# --- mf.bmi ------------------------------------------------------------------

test_that("mf.bmi returns correct BMI", {
  df  <- .make_df()
  res <- mf.bmi(df, "weight", "height")
  expected <- df$weight / (df$height / 100)^2
  expect_equal(res, expected)
})

test_that("mf.bmi result length equals nrow(data)", {
  df <- .make_df()
  expect_length(mf.bmi(df, "weight", "height"), nrow(df))
})

test_that("mf.bmi ignores id_cols argument silently", {
  df <- .make_df()
  expect_no_error(mf.bmi(df, "weight", "height", id_cols = c("study_id","visit")))
})


# --- mf.diff -----------------------------------------------------------------

test_that("mf.diff returns x1 - x2 elementwise when id_cols absent", {
  df  <- .make_df()
  res <- mf.diff(df, "weight", "fvc")
  expect_equal(res, df$weight - df$fvc)
})

test_that("mf.diff with id_cols produces same length as input", {
  df  <- .make_df()
  res <- mf.diff(df, "fvc", "weight", id_cols = c("study_id", "visit"))
  expect_length(res, nrow(df))
})

test_that("mf.diff: rows where x2 is NA produce NA result", {
  df  <- .make_df()
  df$weight[3] <- NA
  res <- mf.diff(df, "fvc", "weight")
  expect_true(is.na(res[3]))
})


# --- mf.days_to_year ---------------------------------------------------------

test_that("mf.days_to_year converts days to integer years (floor)", {
  df  <- .make_df()
  res <- mf.days_to_year(df, "age_days")
  expect_equal(res[1], 40L)   # 14600 / 365 = 40.0
  expect_equal(res[4], 50L)   # 18250 / 365 = 50.0
})

test_that("mf.days_to_year returns integer type", {
  df <- .make_df()
  expect_type(mf.days_to_year(df, "age_days"), "integer")
})

test_that("mf.days_to_year preserves NA", {
  df <- .make_df()
  expect_true(is.na(mf.days_to_year(df, "age_days")[2]))
})


# --- mf.pack_year ------------------------------------------------------------

test_that("mf.pack_year: 20 cig/day * 10 years = 10 pack-years", {
  df  <- .make_df()
  res <- mf.pack_year(df, "cig_per_day", "smk_years")
  expect_equal(res[1], 10)
})

test_that("mf.pack_year: never-smokers (smoking_hist=='2') get 0", {
  df  <- .make_df()
  res <- mf.pack_year(df, "cig_per_day", "smk_years",
                       smk_hist = "smoking_hist", non_smk_val = "2")
  expect_equal(res[4], 0)
})

test_that("mf.pack_year: missing smk_hist column is silently skipped", {
  df <- .make_df()
  df[["smoking_hist"]] <- NULL
  expect_no_error(mf.pack_year(df, "cig_per_day", "smk_years"))
})

test_that("mf.pack_year result length equals nrow(data)", {
  df <- .make_df()
  expect_length(mf.pack_year(df, "cig_per_day", "smk_years"), nrow(df))
})


# --- mf.sum ------------------------------------------------------------------

test_that("mf.sum returns rowSums of named columns as character", {
  df <- .make_df()
  res <- mf.sum(df, "weight", "fvc")
  expected <- as.character(df$weight + df$fvc)
  expect_equal(res, expected)
})

test_that("mf.sum result length equals nrow(data)", {
  df <- .make_df()
  expect_length(mf.sum(df, "weight", "fvc"), nrow(df))
})

test_that("mf.sum: NA in any column produces NA row result", {
  df <- .make_df()
  df$weight[2] <- NA
  res <- mf.sum(df, "weight", "fvc")
  expect_true(is.na(suppressWarnings(as.numeric(res[2]))))
})


# --- mf.value_at_spec_visit --------------------------------------------------

test_that("mf.value_at_spec_visit returns Reg-visit value for all rows of same patient", {
  df  <- .make_df()
  res <- mf.value_at_spec_visit(df, var.targ = "fvc", var.visit = "visit",
                                  visit_name = "Reg", var.key = "study_id")
  # P01 Reg fvc = 80, P02 Reg fvc = 65
  expect_equal(res[df$study_id == "P01"], c(80, 80, 80))
  expect_equal(res[df$study_id == "P02"], c(65, 65, 65))
})

test_that("mf.value_at_spec_visit result length equals nrow(data)", {
  df <- .make_df()
  expect_length(
    mf.value_at_spec_visit(df, "fvc", "visit", "Reg", "study_id"),
    nrow(df)
  )
})

test_that("mf.value_at_spec_visit: patient missing the target visit gets NA", {
  df <- .make_df()
  df <- df[df$visit != "Reg" | df$study_id != "P02", ]  # remove P02/Reg
  res <- mf.value_at_spec_visit(df, "fvc", "visit", "Reg", "study_id")
  expect_true(all(is.na(res[df$study_id == "P02"])))
})


# --- mf.wday -----------------------------------------------------------------

test_that("mf.wday returns integer weekday codes", {
  df <- data.frame(
    d = c("2023-01-01", "2023-01-07", NA),
    stringsAsFactors = FALSE
  )
  df$d <- as.Date(df$d)
  res <- mf.wday(df, "d")
  expect_equal(res[1], lubridate::wday(as.Date("2023-01-01")))
  expect_true(is.na(res[3]))
})

test_that("mf.wday result length equals nrow(data)", {
  df <- data.frame(d = as.Date(c("2023-01-01","2023-01-02")))
  expect_length(mf.wday(df, "d"), nrow(df))
})


# --- mf.change_cutoff --------------------------------------------------------

test_that("mf.change_cutoff: change from Reg baseline is 0 at Reg", {
  df  <- .make_df()
  res <- mf.change_cutoff(df, "fvc", "visit", "study_id", "Reg")
  expect_equal(res[df$study_id == "P01" & df$visit == "Reg"], 0)
  expect_equal(res[df$study_id == "P02" & df$visit == "Reg"], 0)
})

test_that("mf.change_cutoff: W4 change equals fvc_W4 - fvc_Reg", {
  df  <- .make_df()
  res <- mf.change_cutoff(df, "fvc", "visit", "study_id", "Reg")
  expect_equal(res[df$study_id == "P01" & df$visit == "W4"],  78 - 80)
  expect_equal(res[df$study_id == "P02" & df$visit == "W4"],  67 - 65)
})

test_that("mf.change_cutoff result length equals nrow(data)", {
  df <- .make_df()
  expect_length(mf.change_cutoff(df, "fvc", "visit", "study_id", "Reg"), nrow(df))
})

test_that("mf.change_cutoff: patient with no baseline row produces NA", {
  df <- .make_df()
  df <- df[!(df$study_id == "P02" & df$visit == "Reg"), ]
  res <- mf.change_cutoff(df, "fvc", "visit", "study_id", "Reg")
  expect_true(all(is.na(res[df$study_id == "P02"])))
})


# --- mf.text_parse -----------------------------------------------------------

test_that("mf.text_parse evaluates expression with data columns in scope", {
  df  <- .make_df()
  res <- mf.text_parse(df, "paste0(study_id, '_', visit)")
  expect_equal(res[1], "P01_Reg")
})

test_that("mf.text_parse result length equals nrow(data)", {
  df <- .make_df()
  expect_length(mf.text_parse(df, "weight + fvc"), nrow(df))
})
