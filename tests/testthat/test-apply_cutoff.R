# Tests for md_apply_cutoff()


# --- Fixture -----------------------------------------------------------------

.make_cutoff_df <- function() {
  data.frame(
    study_id = paste0("P0", 1:6),
    seg      = c(35, 42, 55, 28, 61, NA),
    stringsAsFactors = FALSE
  )
}

.cutoff_row <- function(col_name, src, cutoff, score, col_type = "numeric") {
  data.frame(
    col_name  = col_name,
    transform = src,
    cutoff    = cutoff,
    var.score = score,
    col_type  = col_type,
    stringsAsFactors = FALSE
  )
}


# --- Return shape ------------------------------------------------------------

test_that("md_apply_cutoff returns unchanged data when cutoff_rows is empty", {
  df  <- .make_cutoff_df()
  empty <- data.frame(col_name=character(0), transform=character(0),
                       cutoff=character(0), var.score=character(0),
                       col_type=character(0), stringsAsFactors=FALSE)
  out <- md_apply_cutoff(df, empty)
  expect_equal(out, df)
})

test_that("md_apply_cutoff adds new column of same length as input", {
  df  <- .make_cutoff_df()
  out <- md_apply_cutoff(df, .cutoff_row("seg_cat", "seg", "{<40}{>=40}", "{0}{1}"))
  expect_true("seg_cat" %in% colnames(out))
  expect_equal(length(out$seg_cat), nrow(df))
})

test_that("md_apply_cutoff does not change row count", {
  df  <- .make_cutoff_df()
  out <- md_apply_cutoff(df, .cutoff_row("seg_cat", "seg", "{<40}{>=40}", "{0}{1}"))
  expect_equal(nrow(out), nrow(df))
})


# --- Binary cutoff ({<40}{>=40} → {0}{1}) ------------------------------------

test_that("binary cutoff: values < 40 get score 0", {
  df  <- .make_cutoff_df()
  out <- md_apply_cutoff(df, .cutoff_row("seg_cat", "seg", "{<40}{>=40}", "{0}{1}"))
  expect_equal(out$seg_cat[df$seg < 40 & !is.na(df$seg)], c(0, 0))
})

test_that("binary cutoff: values >= 40 get score 1", {
  df  <- .make_cutoff_df()
  out <- md_apply_cutoff(df, .cutoff_row("seg_cat", "seg", "{<40}{>=40}", "{0}{1}"))
  expect_equal(out$seg_cat[df$seg >= 40 & !is.na(df$seg)], c(1, 1, 1))
})

test_that("binary cutoff: NA source value gives NA result", {
  df  <- .make_cutoff_df()
  out <- md_apply_cutoff(df, .cutoff_row("seg_cat", "seg", "{<40}{>=40}", "{0}{1}"))
  expect_true(is.na(out$seg_cat[is.na(df$seg)]))
})


# --- Three-level cutoff -------------------------------------------------------

test_that("three-level cutoff assigns correct scores", {
  df  <- .make_cutoff_df()
  out <- md_apply_cutoff(
    df,
    .cutoff_row("seg3", "seg",
                "{<40}{>=40 & x<55}{>=55}",
                "{0}{1}{2}")
  )
  # seg values: 35→0, 42→1, 55→2, 28→0, 61→2, NA→NA
  expect_equal(out$seg3[!is.na(df$seg)], c(0, 1, 2, 0, 2))
  expect_true(is.na(out$seg3[is.na(df$seg)]))
})


# --- Negative scores ---------------------------------------------------------

test_that("negative scores produce correct weighted sum", {
  df  <- .make_cutoff_df()
  out <- md_apply_cutoff(
    df,
    .cutoff_row("seg_pm", "seg", "{<40}{>=40}", "{-1}{1}")
  )
  expect_equal(out$seg_pm[1], -1)   # 35 < 40
  expect_equal(out$seg_pm[2],  1)   # 42 >= 40
})


# --- Error conditions --------------------------------------------------------

test_that("md_apply_cutoff raises error for missing source variable", {
  df <- .make_cutoff_df()
  expect_error(
    md_apply_cutoff(df, .cutoff_row("x", "no_col", "{<40}{>=40}", "{0}{1}")),
    "not found"
  )
})

test_that("md_apply_cutoff raises error when condition and score counts differ", {
  df <- .make_cutoff_df()
  expect_error(
    md_apply_cutoff(df, .cutoff_row("x", "seg", "{<40}{>=40}", "{0}{1}{2}")),
    "2 conditions but 3 scores"
  )
})

test_that("md_apply_cutoff skips row when cutoff is NA", {
  df  <- .make_cutoff_df()
  row <- .cutoff_row("seg_cat", "seg", NA_character_, NA_character_)
  out <- md_apply_cutoff(df, row)
  expect_false("seg_cat" %in% colnames(out))
})


# --- Multiple cutoff rows ----------------------------------------------------

test_that("md_apply_cutoff processes multiple rows independently", {
  df  <- .make_cutoff_df()
  rows <- rbind(
    .cutoff_row("seg_cat",  "seg", "{<40}{>=40}", "{0}{1}"),
    .cutoff_row("seg_cat2", "seg", "{<50}{>=50}", "{0}{1}")
  )
  out <- md_apply_cutoff(df, rows)
  expect_true("seg_cat"  %in% colnames(out))
  expect_true("seg_cat2" %in% colnames(out))
  expect_equal(out$seg_cat2[!is.na(df$seg)], c(0, 0, 1, 0, 1))  # 55 >= 50
})
