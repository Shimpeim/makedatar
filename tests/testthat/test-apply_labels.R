# Tests for md_apply_labels()


# --- Fixture -----------------------------------------------------------------

.make_label_df <- function() {
  data.frame(
    study_id    = paste0("P0", 1:6),
    visit       = c("Reg","W4","W12","Reg","W4","W12"),
    blood_yn    = c("1","0","1","0","1","0"),
    seg_cat     = c(0, 1, 1, 0, 1, 0),
    stringsAsFactors = FALSE
  )
}

.label_row <- function(col_name, var.level, var.label, orig_name = "x",
                        col_type = "text") {
  data.frame(
    col_name  = col_name,
    orig_name = orig_name,
    var.level = var.level,
    var.label = var.label,
    col_type  = col_type,
    stringsAsFactors = FALSE
  )
}


# --- Empty input -------------------------------------------------------------

test_that("md_apply_labels returns unchanged data when label_rows is empty", {
  df    <- .make_label_df()
  empty <- data.frame(col_name=character(0), orig_name=character(0),
                       var.level=character(0), var.label=character(0),
                       col_type=character(0), stringsAsFactors=FALSE)
  out <- md_apply_labels(df, empty)
  expect_equal(out, df)
})


# --- Factor conversion -------------------------------------------------------

test_that("md_apply_labels converts column to factor", {
  df  <- .make_label_df()
  row <- .label_row("blood_yn", "{1}{0}", "{Yes}{No}")
  out <- md_apply_labels(df, row)
  expect_true(is.factor(out$blood_yn))
})

test_that("md_apply_labels factor levels match var.level brace-list", {
  df  <- .make_label_df()
  row <- .label_row("blood_yn", "{1}{0}", "{Yes}{No}")
  out <- md_apply_labels(df, row)
  expect_equal(levels(out$blood_yn), c("Yes","No"))
})

test_that("md_apply_labels factor values are correct labels", {
  df  <- .make_label_df()
  row <- .label_row("blood_yn", "{1}{0}", "{Yes}{No}")
  out <- md_apply_labels(df, row)
  expect_equal(as.character(out$blood_yn),
               ifelse(df$blood_yn == "1", "Yes", "No"))
})

test_that("md_apply_labels: three-level encoding", {
  df  <- .make_label_df()
  row <- .label_row("visit", "{Reg}{W4}{W12}", "{Registration}{Week 4}{Week 12}",
                    col_type = "text")
  out <- md_apply_labels(df, row)
  expect_equal(levels(out$visit), c("Registration","Week 4","Week 12"))
  expect_equal(as.character(out$visit[1:3]),
               c("Registration","Week 4","Week 12"))
})


# --- var.level = NA (infer from data) ----------------------------------------

test_that("md_apply_labels: NA var.level infers levels from data", {
  df  <- .make_label_df()
  # seg_cat has values 0 and 1; sorted → c(0, 1) → two levels → two labels
  row <- .label_row("seg_cat", NA_character_, "{Low}{High}", col_type="numeric")
  out <- md_apply_labels(df, row)
  expect_true(is.factor(out$seg_cat))
  expect_equal(levels(out$seg_cat), c("Low","High"))
})


# --- Non-existent column is silently skipped ---------------------------------

test_that("md_apply_labels silently skips absent column", {
  df  <- .make_label_df()
  row <- .label_row("no_such_col", "{1}{0}", "{Yes}{No}")
  expect_no_error(md_apply_labels(df, row))
})


# --- NA var.label is skipped -------------------------------------------------

test_that("md_apply_labels skips row when var.label is NA", {
  df  <- .make_label_df()
  row <- .label_row("blood_yn", "{1}{0}", NA_character_)
  out <- md_apply_labels(df, row)
  expect_false(is.factor(out$blood_yn))
})


# --- Error: levels / labels count mismatch -----------------------------------

test_that("md_apply_labels raises error when level and label counts differ", {
  df  <- .make_label_df()
  row <- .label_row("blood_yn", "{1}{0}{NA}", "{Yes}{No}")
  expect_error(md_apply_labels(df, row), "3 levels but 2")
})


# --- Multiple rows -----------------------------------------------------------

test_that("md_apply_labels processes multiple rows independently", {
  df  <- .make_label_df()
  rows <- rbind(
    .label_row("blood_yn", "{1}{0}", "{Yes}{No}"),
    .label_row("visit",    "{Reg}{W4}{W12}", "{Reg}{W4}{W12}", col_type="text")
  )
  out <- md_apply_labels(df, rows)
  expect_true(is.factor(out$blood_yn))
  expect_true(is.factor(out$visit))
})


# --- Row count and column order unchanged ------------------------------------

test_that("md_apply_labels does not change row count", {
  df  <- .make_label_df()
  row <- .label_row("blood_yn", "{1}{0}", "{Yes}{No}")
  out <- md_apply_labels(df, row)
  expect_equal(nrow(out), nrow(df))
})

test_that("md_apply_labels preserves column order", {
  df  <- .make_label_df()
  row <- .label_row("blood_yn", "{1}{0}", "{Yes}{No}")
  out <- md_apply_labels(df, row)
  expect_equal(colnames(out), colnames(df))
})
