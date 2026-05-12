# Tests for md_apply_strata()


# --- Fixture -----------------------------------------------------------------

.make_strata_df <- function() {
  data.frame(
    study_id = paste0("P0", 1:6),
    visit    = c("Reg","W4","W12","Reg","W4","W12"),
    site     = c("A","A","A","B","B","B"),
    fvc      = c(80, 78, 75, 65, 67, 63),
    stringsAsFactors = FALSE
  )
}

.strata_row <- function(col_name, strata_in) {
  data.frame(
    col_name  = col_name,
    strata_in = strata_in,
    stringsAsFactors = FALSE
  )
}


# --- Empty input -------------------------------------------------------------

test_that("md_apply_strata returns unchanged data when strata_rows is empty", {
  df    <- .make_strata_df()
  empty <- data.frame(col_name=character(0), strata_in=character(0),
                       stringsAsFactors=FALSE)
  out <- md_apply_strata(df, empty)
  expect_equal(out, df)
})


# --- Single filter -----------------------------------------------------------

test_that("md_apply_strata keeps only allowed visit rows", {
  df  <- .make_strata_df()
  out <- md_apply_strata(df, .strata_row("visit", "{Reg}{W4}"))
  expect_true(all(out$visit %in% c("Reg","W4")))
  expect_false("W12" %in% out$visit)
})

test_that("md_apply_strata: row count equals count of matching rows", {
  df  <- .make_strata_df()
  out <- md_apply_strata(df, .strata_row("visit", "{Reg}"))
  expect_equal(nrow(out), sum(df$visit == "Reg"))
})

test_that("md_apply_strata: filtering on site='A' keeps 3 rows", {
  df  <- .make_strata_df()
  out <- md_apply_strata(df, .strata_row("site", "{A}"))
  expect_equal(nrow(out), 3L)
  expect_true(all(out$site == "A"))
})


# --- Multiple filters (AND logic) --------------------------------------------

test_that("md_apply_strata: two filters are applied sequentially (AND)", {
  df  <- .make_strata_df()
  rows <- rbind(
    .strata_row("visit", "{Reg}{W4}"),
    .strata_row("site",  "{A}")
  )
  out <- md_apply_strata(df, rows)
  expect_equal(nrow(out), 2L)
  expect_true(all(out$site == "A"))
  expect_true(all(out$visit %in% c("Reg","W4")))
})


# --- All rows removed --------------------------------------------------------

test_that("md_apply_strata: non-matching filter produces zero-row data.frame", {
  df  <- .make_strata_df()
  out <- md_apply_strata(df, .strata_row("visit", "{NonExistent}"))
  expect_equal(nrow(out), 0L)
  expect_equal(ncol(out), ncol(df))
})


# --- Edge cases --------------------------------------------------------------

test_that("md_apply_strata silently skips absent column", {
  df  <- .make_strata_df()
  expect_no_error(
    md_apply_strata(df, .strata_row("no_such_col", "{Reg}"))
  )
  out <- md_apply_strata(df, .strata_row("no_such_col", "{Reg}"))
  expect_equal(nrow(out), nrow(df))
})

test_that("md_apply_strata silently skips NA strata_in", {
  df  <- .make_strata_df()
  out <- md_apply_strata(df, .strata_row("visit", NA_character_))
  expect_equal(nrow(out), nrow(df))
})

test_that("md_apply_strata resets rownames after filtering", {
  df  <- .make_strata_df()
  out <- md_apply_strata(df, .strata_row("visit", "{Reg}"))
  expect_equal(rownames(out), as.character(seq_len(nrow(out))))
})

test_that("md_apply_strata preserves column names", {
  df  <- .make_strata_df()
  out <- md_apply_strata(df, .strata_row("visit", "{Reg}"))
  expect_equal(colnames(out), colnames(df))
})
