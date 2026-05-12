# Tests for md_read_col_info()

# Helper: simulate the post-read processing on an in-memory data.frame
.simulate_read_col_info <- function(df) {
  .assert_cols(df, .COL_INFO_REQUIRED, label = "col_info")
  for (col in .COL_INFO_OPTIONAL) {
    if (!col %in% colnames(df)) df[[col]] <- NA_character_
  }
  df[["varseq"]] <- suppressWarnings(as.integer(df[["varseq"]]))
  df <- df[order(df[["varseq"]], na.last = TRUE), , drop = FALSE]
  rownames(df) <- NULL
  df
}


# --- Required column validation ----------------------------------------------

test_that("missing col_name raises error", {
  df <- .fixture_col_info_minimal()
  df[["col_name"]] <- NULL
  expect_error(.simulate_read_col_info(df), "col_name")
})

test_that("missing varseq raises error", {
  df <- .fixture_col_info_minimal()
  df[["varseq"]] <- NULL
  expect_error(.simulate_read_col_info(df), "varseq")
})

test_that("missing col_type raises error", {
  df <- .fixture_col_info_minimal()
  df[["col_type"]] <- NULL
  expect_error(.simulate_read_col_info(df), "col_type")
})

test_that("missing orig_name raises error", {
  df <- .fixture_col_info_minimal()
  df[["orig_name"]] <- NULL
  expect_error(.simulate_read_col_info(df), "orig_name")
})

test_that("all four required columns present — no error", {
  expect_no_error(.simulate_read_col_info(.fixture_col_info_minimal()))
})


# --- Optional column back-fill -----------------------------------------------

test_that("absent optional columns are added as all-NA", {
  out <- .simulate_read_col_info(.fixture_col_info_minimal())
  for (col in .COL_INFO_OPTIONAL) {
    expect_true(col %in% colnames(out),
                label = sprintf("'%s' should be added", col))
    expect_true(all(is.na(out[[col]])),
                label = sprintf("back-filled '%s' should be all-NA", col))
  }
})

test_that("existing optional columns are preserved", {
  out <- .simulate_read_col_info(.fixture_col_info())
  # ID has three non-NA type labels: "subjid", "visit", "site"
  expect_equal(sum(!is.na(out[["ID"]])), 3L)
  expect_setequal(out[["ID"]][!is.na(out[["ID"]])], c("subjid", "visit", "site"))
})

test_that("extra project-specific columns (beyond optional set) are preserved", {
  df <- .fixture_col_info()
  df[["strata_out"]] <- NA_character_   # real file has this column
  df[["Psmodel.1"]]  <- NA_character_
  out <- .simulate_read_col_info(df)
  expect_true("strata_out" %in% colnames(out))
  expect_true("Psmodel.1"  %in% colnames(out))
})


# --- varseq sorting ----------------------------------------------------------

test_that("non-NA varseq rows are sorted ascending", {
  out <- .simulate_read_col_info(.fixture_col_info_unsorted())
  non_na_seq <- out[["varseq"]][!is.na(out[["varseq"]])]
  expect_equal(non_na_seq, sort(non_na_seq))
})

test_that("NA varseq rows are placed at the end", {
  out <- .simulate_read_col_info(.fixture_col_info())
  na_positions  <- which(is.na(out[["varseq"]]))
  non_na_positions <- which(!is.na(out[["varseq"]]))
  expect_true(all(na_positions > max(non_na_positions)))
})

test_that("col_name order matches varseq order for non-NA rows", {
  out  <- .simulate_read_col_info(.fixture_col_info())
  ci   <- .fixture_col_info()
  non_na_ci <- ci[!is.na(ci[["varseq"]]), ]
  expected <- non_na_ci[["col_name"]][order(non_na_ci[["varseq"]])]
  expect_equal(out[["col_name"]][!is.na(out[["varseq"]])], expected)
})

test_that("varseq is coerced to integer from character (Excel string import)", {
  df <- .fixture_col_info()
  df[["varseq"]] <- as.character(df[["varseq"]])
  out <- .simulate_read_col_info(df)
  expect_type(out[["varseq"]], "integer")
})

test_that("rownames are reset to 1..n after sort", {
  out <- .simulate_read_col_info(.fixture_col_info_unsorted())
  expect_equal(rownames(out), as.character(seq_len(nrow(out))))
})


# --- Shape -------------------------------------------------------------------

test_that("result has all required and optional columns", {
  out <- .simulate_read_col_info(.fixture_col_info())
  expect_true(all(.COL_INFO_REQUIRED %in% colnames(out)))
  expect_true(all(.COL_INFO_OPTIONAL %in% colnames(out)))
})

test_that("row count is unchanged", {
  df  <- .fixture_col_info()
  out <- .simulate_read_col_info(df)
  expect_equal(nrow(out), nrow(df))
})


# --- File-not-found guard ----------------------------------------------------

test_that("non-existent file raises informative error", {
  expect_error(md_read_col_info("/tmp/no_such_file_xyz.xlsx"), "file not found")
})
