# Tests for md_import_data() and its private helpers
#
# Excel-reading path requires a real file; tested via the real template.
# The logic-only units are tested against in-memory fixtures:
#   .import_col_spec()    — builds col_names / col_types vectors
#   .reorder_by_varseq()  — sorts columns; NA varseq rows go last

# Fixture col_info summary (10 rows):
#   orig_name non-NA:  study_id(1), redcap_event_name(2), site(NA),
#                      blood_yn(NA), blood_date(NA), seg(10), lymph(11)   = 7 rows
#   orig_name NA:      seg.init(20), lymph.init(21), seg_cat(22)          = 3 rows
#   col_type=="skip":  none in fixture


# --- .import_col_spec --------------------------------------------------------

test_that(".import_col_spec includes only orig_name non-NA rows", {
  ci   <- .fixture_col_info()
  spec <- .import_col_spec(ci)
  n_import <- sum(!is.na(ci$orig_name))   # 7
  expect_length(spec$col_names, n_import)
  expect_length(spec$col_types, n_import)
})

test_that(".import_col_spec col_names match col_info for non-NA orig_name rows", {
  ci       <- .fixture_col_info()
  spec     <- .import_col_spec(ci)
  expected <- ci$col_name[!is.na(ci$orig_name)]
  expect_equal(spec$col_names, expected)
})

test_that(".import_col_spec col_types match col_info for non-NA orig_name rows", {
  ci       <- .fixture_col_info()
  spec     <- .import_col_spec(ci)
  expected <- ci$col_type[!is.na(ci$orig_name)]
  expect_equal(spec$col_types, expected)
})

test_that(".import_col_spec excludes derived variables (orig_name NA)", {
  ci   <- .fixture_col_info()
  spec <- .import_col_spec(ci)
  expect_false("seg.init"  %in% spec$col_names)
  expect_false("lymph.init" %in% spec$col_names)
  expect_false("seg_cat"   %in% spec$col_names)
})

test_that(".import_col_spec includes skip-type rows when orig_name non-NA", {
  ci <- .fixture_col_info()
  ci$col_type[ci$col_name == "blood_yn"] <- "skip"
  spec <- .import_col_spec(ci)
  # blood_yn is still included in spec so readxl can align its col_types vector
  expect_true("blood_yn" %in% spec$col_names)
  expect_true("skip"     %in% spec$col_types)
})

test_that(".import_col_spec: varseq=NA rows are included (site, blood_yn, blood_date)", {
  ci   <- .fixture_col_info()
  spec <- .import_col_spec(ci)
  expect_true("site"       %in% spec$col_names)
  expect_true("blood_yn"   %in% spec$col_names)
  expect_true("blood_date" %in% spec$col_names)
})


# --- .reorder_by_varseq ------------------------------------------------------

# Build a simulated imported data.frame (non-skip, orig_name non-NA cols)
# in REVERSED column order to exercise the sort.
.make_imported <- function() {
  ci      <- .fixture_col_info()
  spec    <- .import_col_spec(ci)
  non_skip <- spec$col_names[spec$col_types != "skip"]
  as.data.frame(
    setNames(
      replicate(length(non_skip), NA_character_, simplify = FALSE),
      rev(non_skip)   # deliberately wrong order
    ),
    stringsAsFactors = FALSE
  )
}

test_that(".reorder_by_varseq: non-NA varseq columns come first, sorted ascending", {
  ci  <- .fixture_col_info()
  df  <- .make_imported()
  out <- .reorder_by_varseq(df, ci)
  # First columns should be those with defined varseq: 1, 2, 10, 11
  keep <- !is.na(ci$orig_name) & ci$col_type != "skip" & !is.na(ci$varseq)
  expected_first <- ci$col_name[keep][order(ci$varseq[keep])]
  expect_equal(head(colnames(out), length(expected_first)), expected_first)
})

test_that(".reorder_by_varseq: varseq=NA columns appear at the end", {
  ci       <- .fixture_col_info()
  df       <- .make_imported()
  out      <- .reorder_by_varseq(df, ci)
  na_cols  <- ci$col_name[!is.na(ci$orig_name) & ci$col_type != "skip" &
                             is.na(ci$varseq)]
  na_pos   <- which(colnames(out) %in% na_cols)
  non_na_pos <- which(!colnames(out) %in% na_cols)
  if (length(na_pos) > 0 && length(non_na_pos) > 0)
    expect_true(min(na_pos) > max(non_na_pos))
})

test_that(".reorder_by_varseq: drops columns not in col_info", {
  ci  <- .fixture_col_info()
  df  <- .make_imported()
  df[["extra_junk"]] <- "noise"
  out <- .reorder_by_varseq(df, ci)
  expect_false("extra_junk" %in% colnames(out))
})

test_that(".reorder_by_varseq: drops skip-type columns from result", {
  ci <- .fixture_col_info()
  ci$col_type[ci$col_name == "blood_yn"] <- "skip"
  df  <- .make_imported()
  df[["blood_yn"]] <- "x"
  out <- .reorder_by_varseq(df, ci)
  expect_false("blood_yn" %in% colnames(out))
})

test_that(".reorder_by_varseq: keeps all non-skip importable columns", {
  ci  <- .fixture_col_info()
  df  <- .make_imported()
  out <- .reorder_by_varseq(df, ci)
  keep_n <- sum(!is.na(ci$orig_name) & ci$col_type != "skip")
  expect_equal(ncol(out), keep_n)
})

test_that(".reorder_by_varseq: returns a data.frame", {
  out <- .reorder_by_varseq(.make_imported(), .fixture_col_info())
  expect_true(is.data.frame(out))
})


# --- Real file integration test ----------------------------------------------

test_that("md_import_data reads real template and returns correct dimensions", {
  f <- file.path(
    "/Users/shimpeimorimoto/Library/CloudStorage/Dropbox/Litterature",
    "Zettelkasten/graph_database/00_utilities_R/makedata/template",
    "NUCR019002_DATA_2024-11-12_1553.xlsx"
  )
  skip_if_not(file.exists(f), "template file not available")

  ci  <- md_read_col_info(f, sheet = "colinfo")
  out <- md_import_data(f, ci, sheet = "data")

  # 1 431 data rows, 14 importable (non-skip, orig_name non-NA) columns
  expect_equal(nrow(out), 1431L)
  expect_equal(ncol(out), 14L)
  # varseq-defined columns come first
  expect_equal(colnames(out)[1], "study_id")
  expect_equal(colnames(out)[2], "redcap_event_name")
})


# --- File-not-found guard ---------------------------------------------------

test_that("md_import_data raises error for missing file", {
  expect_error(
    md_import_data("/tmp/no_such_file_xyz.xlsx", .fixture_col_info()),
    "file not found"
  )
})
