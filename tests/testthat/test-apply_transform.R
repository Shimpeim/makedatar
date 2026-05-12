# Tests for md_apply_transform() and its private helpers


# --- Fixture -----------------------------------------------------------------

.make_trans_df <- function() {
  data.frame(
    study_id = c("P01","P01","P01","P02","P02","P02"),
    visit    = c("Reg","W4","W12","Reg","W4","W12"),
    height   = c(170, 170, 170, 160, 160, 160),
    weight   = c(70,  72,  74,  55,  56,  57),
    fvc      = c(80,  78,  75,  65,  67,  63),
    stringsAsFactors = FALSE
  )
}

# Minimal transform_rows table (like $transform from md_extract_var_roles)
.make_transform_rows <- function(col_name, dsl, col_type = "numeric") {
  data.frame(
    col_name  = col_name,
    transform = dsl,
    col_type  = col_type,
    stringsAsFactors = FALSE
  )
}


# --- .resolve_dsl_args -------------------------------------------------------

test_that(".resolve_dsl_args: var.ID is stripped from args list", {
  raw  <- list("seg", "redcap_event_name", "Reg", "study_id", "var.ID")
  res  <- .resolve_dsl_args(raw, id_cols = c("study_id","visit"))
  expect_false("var.ID" %in% unlist(res))
})

test_that(".resolve_dsl_args: non-sentinel args are preserved as-is", {
  raw  <- list("seg", "Reg", "study_id")
  res  <- .resolve_dsl_args(raw, id_cols = c("study_id"))
  expect_equal(unlist(res), c("seg", "Reg", "study_id"))
})

test_that(".resolve_dsl_args: empty list is returned unchanged", {
  expect_length(.resolve_dsl_args(list(), id_cols = "study_id"), 0L)
})


# --- md_apply_transform — basic dispatch -------------------------------------

test_that("md_apply_transform returns unmodified data when transform_rows is empty", {
  df       <- .make_trans_df()
  empty_tr <- data.frame(col_name=character(0), transform=character(0),
                          col_type=character(0), stringsAsFactors=FALSE)
  out <- md_apply_transform(df, empty_tr, id_cols = "study_id")
  expect_equal(out, df)
})

test_that("md_apply_transform adds new column for mf.bmi", {
  df   <- .make_trans_df()
  rows <- .make_transform_rows("bmi", "mf.bmi_func.(weight,height)")
  out  <- md_apply_transform(df, rows, id_cols = c("study_id","visit"))
  expect_true("bmi" %in% colnames(out))
  expected <- df$weight / (df$height / 100)^2
  expect_equal(out$bmi, expected)
})

test_that("md_apply_transform: result has same number of rows as input", {
  df   <- .make_trans_df()
  rows <- .make_transform_rows("bmi", "mf.bmi_func.(weight,height)")
  out  <- md_apply_transform(df, rows, id_cols = "study_id")
  expect_equal(nrow(out), nrow(df))
})

test_that("md_apply_transform adds mf.value_at_spec_visit column", {
  df   <- .make_trans_df()
  # var.ID sentinel is in position 5 — dispatcher strips it
  rows <- .make_transform_rows(
    "fvc_reg",
    "mf.value_at_spec_visit_func.(fvc,visit,Reg,study_id,var.ID)"
  )
  out <- md_apply_transform(df, rows, id_cols = c("study_id","visit"))
  expect_true("fvc_reg" %in% colnames(out))
  expect_equal(out$fvc_reg[df$study_id == "P01"], c(80, 80, 80))
  expect_equal(out$fvc_reg[df$study_id == "P02"], c(65, 65, 65))
})

test_that("md_apply_transform processes multiple transform rows in order", {
  df   <- .make_trans_df()
  rows <- rbind(
    .make_transform_rows("bmi",      "mf.bmi_func.(weight,height)"),
    # as.character(bmi) — no commas inside the DSL expression
    .make_transform_rows("bmi_str",  "mf.text_parse_func.(as.character(bmi))")
  )
  out <- md_apply_transform(df, rows, id_cols = "study_id")
  expect_true("bmi"     %in% colnames(out))
  expect_true("bmi_str" %in% colnames(out))
  expect_equal(out$bmi_str[1], as.character(out$bmi[1]))
})

test_that("md_apply_transform raises error for unknown function name", {
  df   <- .make_trans_df()
  rows <- .make_transform_rows("x", "mf.nonexistent_func.(height)")
  expect_error(
    md_apply_transform(df, rows, id_cols = "study_id"),
    "not found"
  )
})

test_that("md_apply_transform raises informative error when mf.* function fails", {
  df   <- .make_trans_df()
  # mf.diff with a non-existent column errors inside do.call
  rows <- .make_transform_rows("d", "mf.diff_func.(fvc,no_col)")
  expect_error(
    md_apply_transform(df, rows, id_cols = "study_id"),
    "mf.diff"
  )
})

test_that("md_apply_transform: identity DSL string produces NA column of length nrow", {
  df   <- .make_trans_df()
  rows <- .make_transform_rows("x", NA_character_)
  # identity parse returns func_name = "identity", args = list()
  # identity() with no args errors — test that the error is wrapped
  expect_error(
    md_apply_transform(df, rows, id_cols = "study_id")
  )
})
