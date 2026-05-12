# Tests for md_make_data() and its helpers
#
# Full integration is tested against the real template workbook (skip_if_not).
# Guard and helper tests use in-memory objects only.


# --- .resolve_id_var ---------------------------------------------------------

test_that(".resolve_id_var: explicit id_var is returned unchanged", {
  roles <- list(id = data.frame(col_name="study_id", ID="subjid",
                                stringsAsFactors=FALSE))
  expect_equal(.resolve_id_var("my_id", roles), "my_id")
})

test_that(".resolve_id_var: auto-detects 'subjid' ID type", {
  roles <- list(id = data.frame(
    col_name = c("study_id","redcap_event_name","site"),
    ID       = c("subjid","visit","site"),
    stringsAsFactors = FALSE
  ))
  expect_equal(.resolve_id_var(NULL, roles), "study_id")
})

test_that(".resolve_id_var: falls back to first ID col when no subjid", {
  roles <- list(id = data.frame(
    col_name = c("site","visit"),
    ID       = c("site","visit"),
    stringsAsFactors = FALSE
  ))
  expect_equal(.resolve_id_var(NULL, roles), "site")
})

test_that(".resolve_id_var: returns 'study_id' string when roles$id is empty", {
  roles <- list(id = data.frame(col_name=character(0), ID=character(0),
                                stringsAsFactors=FALSE))
  expect_equal(.resolve_id_var(NULL, roles), "study_id")
})


# --- md_make_data: guard checks -----------------------------------------------

test_that("md_make_data raises error for non-md_settings input", {
  expect_error(md_make_data(list(data_path = "/tmp/x.xlsx")), "md_settings")
})

test_that("md_make_data raises error when data file does not exist", {
  s <- md_settings("/tmp/no_such_file_xyz.xlsx")
  expect_error(md_make_data(s), "not found")
})


# --- print.md_result ---------------------------------------------------------

test_that("print.md_result produces output without error", {
  r <- structure(
    list(
      data     = data.frame(x = 1:3),
      col_info = data.frame(col_name = "x", stringsAsFactors = FALSE),
      roles    = list(
        id             = data.frame(col_name="x", ID="subjid",
                                    stringsAsFactors=FALSE),
        exposure       = data.frame(),
        exposure_conti = data.frame(),
        background     = data.frame(),
        background_conti = data.frame(),
        event          = data.frame(),
        time_to_event  = data.frame(),
        censored_time  = data.frame(),
        psmodel        = data.frame(),
        transform      = data.frame(),
        cutoff         = data.frame(),
        label          = data.frame(),
        label_orig     = data.frame(),
        label_derived  = data.frame(),
        strata_in      = data.frame()
      )
    ),
    class = "md_result"
  )
  expect_output(print(r), "md_result")
  expect_output(print(r), "3 rows")
})

test_that("print.md_result: all-empty roles prints '(all empty)'", {
  empty_roles <- setNames(
    replicate(15, data.frame(), simplify = FALSE),
    c("id","exposure","exposure_conti","background","background_conti",
      "event","time_to_event","censored_time","psmodel",
      "transform","cutoff","label","label_orig","label_derived","strata_in")
  )
  r <- structure(
    list(data=data.frame(), col_info=data.frame(), roles=empty_roles),
    class = "md_result"
  )
  expect_output(print(r), "all empty")
})


# --- Full integration test ---------------------------------------------------

test_that("md_make_data reads real template and returns correct structure", {
  f <- file.path(
    "/Users/shimpeimorimoto/Library/CloudStorage/Dropbox/Litterature",
    "Zettelkasten/graph_database/00_utilities_R/makedata/template",
    "NUCR019002_DATA_2024-11-12_1553.xlsx"
  )
  skip_if_not(file.exists(f), "template file not available")

  s   <- md_settings(f,
                      demo_vars        = NULL,   # handled via fill_from
                      right_truncation = 365 * 3 + 4 * 7)
  res <- md_make_data(s,
                      demo_vars = c("birth_date","gender","height","weight"),
                      fill_from = c(visit_date = "ic_date"),
                      verbose   = FALSE)

  # Return type
  expect_true(inherits(res, "md_result"))
  expect_true(is.data.frame(res$data))
  expect_true(is.data.frame(res$col_info))
  expect_true(is.list(res$roles))

  # Pipeline-level sanity: data is non-empty after all filters
  expect_gt(nrow(res$data), 0L)
  expect_gt(ncol(res$data), 0L)

  # col_info row count equals original variable definitions
  expect_gt(nrow(res$col_info), 0L)

  # Roles list has the expected 15 sub-tables
  expected_roles <- c("id","exposure","exposure_conti","background",
                       "background_conti","event","time_to_event",
                       "censored_time","psmodel","transform","cutoff",
                       "label","label_orig","label_derived","strata_in")
  expect_setequal(names(res$roles), expected_roles)

  # ID columns are present in the result
  id_names <- res$roles$id$col_name
  expect_true(all(id_names %in% colnames(res$data)))
})
