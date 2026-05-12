# Tests for md_extract_var_roles()

.roles <- function() md_extract_var_roles(.fixture_col_info())


# --- Return shape ------------------------------------------------------------

test_that("result is a named list with all 15 elements", {
  r <- .roles()
  expected <- c(
    "id", "exposure", "exposure_conti",
    "background", "background_conti",
    "event", "time_to_event", "censored_time",
    "psmodel",
    "transform", "cutoff",
    "label", "label_orig", "label_derived",
    "strata_in"
  )
  expect_setequal(names(r), expected)
})

test_that("every element is a data.frame", {
  r <- .roles()
  for (nm in names(r)) {
    expect_true(is.data.frame(r[[nm]]),
                label = sprintf("$%s should be a data.frame", nm))
  }
})


# --- $id ---------------------------------------------------------------------
# ID column holds a TYPE LABEL, not the col_name.
# Real file: "subjid", "visit", "site"

test_that("$id has three rows matching the three ID variables", {
  r <- .roles()
  expect_equal(nrow(r$id), 3L)
  expect_setequal(r$id$col_name, c("study_id", "redcap_event_name", "site"))
})

test_that("$id$ID holds type labels, not col_names", {
  r <- .roles()
  expect_setequal(r$id$ID, c("subjid", "visit", "site"))
})

test_that("$id has columns col_name and ID", {
  r <- .roles()
  expect_true(all(c("col_name", "ID") %in% colnames(r$id)))
})

test_that("$id excludes rows where ID is NA", {
  r <- .roles()
  expect_false("seg" %in% r$id$col_name)
  expect_false("blood_yn" %in% r$id$col_name)
})


# --- $exposure / $exposure_conti ---------------------------------------------
# In the fixture (matching real data): all exposures are numeric.

test_that("$exposure is empty (no text-type exposures in fixture)", {
  r <- .roles()
  expect_equal(nrow(r$exposure), 0L)
})

test_that("$exposure_conti has the two numeric exposure variables", {
  r <- .roles()
  expect_equal(nrow(r$exposure_conti), 2L)
  expect_setequal(r$exposure_conti$col_name, c("seg.init", "lymph.init"))
})

test_that("$exposure_conti has correct columns", {
  r <- .roles()
  expect_true(all(c("col_name", "exposure", "timepoint") %in%
                    colnames(r$exposure_conti)))
})

test_that("$exposure_conti timepoints are 'Reg'", {
  r <- .roles()
  expect_true(all(r$exposure_conti$timepoint == "Reg"))
})

test_that("adding a text exposure moves it to $exposure not $exposure_conti", {
  df <- .fixture_col_info()
  df$exposure[df$col_name  == "seg.init"] <- "seg.init"
  df$col_type[df$col_name  == "seg.init"] <- "text"
  r <- md_extract_var_roles(df)
  expect_equal(nrow(r$exposure),       1L)
  expect_equal(nrow(r$exposure_conti), 1L)  # lymph.init remains numeric
})


# --- $background / $background_conti ----------------------------------------

test_that("$background is empty (no text-type backgrounds in fixture)", {
  r <- .roles()
  expect_equal(nrow(r$background), 0L)
})

test_that("$background_conti has seg and lymph", {
  r <- .roles()
  expect_setequal(r$background_conti$col_name, c("seg", "lymph"))
})

test_that("$background_conti has correct columns", {
  r <- .roles()
  expect_true(all(c("col_name", "background", "timepoint") %in%
                    colnames(r$background_conti)))
})


# --- $event / $time_to_event / $censored_time --------------------------------

test_that("$event is empty in fixture", {
  expect_equal(nrow(.roles()$event), 0L)
})

test_that("$event captures 'event' outcome rows", {
  df <- .fixture_col_info()
  df$outcome[df$col_name == "seg"] <- "event"
  r  <- md_extract_var_roles(df)
  expect_equal(nrow(r$event), 1L)
  expect_equal(r$event$col_name, "seg")
})

test_that("$event captures 'rank' outcome rows", {
  df <- .fixture_col_info()
  df$outcome[df$col_name == "seg"] <- "rank"
  r  <- md_extract_var_roles(df)
  expect_equal(r$event$outcome, "rank")
})

test_that("$time_to_event captures 'time' outcome rows", {
  df <- .fixture_col_info()
  df$outcome[df$col_name == "lymph"] <- "time"
  r  <- md_extract_var_roles(df)
  expect_equal(r$time_to_event$col_name, "lymph")
})

test_that("$censored_time captures 'censored.time' outcome rows", {
  df <- .fixture_col_info()
  df$outcome[df$col_name == "lymph"] <- "censored.time"
  r  <- md_extract_var_roles(df)
  expect_equal(r$censored_time$col_name, "lymph")
})


# --- $psmodel ----------------------------------------------------------------

test_that("$psmodel contains seg and lymph", {
  r <- .roles()
  expect_setequal(r$psmodel$col_name, c("seg", "lymph"))
})

test_that("$psmodel excludes non-Psmodel rows", {
  r <- .roles()
  expect_false("study_id"  %in% r$psmodel$col_name)
  expect_false("seg.init"  %in% r$psmodel$col_name)
  expect_false("blood_yn"  %in% r$psmodel$col_name)
})


# --- $transform --------------------------------------------------------------
# Only rows where transform non-NA AND cutoff is NA.
# seg_cat has transform="seg" but cutoff non-NA → must be excluded.

test_that("$transform has the two DSL-transform rows", {
  r <- .roles()
  expect_equal(nrow(r$transform), 2L)
  expect_setequal(r$transform$col_name, c("seg.init", "lymph.init"))
})

test_that("$transform DSL strings contain '_func.('", {
  r <- .roles()
  expect_true(all(grepl("_func[.][(]", r$transform$transform)))
})

test_that("$transform excludes cutoff rows (seg_cat)", {
  r <- .roles()
  expect_false("seg_cat" %in% r$transform$col_name)
})

test_that("$transform has columns transform, col_name, col_type", {
  r <- .roles()
  expect_true(all(c("transform", "col_name", "col_type") %in%
                    colnames(r$transform)))
})

test_that("transform DSL may contain R-object references as arguments", {
  r <- .roles()
  # The 5th argument 'var.ID' is an R object name, not a string literal.
  # .parse_transform_dsl should return it as a plain string arg.
  parsed <- .parse_transform_dsl(r$transform$transform[1])
  expect_true("var.ID" %in% unlist(parsed$args))
})


# --- $cutoff -----------------------------------------------------------------

test_that("$cutoff has one row (seg_cat)", {
  r <- .roles()
  expect_equal(nrow(r$cutoff), 1L)
  expect_equal(r$cutoff$col_name, "seg_cat")
})

test_that("$cutoff transform column holds source variable name (not DSL)", {
  r <- .roles()
  expect_equal(r$cutoff$transform, "seg")
  expect_false(grepl("_func[.][(]", r$cutoff$transform))
})

test_that("$cutoff has correct columns", {
  r <- .roles()
  expect_true(all(
    c("col_name", "transform", "var.score", "cutoff", "col_type") %in%
      colnames(r$cutoff)
  ))
})


# --- $label / $label_orig / $label_derived -----------------------------------
# var.label is on the same row as the variable definition.
# label_orig  (orig_name non-NA): redcap_event_name, site, blood_yn
# label_derived (orig_name NA):   seg.init

test_that("$label has four rows (three orig + one derived)", {
  r <- .roles()
  expect_equal(nrow(r$label), 4L)
  expect_setequal(r$label$col_name,
                  c("redcap_event_name", "site", "blood_yn", "seg.init"))
})

test_that("$label_orig: three rows with orig_name non-NA", {
  r <- .roles()
  expect_equal(nrow(r$label_orig), 3L)
  expect_setequal(r$label_orig$col_name,
                  c("redcap_event_name", "site", "blood_yn"))
  expect_true(all(!is.na(r$label_orig$orig_name)))
})

test_that("$label_derived: one row with orig_name NA", {
  r <- .roles()
  expect_equal(nrow(r$label_derived), 1L)
  expect_equal(r$label_derived$col_name, "seg.init")
  expect_true(is.na(r$label_derived$orig_name))
})

test_that("label_orig includes varseq=NA rows (site, blood_yn)", {
  r <- .roles()
  expect_true("site"     %in% r$label_orig$col_name)
  expect_true("blood_yn" %in% r$label_orig$col_name)
})

test_that("$label_orig excludes skip col_type rows", {
  df <- .fixture_col_info()
  df$col_type[df$col_name == "blood_yn"] <- "skip"
  r  <- md_extract_var_roles(df)
  expect_false("blood_yn" %in% r$label_orig$col_name)
})

test_that("$label has correct columns", {
  r <- .roles()
  expect_true(all(
    c("orig_name", "col_name", "var.level", "var.label", "col_type") %in%
      colnames(r$label)
  ))
})


# --- $strata_in --------------------------------------------------------------

test_that("$strata_in is empty when no strata filters defined", {
  expect_equal(nrow(.roles()$strata_in), 0L)
})

test_that("$strata_in picks the correct row when present", {
  df <- .fixture_col_info()
  df$strata_in[df$col_name == "redcap_event_name"] <- "{Reg}{W4}"
  r  <- md_extract_var_roles(df)
  expect_equal(nrow(r$strata_in), 1L)
  expect_equal(r$strata_in$col_name, "redcap_event_name")
})


# --- Edge cases --------------------------------------------------------------

test_that("all elements empty for col_info with no role columns", {
  df <- data.frame(col_name="x", col_type="numeric", varseq=1L,
                   orig_name="X", stringsAsFactors=FALSE)
  r  <- md_extract_var_roles(df)
  for (nm in names(r)) {
    expect_equal(nrow(r[[nm]]), 0L, label = sprintf("$%s empty", nm))
  }
})

test_that("missing optional columns tolerated — all sub-tables empty", {
  r <- md_extract_var_roles(.fixture_col_info_minimal())
  for (nm in names(r)) {
    expect_equal(nrow(r[[nm]]), 0L)
  }
})
