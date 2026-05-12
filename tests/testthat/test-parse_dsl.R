test_that(".parse_transform_dsl: two-arg call", {
  r <- .parse_transform_dsl("mf.bmi_func.(height,weight)")
  expect_equal(r$func_name, "mf.bmi")
  expect_equal(r$args, list("height", "weight"))
})

test_that(".parse_transform_dsl: one-arg call", {
  r <- .parse_transform_dsl("mf.days_to_year_func.(birth_date)")
  expect_equal(r$func_name, "mf.days_to_year")
  expect_equal(r$args, list("birth_date"))
})

test_that(".parse_transform_dsl: multi-arg with underscores in func name", {
  r <- .parse_transform_dsl("mf.value_at_spec_visit_func.(fvc,visit_name,W 0,study_id)")
  expect_equal(r$func_name, "mf.value_at_spec_visit")
  expect_equal(r$args, list("fvc", "visit_name", "W 0", "study_id"))
})

test_that(".parse_transform_dsl: bare function name (no args)", {
  r <- .parse_transform_dsl("identity")
  expect_equal(r$func_name, "identity")
  expect_equal(r$args, list())
})

test_that(".parse_transform_dsl: NA returns identity", {
  r <- .parse_transform_dsl(NA_character_)
  expect_equal(r$func_name, "identity")
  expect_equal(r$args, list())
})

test_that(".parse_transform_dsl: empty string returns identity", {
  r <- .parse_transform_dsl("")
  expect_equal(r$func_name, "identity")
  expect_equal(r$args, list())
})

test_that(".parse_transform_dsl: whitespace around args is stripped", {
  r <- .parse_transform_dsl("mf.diff_func.( x1 , x2 )")
  expect_equal(r$args, list("x1", "x2"))
})

test_that(".parse_transform_dsl: five-arg call (mf.change_cutoff)", {
  r <- .parse_transform_dsl(
    "mf.change_cutoff_func.(percent_fvc,visit_name,study_id,W 0)"
  )
  expect_equal(r$func_name, "mf.change_cutoff")
  expect_equal(length(r$args), 4L)
  expect_equal(r$args[[1]], "percent_fvc")
})

test_that(".parse_transform_dsl: length-1 is enforced", {
  expect_error(.parse_transform_dsl(c("a", "b")))
})

test_that(".parse_transform_dsl_vec: vectorises correctly", {
  exprs <- c("mf.bmi_func.(height,weight)", "identity", NA_character_)
  res   <- .parse_transform_dsl_vec(exprs)
  expect_length(res, 3L)
  expect_equal(res[[1]]$func_name, "mf.bmi")
  expect_equal(res[[2]]$func_name, "identity")
  expect_equal(res[[3]]$func_name, "identity")
})


# --- .parse_braces ------------------------------------------------------------

test_that(".parse_braces: two values", {
  expect_equal(.parse_braces("{Male}{Female}"), c("Male", "Female"))
})

test_that(".parse_braces: three values", {
  expect_equal(
    .parse_braces("{never}{former}{current}"),
    c("never", "former", "current")
  )
})

test_that(".parse_braces: single value", {
  expect_equal(.parse_braces("{active}"), "active")
})

test_that(".parse_braces: values containing comparison operators", {
  expect_equal(.parse_braces("{>=0}{<0}"), c(">=0", "<0"))
})

test_that(".parse_braces: values containing spaces", {
  expect_equal(.parse_braces("{W 0}{W 4}{W 12}"), c("W 0", "W 4", "W 12"))
})

test_that(".parse_braces: NA returns character(0)", {
  expect_equal(.parse_braces(NA), character(0L))
  expect_equal(.parse_braces(NA_character_), character(0L))
})

test_that(".parse_braces: no leading brace raises error by default", {
  expect_error(.parse_braces("plain_string"))
})

test_that(".parse_braces: allow_bare returns scalar", {
  expect_equal(.parse_braces("plain_string", allow_bare = TRUE), "plain_string")
})

test_that(".parse_braces: length-1 is enforced", {
  expect_error(.parse_braces(c("{A}{B}", "{C}")))
})

test_that(".parse_braces_vec: vectorises and preserves NAs", {
  sv  <- c("{A}{B}", NA, "{C}")
  res <- .parse_braces_vec(sv, allow_bare = FALSE)
  expect_length(res, 3L)
  expect_equal(res[[1]], c("A", "B"))
  expect_equal(res[[2]], character(0L))
  expect_equal(res[[3]], "C")
})

test_that(".parse_braces_vec: same length as input", {
  sv <- c("{X}{Y}", "{Z}")
  expect_length(.parse_braces_vec(sv), 2L)
})


# --- round-trip property ------------------------------------------------------
# .parse_braces(paste0("{", paste(v, collapse="}{"), "}")) == v

test_that("brace round-trip for arbitrary string vectors", {
  original <- c("alpha", "beta value", "gamma>=1")
  encoded  <- paste0("{", paste(original, collapse = "}{"), "}")
  expect_equal(.parse_braces(encoded), original)
})
