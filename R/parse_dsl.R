# =============================================================================
# DSL parsers for the col_info schema table
#
# Two mini-languages are embedded in col_info column values:
#
# 1. Transform DSL  (col_info$transform column)
#    Encodes a function call as a plain string so it can live in an Excel cell.
#    Grammar:  <func_name>_func.(<arg1>,<arg2>,...)
#    Examples:
#      "mf.bmi_func.(height,weight)"        -> func="mf.bmi",   args=["height","weight"]
#      "mf.days_to_year_func.(birth_date)"  -> func="mf.days_to_year", args=["birth_date"]
#      "identity"                           -> func="identity",  args=[]
#
# 2. Brace list  (col_info$var.level, $var.label, $strata_in, $cutoff columns)
#    Encodes a character vector as a single string so multi-value entries fit
#    in one Excel cell.
#    Grammar:  {value1}{value2}{value3}
#    Examples:
#      "{Male}{Female}"   -> c("Male", "Female")
#      "{>=0}{<0}"        -> c(">=0", "<0")
#      NA                 -> character(0)
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Transform DSL
# -----------------------------------------------------------------------------

#' Parse a transform DSL string into a function name and argument list
#'
#' The transform DSL encodes a function call as a plain string so it can be
#' stored in an Excel cell.  The canonical grammar is:
#'
#' ```
#' <func_name>_func.(<arg1>,<arg2>,...)
#' ```
#'
#' If the string does not contain the `_func.(` marker the whole string is
#' treated as a bare function name with no arguments (the common case is
#' `"identity"`).
#'
#' Arguments are split on the first-level commas only; whitespace around each
#' argument is stripped.
#'
#' @param expr A single character string from the `transform` column of a
#'   col_info table.  `NA` is allowed and returns `list(func_name = "identity",
#'   args = list())`.
#'
#' @return A named list with two elements:
#'   * `func_name`  --  character(1), the function to call.
#'   * `args`       --  list of character strings, one per argument.
#'
#' @examples
#' \dontrun{
#' .parse_transform_dsl("mf.bmi_func.(height,weight)")
#' # list(func_name = "mf.bmi", args = list("height", "weight"))
#'
#' .parse_transform_dsl("identity")
#' # list(func_name = "identity", args = list())
#'
#' .parse_transform_dsl(NA_character_)
#' # list(func_name = "identity", args = list())
#' }
#'
#' @keywords internal
.parse_transform_dsl <- function(expr) {
  stopifnot(length(expr) == 1L)

  if (is.na(expr) || !nzchar(trimws(expr))) {
    return(list(func_name = "identity", args = list()))
  }

  expr <- trimws(expr)
  marker <- "_func.("

  if (!grepl(marker, expr, fixed = TRUE)) {
    # Bare function name with no arguments
    return(list(func_name = expr, args = list()))
  }

  # Split on the FIRST occurrence of "_func.(" to get func_name and args_str.
  # Using regexpr so we handle names that themselves contain underscores.
  m <- regexpr("_func\\.\\(", expr, perl = TRUE)
  func_name <- substr(expr, 1L, m - 1L)

  # Everything after "_func.(" up to (but not including) the closing ")"
  rest      <- substr(expr, m + attr(m, "match.length"), nchar(expr))
  # Strip the trailing ")"  --  it must be the last character
  if (substr(rest, nchar(rest), nchar(rest)) == ")") {
    args_str <- substr(rest, 1L, nchar(rest) - 1L)
  } else {
    # Malformed: no closing paren.  Return what we have and warn.
    warning(sprintf(
      ".parse_transform_dsl: no closing ')' in %s  --  treating tail as args",
      dQuote(expr)
    ))
    args_str <- rest
  }

  if (!nzchar(trimws(args_str))) {
    args <- list()
  } else {
    args <- as.list(trimws(strsplit(args_str, ",", fixed = TRUE)[[1L]]))
  }

  list(func_name = func_name, args = args)
}


#' Vectorised wrapper: parse a character vector of transform DSL strings
#'
#' Applies [.parse_transform_dsl()] to each element of `exprs` and returns a
#' list of parsed results (same length as `exprs`).
#'
#' @param exprs Character vector of transform DSL strings (NAs allowed).
#' @return List of parse results, each a list with `func_name` and `args`.
#' @keywords internal
.parse_transform_dsl_vec <- function(exprs) {
  lapply(exprs, .parse_transform_dsl)
}


# -----------------------------------------------------------------------------
# 2. Brace-list DSL
# -----------------------------------------------------------------------------

#' Parse a brace-list string into a character vector
#'
#' Parses brace-encoded strings (e.g. \code{"\{Male\}\{Female\}"}) into a
#' character vector (e.g. \code{c("Male", "Female")}).
#' \code{NA} returns \code{character(0)}. Non-brace strings raise an error
#' unless \code{allow_bare} is \code{TRUE}.
#'
#' @param s character(1)  --  the brace-list string to parse
#' @param allow_bare logical  --  if TRUE, plain strings are returned as-is
#' @return character vector
#' @keywords internal
.parse_braces <- function(s, allow_bare = FALSE) {
  stopifnot(length(s) == 1L)

  if (is.na(s)) return(character(0L))

  s <- as.character(s)

  if (!startsWith(s, "{")) {
    if (allow_bare) return(s)
    stop(sprintf(
      ".parse_braces: expected string starting with '{', got %s",
      dQuote(s)
    ))
  }

  # Strip outer { and } then split on }{
  inner <- gsub("^\\{(.+)\\}$", "\\1", s, perl = TRUE)
  strsplit(inner, "\\}\\{", perl = TRUE)[[1L]]
}


#' Vectorised wrapper: parse a character vector of brace-list strings
#'
#' @param sv Character vector (NAs allowed).
#' @param allow_bare Passed to [.parse_braces()].
#' @return A list of character vectors, same length as `sv`.
#' @keywords internal
.parse_braces_vec <- function(sv, allow_bare = FALSE) {
  lapply(sv, .parse_braces, allow_bare = allow_bare)
}
