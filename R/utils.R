# =============================================================================
# Internal utilities shared across the package
# =============================================================================


#' Assert that a data.frame has all required columns
#'
#' @param df    A data.frame.
#' @param cols  Character vector of required column names.
#' @param label Informative label for error messages (e.g. `"col_info"`).
#' @keywords internal
.assert_cols <- function(df, cols, label = deparse(substitute(df))) {
  missing <- setdiff(cols, colnames(df))
  if (length(missing)) {
    stop(sprintf(
      "%s is missing required column(s): %s",
      label,
      paste(dQuote(missing), collapse = ", ")
    ))
  }
  invisible(df)
}


#' Coerce a value to integer, raising an informative error on failure
#' @keywords internal
.as_int <- function(x, label = deparse(substitute(x))) {
  out <- suppressWarnings(as.integer(x))
  if (anyNA(out) && !anyNA(x)) {
    stop(sprintf("Cannot coerce %s to integer: %s", label, paste(x, collapse = ", ")))
  }
  out
}


#' Return rows of a data.frame where a column is non-NA
#' @keywords internal
.non_na_rows <- function(df, col) df[!is.na(df[[col]]), , drop = FALSE]


#' Split a `{A}{B}{C}` brace-string; thin convenience wrapper used in loops
#' where NA should silently yield `character(0)`.
#' @keywords internal
.brace_split <- function(s) {
  if (is.na(s) || !nzchar(s)) return(character(0L))
  .parse_braces(s, allow_bare = FALSE)
}
