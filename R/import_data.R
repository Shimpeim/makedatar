# =============================================================================
# md_import_data()
#
# Reads the "data" sheet of the project Excel workbook using the col_info
# schema table to control column names and types, then returns a sorted
# data.frame with only the non-skip columns.
#
# Original makedata_01.R logic:
#   read_excel(... skip=1, na="*",
#              col_names = col_info[!is.na(orig_name), "col_name"],
#              col_types = col_info[!is.na(orig_name), "col_type"])
#   followed by a varseq-ordered column select that also drops any
#   extra columns (e.g. the .y suffix columns added by merge).
# =============================================================================


#' Import the data sheet using a col_info schema
#'
#' @export
#' @importFrom readxl read_excel
#' @param path     Path to the Excel workbook.
#' @param col_info A `data.frame` returned by md_read_col_info().
#' @param sheet    Sheet name or index for the data sheet (default `"data"`).
#' @param skip     Number of rows to skip before reading (default `1`).
#'   The first skipped row is typically the original EDC column-name row
#'   exported before the renamed headers.
#' @param na       String(s) to interpret as `NA` (default `"*"`).
#'
#' @return A `data.frame` with columns named according to `col_info$col_name`,
#'   restricted to rows where `col_info$orig_name` is non-`NA` and
#'   `col_info$col_type` is not `"skip"`, ordered by `col_info$varseq`.
#'
#' @details
#' `col_type == "skip"` rows are recognised by `readxl::read_excel()` and are
#' never materialised in the result.  The column ordering step replicates the
#' `order(varseq)` select from the original script, ensuring the output column
#' sequence always matches the schema regardless of how readxl orders them
#' internally.
#'
#' @examples
#' \dontrun{
#' col_info <- md_read_col_info("workbook.xlsx")
#' data     <- md_import_data("workbook.xlsx", col_info)
#' }
#'
#' @export
md_import_data <- function(path, col_info,
                            sheet = "data",
                            skip  = 1L,
                            na    = "*") {
  if (!file.exists(path)) {
    stop(sprintf("md_import_data: file not found: %s", path))
  }
  .assert_cols(col_info, .COL_INFO_REQUIRED, label = "col_info")

  spec <- .import_col_spec(col_info)

  raw <- tryCatch(
    readxl::read_excel(
      path      = path,
      sheet     = sheet,
      skip      = as.integer(skip),
      na        = na,
      col_names = spec$col_names,
      col_types = spec$col_types
    ),
    error = function(e) stop(sprintf(
      "md_import_data: cannot read sheet %s from %s\n  %s",
      dQuote(sheet), path, conditionMessage(e)
    ))
  )
  df <- as.data.frame(raw, stringsAsFactors = FALSE)

  # Sort columns by varseq (readxl may return them in file order, not
  # schema order).  Also implicitly drops any columns not in the spec.
  df <- .reorder_by_varseq(df, col_info)

  df
}


# -----------------------------------------------------------------------------
# Private helpers (exported for testability, not for end-users)
# -----------------------------------------------------------------------------

#' Build the col_names / col_types vectors to pass to read_excel
#'
#' Selects rows from col_info where `orig_name` is non-NA.  These are the
#' columns present in the data sheet.  Rows with `col_type == "skip"` are
#' included in the spec because `read_excel()` needs them to correctly align
#' the column-type vector; `read_excel` will not materialise them.
#'
#' @param col_info A validated col_info data.frame.
#' @return Named list with `$col_names` and `$col_types` (character vectors).
#' @keywords internal
.import_col_spec <- function(col_info) {
  import_rows <- !is.na(col_info[["orig_name"]])
  list(
    col_names = col_info[import_rows, "col_name"],
    col_types = col_info[import_rows, "col_type"]
  )
}


#' Re-order the columns of a freshly imported data.frame by varseq
#'
#' Selects only columns that appear in col_info with `orig_name` non-NA and
#' `col_type != "skip"`, in ascending `varseq` order.  Any extra columns in
#' `df` (e.g. `.y` suffix columns from a prior `merge()`) are silently dropped.
#'
#' @param df       data.frame to reorder.
#' @param col_info Validated col_info data.frame.
#' @return Reordered data.frame.
#' @keywords internal
.reorder_by_varseq <- function(df, col_info) {
  keep <- !is.na(col_info[["orig_name"]]) &
    !.eq_safe(col_info[["col_type"]], "skip")
  spec <- col_info[keep, c("col_name", "varseq"), drop = FALSE]
  spec <- spec[order(spec[["varseq"]]), , drop = FALSE]

  # Only select columns that are actually present (guard for partial imports)
  ordered_cols <- intersect(spec[["col_name"]], colnames(df))
  df[, ordered_cols, drop = FALSE]
}
