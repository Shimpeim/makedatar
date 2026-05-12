# =============================================================================
# md_read_col_info()
#
# Reads the "colinfo" sheet from the project Excel workbook and returns a
# validated, column-ordered data.frame.
#
# Minimum required columns in the sheet:
#   col_name   --  renamed column name in the output dataset
#   col_type   --  readxl type ("text","numeric","date","logical","skip")
#   varseq     --  integer ordering key
#   orig_name  --  source column name in the "data" sheet (NA for derived vars)
#
# All other role/annotation columns are optional.  If absent from the sheet
# they are added as all-NA columns so that downstream code can always rely on
# their existence.
# =============================================================================

#' Read and validate the col_info schema sheet
#'
#' @export
#' @importFrom readxl read_excel
#' @param path  Path to the Excel workbook.
#' @param sheet Name or index of the col_info sheet (default `"colinfo"`).
#' @param na    String(s) that should be read as `NA` (default `"NA"`).
#'
#' @return A `data.frame` with one row per variable.  Rows are ordered by
#'   `varseq`.  Optional annotation columns that were absent from the sheet are
#'   added as `NA_character_` columns.
#'
#' @details
#' The following columns are **required** and cause an error if absent:
#' `col_name`, `col_type`, `varseq`, `orig_name`.
#'
#' The following columns are **optional**; they are created as all-`NA` columns
#' if missing from the sheet:
#' `col_label`, `ID`, `exposure`, `background`, `outcome`, `Psmodel`,
#' `transform`, `cutoff`, `var.score`, `var.level`, `var.label`,
#' `strata_in`, `timepoint`.
#'
#' @examples
#' \dontrun{
#' col_info <- md_read_col_info("path/to/workbook.xlsx")
#' }
#'
#' @export
md_read_col_info <- function(path, sheet = "colinfo", na = "NA") {
  if (!file.exists(path)) {
    stop(sprintf("md_read_col_info: file not found: %s", path))
  }

  raw <- tryCatch(
    readxl::read_excel(path, sheet = sheet, na = na),
    error = function(e) stop(sprintf(
      "md_read_col_info: cannot read sheet %s from %s\n  %s",
      dQuote(sheet), path, conditionMessage(e)
    ))
  )
  df <- as.data.frame(raw, stringsAsFactors = FALSE)

  # --- Required columns -------------------------------------------------------
  .assert_cols(df, .COL_INFO_REQUIRED, label = "col_info")

  # --- Optional columns: add as NA if absent ----------------------------------
  for (col in .COL_INFO_OPTIONAL) {
    if (!col %in% colnames(df)) {
      df[[col]] <- NA_character_
    }
  }

  # --- Coerce varseq to integer -----------------------------------------------
  df[["varseq"]] <- suppressWarnings(as.integer(df[["varseq"]]))

  # --- Sort by varseq ---------------------------------------------------------
  df <- df[order(df[["varseq"]]), , drop = FALSE]
  rownames(df) <- NULL

  df
}


# --- Column name constants (used in tests too) --------------------------------

#' @keywords internal
.COL_INFO_REQUIRED <- c("col_name", "col_type", "varseq", "orig_name")

#' @keywords internal
.COL_INFO_OPTIONAL <- c(
  "col_label",
  "ID",
  "exposure",
  "background",
  "outcome",
  "Psmodel",
  "transform",
  "cutoff",
  "var.score",
  "var.level",
  "var.label",
  "strata_in",
  "timepoint"
)
