# =============================================================================
# md_fill_demographics()
#
# Propagates per-patient constant fields (birth_date, gender, height, weight)
# across all visit rows for the same patient, then fills date columns that are
# NA from a fallback column.
#
# Original makedata_01.R logic (two distinct operations):
#
# 1. Broadcast demographics  --  merge-based approach that created .x/.y columns:
#      df <- merge(df, df[!is.na(df$birth_date), c('study_id','birth_date','gender','height','weight')], by='study_id')
#      df$birth_date.x <- df$birth_date.y   # overwrite with propagated value
#      ...rename birth_date.x -> birth_date...
#      # .y columns later dropped by the varseq column-select step
#
# 2. Fill date from fallback column:
#      df[is.na(df$visit_date), 'visit_date'] <- df[is.na(df$visit_date), 'ic_date']
#
# The package replaces the merge pattern with a direct lookup-and-fill that
# produces no suffix columns and is independent of the downstream column-select.
# =============================================================================


#' Propagate per-patient constant fields and fill fallback date columns
#'
#' @param data     A `data.frame` (returned by [md_import_data()]).
#' @param vars     Character vector of column names to broadcast from non-`NA`
#'   rows to all rows sharing the same patient ID.  Defaults to the four
#'   demographic constants in the original script:
#'   `c("birth_date", "gender", "height", "weight")`.
#'   Columns absent from `data` are silently skipped.
#' @param id_var   Name of the patient identifier column (default `"study_id"`).
#' @param fill_from Named character vector where each **name** is a target
#'   column and each **value** is the fallback source column.  When a target
#'   cell is `NA`, the corresponding source cell is used instead.
#'   Default `c(visit_date = "ic_date")` replicates the original script's
#'   registration-visit date fill.  Set to `NULL` to disable.
#'   Pairs where either column is absent from `data` are silently skipped.
#'
#' @return The modified `data.frame` (same dimensions as `data`; no new columns
#'   are added).
#'
#' @details
#' **Broadcasting logic**  --  for each variable `v` in `vars`:
#' 1. Find rows where `v` is non-`NA`; keep the first non-`NA` value per patient.
#' 2. Use `match()` to map every row's patient ID to that reference value.
#' 3. Replace the entire `v` column with the looked-up values (patients with no
#'    non-`NA` reference row remain `NA`).
#'
#' This is functionally equivalent to the original `merge()` approach but does
#' not create `.x`/`.y` suffix columns.
#'
#' @examples
#' # Default usage  --  broadcasts birth_date, gender, height, weight and fills
#' # visit_date from ic_date:
#' \dontrun{
#' data <- md_fill_demographics(data)
#' }
#'
#' # Custom: broadcast only one var, no date fill:
#' \dontrun{
#' data <- md_fill_demographics(data, vars = "birth_date", fill_from = NULL)
#' }
#'
#' @export
md_fill_demographics <- function(
    data,
    vars      = c("birth_date", "gender", "height", "weight"),
    id_var    = "study_id",
    fill_from = c(visit_date = "ic_date")) {

  if (!id_var %in% colnames(data)) {
    stop(sprintf(
      "md_fill_demographics: id_var %s not found in data", dQuote(id_var)
    ))
  }

  # --- 1. Broadcast per-patient constants -----------------------------------
  for (v in vars) {
    if (!v %in% colnames(data)) next

    ref <- data[!is.na(data[[v]]), c(id_var, v), drop = FALSE]

    if (nrow(ref) == 0L) next  # variable is entirely NA  --  nothing to do

    # One reference row per patient (first non-NA wins)
    ref <- ref[!duplicated(ref[[id_var]]), , drop = FALSE]

    # Broadcast: map every row's patient ID to the reference value
    idx        <- match(data[[id_var]], ref[[id_var]])
    data[[v]]  <- ref[[v]][idx]
  }

  # --- 2. Fill date columns from fallback ----------------------------------
  if (!is.null(fill_from)) {
    for (i in seq_along(fill_from)) {
      target <- names(fill_from)[[i]]
      source <- fill_from[[i]]

      if (!target %in% colnames(data)) next
      if (!source %in% colnames(data)) next

      na_mask           <- is.na(data[[target]])
      data[[target]][na_mask] <- data[[source]][na_mask]
    }
  }

  data
}
