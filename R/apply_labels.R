# md_apply_labels()  --  encode columns as ordered/unordered factors
#
# Processes the `$label` sub-table from md_extract_var_roles().
# Each row describes one variable that has a var.label annotation.
#
# var.level (brace-list or NA):
#   non-NA -> parse as the factor LEVELS  (internal codes)
#   NA      -> infer unique sorted values from the column as levels
#
# var.label (brace-list):
#   human-readable LABELS corresponding to each level
#
# Both orig (imported) and derived (transform/cutoff output) columns are
# handled identically; the caller passes whichever sub-table is appropriate.


#' Apply factor encoding to data columns using var.level / var.label metadata
#'
#' @export
#' @param data        data.frame  --  the clinical data
#' @param label_rows  data.frame  --  the `$label` (or `$label_orig` /
#'                    `$label_derived`) sub-table from md_extract_var_roles();
#'                    must have columns `col_name`, `var.level`, `var.label`
#' @return  data with specified columns converted to factor; other columns
#'          and row count are unchanged
md_apply_labels <- function(data, label_rows) {
  if (nrow(label_rows) == 0L) return(data)

  for (i in seq_len(nrow(label_rows))) {
    col_name  <- label_rows$col_name[i]
    level_dsl <- label_rows$var.level[i]
    label_dsl <- label_rows$var.label[i]

    if (is.na(label_dsl)) next
    if (!col_name %in% colnames(data)) next

    labels <- .parse_braces(label_dsl)

    if (is.na(level_dsl)) {
      # Infer levels from the column's unique non-NA values (sorted)
      vals   <- unique(data[[col_name]])
      vals   <- sort(vals[!is.na(vals)])
      levels <- as.character(vals)
    } else {
      levels <- .parse_braces(level_dsl)
    }

    if (length(levels) != length(labels)) {
      stop(sprintf(
        "md_apply_labels: col '%s': %d levels but %d labels",
        col_name, length(levels), length(labels)
      ))
    }

    data[[col_name]] <- factor(data[[col_name]], levels = levels, labels = labels)
  }

  data
}
