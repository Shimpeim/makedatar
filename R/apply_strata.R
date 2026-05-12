# md_apply_strata()  --  filter rows to allowed strata values
#
# Processes the `$strata_in` sub-table from md_extract_var_roles().
# Each row specifies a column and a brace-list of allowed values.
# Rows where the column value is NOT in the allowed set are dropped.
# Multiple strata_in rows are applied sequentially (AND logic).


#' Filter data rows to allowed strata values
#'
#' @export
#' @param data        data.frame  --  the clinical data
#' @param strata_rows data.frame  --  the `$strata_in` sub-table from
#'                    md_extract_var_roles(); must have columns
#'                    `col_name` and `strata_in`
#' @return  data with non-matching rows removed; columns are unchanged
md_apply_strata <- function(data, strata_rows) {
  if (nrow(strata_rows) == 0L) return(data)

  for (i in seq_len(nrow(strata_rows))) {
    col_name  <- strata_rows$col_name[i]
    value_dsl <- strata_rows$strata_in[i]

    if (is.na(value_dsl))               next
    if (!col_name %in% colnames(data))  next

    allowed <- .parse_braces(value_dsl)
    keep    <- as.character(data[[col_name]]) %in% allowed
    data    <- data[keep, , drop = FALSE]
  }

  rownames(data) <- NULL
  data
}
