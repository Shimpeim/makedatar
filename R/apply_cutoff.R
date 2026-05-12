# md_apply_cutoff()  --  discretise numeric variables via threshold scoring
#
# For each row in `cutoff_rows` (the `$cutoff` element from
# md_extract_var_roles()):
#   - `col_name`   --  name of the new column to create
#   - `transform`  --  name of the SOURCE variable in `data` (NOT a DSL string)
#   - `cutoff`     --  brace-list of R conditions, e.g. "{<40}{>=40}"
#                   each condition is a suffix appended to `x` and evaluated
#   - `var.score`  --  brace-list of numeric scores,  e.g. "{0}{1}"
#
# The result for each row is the WEIGHTED SUM:
#   sum_j ( as.numeric(x satisfies condition_j) * score_j )
#
# NA in the source variable propagates as NA in the result.


#' Apply threshold-based scoring to create discretised columns
#'
#' @export
#' @param data          data.frame  --  the clinical data
#' @param cutoff_rows   data.frame  --  the `$cutoff` sub-table from
#'                      md_extract_var_roles(); must have columns
#'                      `col_name`, `transform`, `cutoff`, `var.score`
#' @return  data with new numeric columns added; rows never added/removed
md_apply_cutoff <- function(data, cutoff_rows) {
  if (nrow(cutoff_rows) == 0L) return(data)

  for (i in seq_len(nrow(cutoff_rows))) {
    col_name   <- cutoff_rows$col_name[i]
    src_var    <- cutoff_rows$transform[i]
    cutoff_dsl <- cutoff_rows$cutoff[i]
    score_dsl  <- cutoff_rows$var.score[i]

    if (is.na(src_var) || is.na(cutoff_dsl) || is.na(score_dsl)) next
    if (!src_var %in% colnames(data)) {
      stop(sprintf(
        "md_apply_cutoff: source variable '%s' not found in data (col '%s')",
        src_var, col_name
      ))
    }

    conditions <- .parse_braces(cutoff_dsl)            # e.g. c("<40", ">=40")
    scores     <- as.numeric(.parse_braces(score_dsl)) # e.g. c(0, 1)

    if (length(conditions) != length(scores)) {
      stop(sprintf(
        "md_apply_cutoff: col '%s': %d conditions but %d scores",
        col_name, length(conditions), length(scores)
      ))
    }

    x      <- as.numeric(data[[src_var]])
    result <- numeric(length(x))

    for (j in seq_along(conditions)) {
      # Evaluate "x<40", "x>=40", "x>=40 & x<60", etc.
      hit    <- eval(parse(text = sprintf("x%s", conditions[j])))
      result <- result + as.numeric(hit) * scores[j]
    }

    result[is.na(x)] <- NA_real_
    data[[col_name]] <- result
  }

  data
}
