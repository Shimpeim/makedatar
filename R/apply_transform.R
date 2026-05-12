# md_apply_transform()  --  dispatch the transform step
#
# For each row in the `$transform` sub-table (from md_extract_var_roles()),
# parses the DSL string, resolves the `var.ID` sentinel, calls the
# corresponding mf.* function, and stores the result as a new column.
#
# Calling convention for mf.* functions (see trans_functions.R):
#   mf.func(data, <positional DSL args>, ..., id_cols = <id col names>)
#
# `var.ID` in DSL args is a sentinel for the id_cols vector.  It is REMOVED
# from the positional args list and id_cols is always injected as a named arg.


# --- Public API --------------------------------------------------------------

#' Apply DSL-driven column transforms to a data.frame
#'
#' @export
#' @param data         data.frame  --  the imported clinical data
#' @param transform_rows  data.frame  --  the `$transform` element returned by
#'                     [md_extract_var_roles()]; must have columns `col_name`
#'                     and `transform`
#' @param id_cols      character vector  --  names of ID columns (subject, visit,
#'                     site...); passed as the `id_cols` argument to every mf.*
#'                     function
#' @param pkg_env      environment in which to look up mf.* functions
#'                     (default: the makedatar namespace)
#' @return  data with new / updated columns; rows are never added or removed
md_apply_transform <- function(data,
                                transform_rows,
                                id_cols  = character(0),
                                pkg_env  = getNamespace("makedatar")) {
  if (nrow(transform_rows) == 0L) return(data)

  for (i in seq_len(nrow(transform_rows))) {
    col_name <- transform_rows$col_name[i]
    dsl_str  <- transform_rows$transform[i]

    parsed   <- .parse_transform_dsl(dsl_str)
    func_name <- parsed$func_name
    raw_args  <- parsed$args          # list of character strings

    # Resolve tokens and build positional arg list
    pos_args <- .resolve_dsl_args(raw_args, id_cols)

    # Look up function (error if not found)
    if (!existsFunction(func_name, where = pkg_env)) {
      stop(sprintf(
        "md_apply_transform: function '%s' not found (col '%s', DSL: '%s')",
        func_name, col_name, dsl_str
      ))
    }
    f <- get(func_name, envir = pkg_env)

    # Dispatch: data always first (named); positional DSL args follow;
    # id_cols always appended as a named arg.
    call_args <- c(list(data = data), pos_args, list(id_cols = id_cols))
    tryCatch({
      result           <- do.call(f, call_args)
      data[[col_name]] <- result
    }, error = function(e) {
      stop(sprintf(
        "md_apply_transform: error in '%s' for col '%s': %s",
        func_name, col_name, conditionMessage(e)
      ))
    })
  }

  data
}


# --- Private helpers ---------------------------------------------------------

# .resolve_dsl_args()
#
# Takes the raw positional arg list from .parse_transform_dsl() and:
#   - removes the "var.ID" sentinel (id_cols is injected as a named arg)
#   - keeps all other string args as-is
#
# Returns a list suitable for splicing into do.call() args.
.resolve_dsl_args <- function(raw_args, id_cols) {
  # raw_args is a list of character strings (or empty list for identity)
  resolved <- lapply(raw_args, function(a) {
    if (identical(a, "var.ID")) NULL else a
  })
  # Drop NULL entries (var.ID sentinels)
  Filter(Negate(is.null), resolved)
}


# existsFunction()  --  safe check for a function name in an environment
existsFunction <- function(name, where) {
  exists(name, envir = where, inherits = FALSE) &&
    is.function(get(name, envir = where, inherits = FALSE))
}
