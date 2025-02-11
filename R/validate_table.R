#' Report pass/fail status
#' @title report_passfail
#' @description Report the pass/fail status
#' @param var character, the variable name
#' @param func_str character, the name of the test function
#' @param message character, the message to report
#' @param max_len integer, the maximum length of the printed output
#' @export
report_passfail <- function(var, func_str, message, max_len = 25) {
  msg <- paste(
    paste0(var, strrep(" ", max_len - nchar(var))),
    paste0(func_str, strrep(" ", max_len - nchar(func_str))),
    message,
    sep = " | "
  )
  message(msg)
}


#' Validate a data.table against a schema
#' Schema
#' @title validate_table
#' @description Validate a data.table against a schema
#' @param dt data.table, the data.table to validate
#' @param schema list, a list of schema values for the table
#' @param trim logical, whether to trim the columns to only those in the schema
#' @param ... additional locals arguments to pass to the test functions, e.g. tables_list
#' @return data.table
#' @export
validate_table <- function(dt, schema, trim = FALSE, ...) {
  # Copy the data.table to a working copy
  valid_dt <- copy(dt)

  # Get max name length
  max_len <- max(
    c(
      nchar(names(schema)),
      nchar(sapply(schema, function(x) x[["tests"]]))
    )
  )

  # Loop through each variable in the schema
  valid_fields <- list()
  keep_cols <- c()
  for (var in names(schema)) {
    # Check if the variable exists in the data.table
    col <- grep(
      paste0("^", var, "$"),
      names(valid_dt),
      value = TRUE
    )

    # Get the schema values
    field_schema <- schema[[var]]
    required <- as.logical(field_schema[["required"]])


    if (!is.list(field_schema[["tests"]])) {
      field_schema[["tests"]] <- list(field_schema[["tests"]])
    }

    # Collapse tests into string label
    tests_str <- paste(
      lapply(field_schema[["tests"]], function(test) {
        if (is.function(test)) {
          test <- "user defined function"
        }
        return(test)
      }),
      collapse = ", "
    )

    if (length(col) == 0) {
      valid_fields[[var]] <- FALSE | !required
      msg <- if (required) {
        "Required field not found in table!"
      } else {
        "passed. Optional field not found in table."
      }
      report_passfail(var, tests_str, msg, max_len)
      next
    }
    keep_cols <- c(keep_cols, col)

    # Run the test function and catch any errors
    valid_fields[[var]] <- tryCatch(
      {
        test_results <- validate_field(valid_dt, col, field_schema, ...)
        msg <- if (test_results) "passed." else ">>> failed validation function."
        report_passfail(var, tests_str, msg, max_len)
        test_results
      },
      warning = function(w) {
        report_passfail(var, tests_str, paste(">>>", w$message), max_len)
        return(FALSE)
      },
      error = function(e) {
        report_passfail(var, tests_str, paste(">>>", e$message), max_len)
        return(FALSE)
      }
    )
  }

  # Check all outcomes
  stopifnot(all(unlist(valid_fields) == TRUE))

  # Trim columns if requested
  if (trim) {
    valid_dt <- valid_dt[, keep_cols, with = FALSE]
    return(valid_dt)
  }
}
