#' Report pass/fail status
#' @title report_passfail
#' @description Report the pass/fail status
#' @param var character, the variable name
#' @param func_str character, the name of the test function
#' @param message character, the message to report
#' @param max_len integer, the maximum length of the printed output
#' @export
report_passfail = function(var, func_str, message, max_len = 25) {
    msg = paste(
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
#' @param table_name character, the name of the data.table to validate
#' @param tables_list list, a named list of data.tables to validate
#' @param schemas_list list, a named list of schema to validate against
#' @param trim logical, whether to trim the columns to only those in the schema
#' @return data.table
#' @export
validate_table = function(table_name, tables_list, schemas_list, trim = FALSE) {
    
    # Copy the data.table to a working copy
    valid_dt = copy(tables_list[[table_name]])
    schema = schemas_list[[table_name]]

    # Get max name length
    max_len = max(
        c(
            nchar(names(schema)),
            nchar(sapply(schema, function(x) x[['tests']]))
        )
    )

    # Loop through each variable in the schema
    valid_fields = list()
    for (var in names(schema)) {
        # Check if the variable exists in the data.table
        col = grep(
            paste0("^", var, "$"),
            names(tables_list[[table_name]]),
            value = TRUE
        )

        # Get the schema values
        field_schema = schema[[var]]
        required = as.logical(field_schema[['required']])
        tests_str = paste(field_schema[['tests']], collapse = ", ")

        if (length(col) == 0) {
            valid_fields[[var]] = FALSE | !required
            msg = if (required) {
                "Required field not found in table!"
            } else {
                "passed. Optional field not found in table."
            }
            report_passfail(var, tests_str, msg, max_len)
            next
        }

        # Run the test function and catch any errors
        valid_fields[[var]] = tryCatch(
            {
                test_results = validate_field(valid_dt, col, field_schema, tables_list)
                msg = if (test_results) "passed." else ">>> failed validation function."
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
        valid_dt = valid_dt[, names(schema), with = FALSE]
    }

    return(valid_dt)
}
