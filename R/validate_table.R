#' Validate a data.table against a schema
#' Schema
#' @title validate_table
#' @description Validate a data.table against a schema
#' @param table_name character, the name of the data.table to validate
#' @param tables_list list, a named list of data.tables to validate
#' @param schema list, a named list of schema to validate against
#' @param trim logical, whether to trim the columns to only those in the schema
#' @return data.table
#' @export
validate_table = function(table_name, tables_list, schema, trim = FALSE) {
    
    # Get max name length
    max_var_len = max(nchar(names(schema)))
    max_func_len = max(nchar(sapply(schema, function(x) x[['tests']])))

    # Copy the data.table to a working copy
    valid_dt = copy(tables_list[[table_name]])

    # Internal function to report pass/fail
    # Uses max_var_len and max_func_len from parent function scope
    report_passfail = function(var, func_str, message) {
        msg = paste(
            paste0(var, strrep(" ", max_var_len - nchar(var))),
            paste0(func_str, strrep(" ", max_func_len - nchar(func_str))),
            message,
            sep = " | "
        )
        message(msg)
    }

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
        tests = field_schema[['tests']]

        # For each test on field, run the test function
        for (func_str in tests) {
            required = as.logical(field_schema[['required']])
            coerce = as.logical(field_schema[['coerce']])
            data_type = field_schema[['data_type']]

            if (length(col) == 0) {
                valid_fields[[var]] = FALSE | !required
                msg = if (required) {
                    ">>> Required field not found in table!"
                } else {
                    "passed. Optional field not found in table."
                }
                report_passfail(var, func_str, msg)
                next
            }

            # Run the test function and catch any errors
            valid_fields[[var]] = tryCatch(
                {
                    # Coerce or check the data type
                    if (coerce) {
                        valid_dt[[col]] = as(valid_dt[[col]], data_type)
                    } else {
                        stopifnot(class(valid_dt[[col]]) == data_type)
                    }
                    test_results = validate_field(valid_dt, col, func_str, tables_list)
                    msg = if (test_results) "passed." else ">>> failed validation function."
                    report_passfail(var, func_str, msg)
                    test_results
                },
                warning = function(w) {
                    report_passfail(var, func_str, paste(">>>", w$message))
                    return(FALSE)
                },
                error = function(e) {
                    report_passfail(var, func_str, paste(">>>", e$message))
                    return(FALSE)
                }
            )
        }
    }

    # Check all outcomes
    stopifnot(all(unlist(valid_fields) == TRUE))

    # Trim columns if requested
    if (trim) {
        valid_dt = valid_dt[, names(schema), with = FALSE]
    }

    return(valid_dt)
}
