#' Report pass/fail status
#' @title report_passfail
#' @description Report the pass/fail status
#' @param var character, the variable name
#' @param func_str character, the name of the test function
#' @param message character, the message to report
#' @param max_len integer, the maximum length of the printed output
#' @return NULL
#' @export
report_passfail = function(var, func_str, message, max_len = 25) {
    msg = paste(
        paste0(var, strrep(" ", max_len - nchar(var))),
        paste0(func_str, strrep(" ", max_len - nchar(func_str))),
        message,
        sep = " | "
    )
    message(msg)

    return(NULL)
}


#' Validate a data.table against a schema
#' Schema
#' @title validate_table
#' @description Validate a data.table against a schema
#' @param dt data.table, the data.table to validate
#' @param schema list, a list of schema values for the table
#' @param report_path File path to save the pointblank report
#' @param ... additional locals arguments to pass to the test functions, e.g. tables_list
#' @return NULL
#' @export
validate_table = function(dt, schema, report_path = NULL, ...) {
    
    # Copy the data.table to a working copy
    valid_dt = copy(dt)

    # Collate pointblank tests
    collated = collate_tests(schema)
    pb_results = run_pbtests(valid_dt, pb_tests = collated$pb_tests)
    custom_tests = collated$custom_tests
    run_custom_tests(valid_dt, custom_tests, ...)

    # Report the results of the pointblank tests
    if (!is.null(pb_results) && sum(pb_results$validation_set$n_failed) > 0) {

        # Get column name for each step
        fail_idx = which(pb_results$validation_set$n_failed > 0)
        failed_cols = unlist(pb_results$validation_set$column)[fail_idx]
        failed_test = pb_results$validation_set$assertion_type[fail_idx]
        max_testlen = max(sapply(failed_test, function(x) nchar(x)))
        max_collen = max(sapply(failed_cols, function(x) nchar(x)))
        
        msg = "\n==== Pointblank tests failed ===="
        for (i in seq_along(failed_cols)) {
            var = failed_cols[i]
            func_str = failed_test[i]
            row = paste(
                paste0(var, strrep(" ", max_testlen - nchar(var))),
                paste0(func_str, strrep(" ", max_testlen - nchar(func_str))),
                ">>> fail :(",
                sep = " | "
            )
            msg = paste(msg, row, sep = "\n")
        }

        if (!is.null(report_path)) {
            # Create the report directory if it doesn't exist
            report_dir = dirname(report_path)
            if (!dir.exists(report_dir)) {
                dir.create(report_dir, recursive = TRUE)
            }
            msg = paste(msg, "\n", "Saving report to: ", report_path, "\n", sep = "")

            # Save the pointblank report
            pb_results = pointblank::export_report(
                pb_results,
                file = report_path,
                quiet = TRUE,
            )
        }
        
        stop(msg)
    }

    # # Get the maximum length of the variable names
    # if (length(names(custom_tests)) == 0) {
    #     return(NULL)
    # }

    # # Run custom tests
    # message("==== Running custom tests ====")
    
    # max_len = max(
    #     c(
    #         nchar(names(custom_tests)),
    #         nchar(sapply(custom_tests, function(x) {
    #             res = ifelse(is.function(x[['tests']]), "user defined function", x[['tests']])
    #             return(res)
    #         }))
    #     )
    # )

    # # Loop through each variable in the schema
    # valid_fields = list()
    # keep_cols = c()
    # for (var in names(custom_tests)) {
    #     # Check if the variable exists in the data.table
    #     col = grep(
    #         paste0("^", var, "$"),
    #         names(valid_dt),
    #         value = TRUE
    #     )

    #     # Get the schema values
    #     field_schema = custom_tests[[var]]
    #     required = as.logical(field_schema[['required']])
        
    #     if (!is.list(field_schema[['tests']])) {
    #         field_schema[['tests']] = list(field_schema[['tests']])
    #     }

    #     # Collapse tests into string label
    #     tests_str = paste(
    #         lapply(field_schema[['tests']], function(test) {
    #             if (is.function(test)) {
    #                 test = "user defined function"
    #             }
    #             return(test)
    #         }),
    #         collapse = ", "
    #     )

    #     if (length(col) == 0) {
    #         valid_fields[[var]] = FALSE | !required
    #         msg = if (required) {
    #             "Required field not found in table!"
    #         } else {
    #             "passed. Optional field not found in table."
    #         }
    #         report_passfail(var, tests_str, msg, max_len)
    #         next
    #     }
    #     keep_cols = c(keep_cols, col)

    #     # Run the test function and catch any errors
    #     valid_fields[[var]] = tryCatch(
    #         {
    #             test_results = validate_field(valid_dt, col, field_schema, ...)
    #             msg = if (test_results) "passed." else ">>> failed validation function."
    #             report_passfail(var, tests_str, msg, max_len)
    #             test_results
    #         },
    #         warning = function(w) {
    #             report_passfail(var, tests_str, paste(">>>", w$message), max_len)
    #             return(FALSE)
    #         },
    #         error = function(e) {
    #             report_passfail(var, tests_str, paste(">>>", e$message), max_len)
    #             return(FALSE)
    #         }
    #     )
    # }

    # # Check all outcomes
    # stopifnot(all(unlist(valid_fields) == TRUE))

    return(NULL)

}
