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

    return(NULL)

}
