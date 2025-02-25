
#' Collate the pointblank tests from the schema
#' @param schema list, a list of schema values for the table
#' @return list
#' @export
collate_tests = function(schema) { # nolint: cyclocomp_linter.

    # Collate pointblank tests into a list of tests and their columns
    pb_tests = list()
    custom_tests = copy(schema)
    for (var in names(schema)) {
        tests = schema[[var]]$tests

        if (is.function(tests)) next
        if (!is.list(tests)) tests = as.list(tests)

        for (test in tests) {

            if (is.function(test)) next

            # Parse the test function arguments
            args_list = parse_test_args(test)

            # Remove the () from the test string
            test_str = sub("\\(.*\\)", "", test)

            # Default to the parent namespace.
            if (grepl('::', test_str)) {
                # If explicit package is provided
                params = tstrsplit(test_str, "::")
                test_str = params[[2]]
                namespace = params[[1]]
            } else {
                namespace = 'base'
            }

            # Check if in pointblank namespace
            if (
                namespace == 'pointblank' ||
                test_str %in% ls(asNamespace("pointblank"))
            ) {
                pb_tests[[test]] = list(
                    fun = get(test_str, envir = asNamespace("pointblank")),
                    args = args_list,
                    columns = c(pb_tests[[test]]$columns, var)
                )
                # Drop the test from the custom tests schema, leaving only custom tests
                custom_tests[[var]]$tests = setdiff(custom_tests[[var]]$tests, test)
            }
        }

        # If no tests left, function is simple true
        if (length(custom_tests[[var]]$tests) == 0) {
            custom_tests[[var]] = NULL
        }
    }

    return(list(
        pb_tests = pb_tests,
        custom_tests = custom_tests
    ))
}

#' Parse test function arguments
#' @title parse_test_args
#' @description Parse test function arguments
#' @param test_str character, the test function string
#' @return list
#' @export
parse_test_args = function(test_str) {
    # Check if function has args ()
    if (grepl("\\(", test_str)) {
        # Parse function and args from test function string
        args_str = sub("^[^(]*\\((.*)\\)$", "\\1", test_str)
        args_list = eval(parse(text = paste0("list(", args_str, ")")))

        # If args list missing names, stop
        if (is.null(names(args_list))) {
            stop("Missing argument names. Function arguments must be named in the schema.")
        }

    } else {
        args_list = NULL
    }

    return(args_list)
}

#' Parse regex column names
#' @title parse_regex_cols
#' @description Parse regex column names
#' @param columns vector, the columns to validate
#' @param dt_cols vector, the columns in the data.table
#' @return vector
#' @export
parse_regex_cols = function(columns, dt_cols) {
    # Parse regex column names
    regexed_cols = c()
    for (col in columns) {
        regexed_cols = c(
            regexed_cols,
            grep(
                paste0("^", col, "$"),
                dt_cols,
                value = TRUE
            )
        )
    }

    regexed_cols = intersect(regexed_cols, dt_cols)

    return(regexed_cols)
}


#' Run pointblank tests
#' @param valid_dt data.table, the data.table to validate
#' @param pb_tests list, a list of pointblank tests
#' @return agent
#' @export
run_pbtests = function(valid_dt, pb_tests) {
    if (length(pb_tests) > 0) {
        # Initialize the agent
        # al = pointblank::action_levels(warn_at = 1)
        agent = pointblank::create_agent(valid_dt)

        # Add the tests to the agent
        for (test in names(pb_tests)) {
            test_fn = pb_tests[[test]]$fun
            columns = parse_regex_cols(pb_tests[[test]]$columns, names(valid_dt))

            if (length(columns) == 0) {
                next
            }
            args = list(
                agent,
                columns = columns
            )

            if (!is.null(pb_tests[[test]]$args)) {
                args = c(args, pb_tests[[test]]$args)
            }

            agent = do.call(test_fn, args)
        }

        # Run the tests
        agent = pointblank::interrogate(agent, progress = FALSE)
        return(agent)
    } else {
        return(NULL)
    }
}

#' Run custom tests
#' @title run_custom_tests
#' @description Run custom tests
#' @param valid_dt data.table, the data.table to validate
#' @param custom_tests list, a list of custom tests
#' @param ... additional keyword arguments, i.e., locals list with data.tables
#' @return NULL
#' @export
run_custom_tests = function(valid_dt, custom_tests, ...) {
    # Get the maximum length of the variable names
    if (length(names(custom_tests)) == 0) {
        return(NULL)
    }

    # Run custom tests
    message("==== Running custom tests ====")
    
    max_len = max(
        c(
            nchar(names(custom_tests)),
            nchar(sapply(custom_tests, function(x) {
                res = ifelse(is.function(x[['tests']]), "user defined function", x[['tests']])
                return(res)
            }))
        )
    )

    # Loop through each variable in the schema
    valid_fields = list()
    keep_cols = c()
    for (var in names(custom_tests)) {
        # Check if the variable exists in the data.table
        col = grep(
            paste0("^", var, "$"),
            names(valid_dt),
            value = TRUE
        )

        # Get the schema values
        field_schema = custom_tests[[var]]
        required = as.logical(field_schema[['required']])
        
        if (!is.list(field_schema[['tests']])) {
            field_schema[['tests']] = list(field_schema[['tests']])
        }

        # Collapse tests into string label
        tests_str = paste(
            lapply(field_schema[['tests']], function(test) {
                if (is.function(test)) {
                    test = "user defined function"
                }
                return(test)
            }),
            collapse = ", "
        )

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
        keep_cols = c(keep_cols, col)

        # Run the test function and catch any errors
        valid_fields[[var]] = tryCatch(
            {
                test_results = validate_field(valid_dt, col, field_schema, ...)
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

    return(NULL)
}