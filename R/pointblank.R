
#' Collate the pointblank tests from the schema
#' @param schema list, a list of schema values for the table
#' @return list
#' @import pointblank
#' @export
collate_tests = function(schema) {

    # Collate pointblank tests into a list of tests and their columns
    pb_tests = list()
    custom_tests = copy(schema)
    for (var in names(schema)) {
        tests = schema[[var]]$tests

        for (test in tests) {
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
                    columns = c(pb_tests[[test_str]]$columns, var)
                )
                # Drop the test from the custom tests schema, leaving only custom tests
                custom_tests[[var]]$tests = setdiff(custom_tests[[var]]$tests, test)
            }
        }

        # If no tests left, function is simple true
        if (length(custom_tests[[var]]$tests) == 0) {
            custom_tests[[var]]$tests = function(x) {
                return(TRUE)
            }
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
            stop("Arguments must be named in the schema.")
        }

    } else {
        args_list = NULL
    }

    return(args_list)
}


#' Run pointblank tests
#' @param valid_dt data.table, the data.table to validate
#' @param pb_tests list, a list of pointblank tests
#' @return agent
#' @import pointblank
#' @export
run_pbtests = function(valid_dt, pb_tests) {
    if (length(pb_tests) > 0) {
        message("==== Running pointblank tests ====")
        # Initialize the agent
        # al = pointblank::action_levels(warn_at = 1)
        agent = pointblank::create_agent(valid_dt)

        dt_cols = names(valid_dt)

        # Add the tests to the agent
        for (test in names(pb_tests)) {
            test_fn = pb_tests[[test]]$fun
            columns = intersect(pb_tests[[test]]$columns, dt_cols)

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
        message("==== Pointblank tests complete ====")
        return(agent)
    } else {
        return(NULL)
    }
}