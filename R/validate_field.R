#' Validate field
#' @title validate_field
#' @description Validate a field against a schema
#' @param dt data.table, the data.table to validate
#' @param col character, the column to validate
#' @param func_str character, the name of the test function to run
#' @param locals list, a list of local variables to pass to the test function
#' @return logical
#' @export
validate_field = function(dt, col, func_str, locals) {

    # Run the test function
    if (is.na(func_str)) {
        warning(str_glue("No test function found for {var}. Skipping."))
        func = function(x) TRUE
    } else {
        # Get the test function and args
        func = get(func_str)
        args = formalArgs(get(func_str))
        
        if (length(args) == 1 && args == "x") {
            # If the function does not have args, assume simple vector input
            test_results = func(dt[[col]])
        } else {
            # Otherwise, pass the locals list to the function
            test_results = do.call(func, list(
                dt = dt,
                col = col,
                locals = locals
            ))
        }

    }

    return(test_results)
}
