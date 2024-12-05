#' Validate field
#' @title validate_field
#' @description Validate a field against a schema
#' @param dt data.table, the data.table to validate
#' @param col character, the column to validate
#' @param field_schema list, a list of schema values for the field
#' @param ... additional locals arguments to pass to the test
#' @return logical
#' @export
validate_field = function(dt, col, field_schema, ...) {

    # Get the schema values
    required = as.logical(field_schema[['required']])
    coerce = as.logical(field_schema[['coerce']])
    data_type = field_schema[['data_type']]
    tests = field_schema[['tests']]

    # Coerce or check the data type
    if (coerce) {
        dt[[col]] = as(dt[[col]], data_type)
    } else {
        stopifnot(class(dt[[col]]) == data_type)
    }


    # If tests is a function itself, pass as a list
    if (!is.list(tests)) {
        tests = list(tests)
    }

    # For each test on field, run the test function
    for (func_str in tests) {
        
        if (!is.function(func_str) && is.na(func_str)) {
            warning(paste0("No test function found for ", col, ". Skipping."))
            func = function(x) TRUE
        } else if (length(col) == 0) {
            msg = if (required) {
                "Required field not found in table!"
            } else {
                "passed. Optional field not found in table."
            }
            warning(msg)
            func = function(x) FALSE | !required
        } else {
            test_results = eval_func(func_str, dt, col, ...)
        }
    }

    return(test_results)
}


#' Evaluate test function
#' @title eval_test
#' @description Evaluate a test function
#' @param func_str character, the function name or function itself
#' @param dt data.table, the data.table to validate
#' @param col character, the column to validate
#' @param ... additional locals arguments to pass to the test
#' @return logical
#' @import data.table
#' @import methods
#' @export
eval_func = function(func_str, dt, col, ...) {

    # Get the test function and args
    func = if (is.function(func_str)) {
        func_str
    } else {
        get(func_str)
    }
    args = formalArgs(func)
    
    if (length(args) == 1 && args == "x") {
        # If the function does not have args, assume simple vector input
        test_results = func(dt[[col]])
    } else {
        # Otherwise, pass the locals list to the function
        test_results = do.call(func, list(
            dt = dt,
            col = col,
            ... # Pass locals to the test function
        ))
    }

    return(test_results)
}