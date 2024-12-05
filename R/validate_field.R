#' Validate field
#' @title validate_field
#' @description Validate a field against a schema
#' @param dt data.table, the data.table to validate
#' @param col character, the column to validate
#' @param field_schema list, a list of schema values for the field
#' @param ... additional locals arguments to pass to the test
#' @import methods
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

    # For each test on field, run the test function
    for (func_str in tests) {
        
        if (is.na(func_str)) {
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
                    ... # Pass locals to the test function
                ))
            }

        }
    }

    return(test_results)
}
