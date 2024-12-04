#' Load table schema from CSV.
#' @title load_schema
#' @description Load a table schema from a CSV file and format it schema list.
#' @param path path to CSV file
#' @return list
#' @export
load_schema = function(path) {

    message("Loading schema ", basename(path))

    # Read the schema table
    schema_table = data.table::fread(path)

    # Transpose the table
    key_names = names(schema_table)['column_name' != names(schema_table)]
    schema_t = data.table::transpose(schema_table, make.names = 'column_name')
    
    # Convert to list and parse any test lists into a sub-list
    schema = as.list(schema_t)
    for (var in names(schema)) {
        # Convert to list to allow for multiple tests
        schema[[var]] = as.list(schema[[var]])
        names(schema[[var]]) = key_names

        # Parse the tests into a list if multiple
        schema[[var]][['tests']] = trimws(data.table::tstrsplit(schema[[var]][['tests']], "&"))
    } 

    return(schema)
}
