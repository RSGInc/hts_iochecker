#' Validate all tables
#' @title validate_all_tables
#' @description Validate all standard tables in the HTS data set.
#' @param tables_list list, a named list of data tables to validate.
#' @param schemas_list list, a named list of schemas to validate against.
#' @param trim logical, whether to trim the columns to only those in the schema.
#'   This functionality might be useful when loading and validating
#'   the data on the fly.
#' @return TRUE if all tables pass validation
#' @export
validate_all_tables = function(
    tables_list,
    schemas_list,
    report_dir = NULL
    ) {

    # Assert that table list is named
    if (is.null(names(tables_list))) {
        stop("Table list must be named.")
    }

    # Assert that schema list is named
    if (is.null(names(schemas_list))) {
        stop("Schema list must be named.")
    }

    # Assert that all tables have a schema
    if (!all(names(tables_list) %in% names(schemas_list))) {
        stop("All tables must have a schema.")
    }

    # Create report directory if it doesn't exist
    if (!is.null(report_dir)) {
        if (!dir.exists(report_dir)) {
            dir.create(report_dir, recursive = TRUE)
        }
    }

    # Validate each table
    valid_tables = list()
    for (table_name in names(tables_list)) {

        if (all(c('sf', 'data.frame') %in% class(tables_list[[table_name]]))) {
            valid_tables[[table_name]] = tables_list[[table_name]]
            message("\u1F7E0 Table is an SF object, skipping: ", table_name)
            next
        }

        report_path = if (!is.null(report_dir)) {
            file.path(report_dir, paste0("failure_report_", table_name, ".html"))
        } else {
            NULL
        }

        valid_tables[[table_name]] = tryCatch(
            {
                valid_dt = validate_table(
                    dt = tables_list[[table_name]],
                    schema = schemas_list[[table_name]],
                    report_path = report_path,
                    # Pass locals to the test functions
                    tables_list = tables_list,
                    schemas_list = schemas_list
                )
                message("\u2705 Validation passed for table: ", table_name)
                valid_dt
            },
            warning = function(w) {
                message("\u1F7E0 Warning for table: ", table_name, " ", w$message)
                return(FALSE)
            },
            error = function(e) {
                message("\u274C Validation failed for table: ", table_name, " ", e$message)
                return(FALSE)
            }
        )

    }

    all_pass = sapply(valid_tables, function(x) if (is.logical(x)) x else TRUE)
    fails = paste(names(all_pass)[!all_pass], collapse = ", ")

    if (!all(all_pass)) {
        stop("Validation failed on tables: ", fails)
    } else {
        message("\u2705 All tables passed validation!")
    }

    return(TRUE)
}
