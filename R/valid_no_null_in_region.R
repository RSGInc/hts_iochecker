# This is a library of data check functions that can be used to validate data.

#' Validate home geo columns
#' @param dt data.table
#' @param col Target column name
#' @param ... Additional keyword arguments, i.e., locals list with data.tables
#' @return Logical
#' @import data.table
#' @export
valid_no_null_in_region = function(dt, col, ...) {

    # Only check for households in the region
    x = dt[get("home_in_region") == 1, get(col)]

    checks = c(
        !any(is.na(x)),
        !any(trimws(x) == "")
    )

    if (!all(checks)) {
        return(FALSE)
    }

    return(TRUE)        
}

#' Validate no NULL bg columns
#' @title valid_sample_home_bg
#' @description Validate that there are no NULL values in both
#'  sample_home_bg and home_bg_20xx columns, except for households that
#'  are not in the region.
#' @param dt data.table
#' @param col Target column name
#' @param ... Additional keyword arguments, i.e., locals list with data.tables
#' @return Logical
#' @import data.table
#' @export
valid_sample_home_bg = function(dt, col, ...) {

    # Regex for other bg columns like home_bg_2010 and home_bg_2020
    bg_cols = grep("home_bg_\\d{4}", names(dt), value = TRUE)

    # Ensure that if there are any NULLs in sample_home_bg,
    # there are no NULLs in the other bg columns
    check = dt[
        get("home_in_region") == 1,
        is.na(get("sample_home_bg")) & rowSums(is.na(.SD)) > 0,
        .SDcols = bg_cols
    ]

    if (any(check)) {
        return(FALSE)
    }

    return(TRUE)        
}
