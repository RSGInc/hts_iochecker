# This is a library of data check functions that can be used to validate data.

#' Primary key check
#' @param x Column vector
#' @return Logical
#' @export
#' @examples
#' {
#' dt = data.table(x = c(1, 2, 3, 4, 5))
#' valid_primary_key(dt, "x")
#' }
valid_primary_key = function(x) {

    checks = c(
        is.character(x),
        length(unique(x)) == length(x),
        !any(is.na(x)),
        !any(trimws(x) == ""),
        !any(x == "NA"),
        !any(x == "NULL")
    )

    if (!all(checks)) {
        return(FALSE)
    }

    return(TRUE)
}

#' Validate weight columns
#' @param x Column vector
#' @return Logical
#' @export
#' @examples
#' {
#' dt = data.table(x = c(1, 2, 3, 4, 5))
#' valid_weight(dt, "x")
#' }
valid_numeric = function(x) {

    checks = c(
        is.numeric(x),
        !any(is.na(x)),
        !any(trimws(x) == "")
    )

    if (!all(checks)) {
        return(FALSE)
    }

    return(TRUE)
}

#' Validate binary columns
#' @param dt data.table
#' @param col Target column name
#' @return Logical
#' @export
#' @examples
#' {
#' dt = data.table(x = c(1, 2, 3, 4, 5))
#' valid_binary(dt, "x")
#' }
valid_binary = function(x) {

    checks = c(
        is.numeric(x),
        !any(is.na(x)),
        !any(trimws(x) == ""),
        all(x %in% c(0, 1))
    )

    if (!all(checks)) {
        return(FALSE)
    }

    return(TRUE)
}


#' Validate binary or 99x-like columns
#' @param dt data.table
#' @param col Target column name
#' @return Logical
#' @export
#' @examples
#' {
#' dt = data.table(x = c(1, 2, 3, 4, 999))
#' valid_binary(dt, "x")
#' }
valid_binary_or_99x = function(x) {

    checks = c(
        is.numeric(x),
        !any(is.na(x)),
        !any(trimws(x) == ""),
        all(x %in% c(0, 1, 990:999))
    )

    if (!all(checks)) {
        return(FALSE)
    }

    return(TRUE)
}


#' Validate DOW columns
#' @param x Column vector
#' @export
#' @examples
#' {
#' dt = data.table(x = c(1, 2, 3, 4, 5))
#' valid_dow(dt, "x")
#' }
valid_dow = function(x) {

    checks = c(
        is.numeric(x),
        !any(is.na(x)),
        !any(trimws(x) == ""),
        all(x %in% 1:7)
    )

    if (!all(checks)) {
        return(FALSE)
    }

    return(TRUE)
}

#' Validate no NULL columns
#' @param x Column vector
#' @return Logical
#' @export
#' @examples
#' {
#'  dt = data.table(x = c(1, 2, 3, 4, 5))
#'  valid_no_null(dt, "x")
#' }
valid_no_null = function(x) {
    checks = c(
        !any(is.na(x)),
        !any(trimws(x) == "")
    )

    if (!all(checks)) {
        return(FALSE)
    }

    return(TRUE)
}


#' Validate home geo columns
#' @param dt data.table
#' @param col Target column name
#' @param ... Additional keyword arguments, i.e., locals list with data.tables
#' @return Logical
#' @export
valid_no_null_in_region = function(dt, col, ...) {

    # Only check for households in the region
    x = dt[home_in_region == 1, get(col)]

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
#' @export
valid_sample_home_bg = function(dt, col, ...) {

    # Regex for other bg columns like home_bg_2010 and home_bg_2020
    bg_cols = grep("home_bg_\\d{4}", names(dt), value = TRUE)

    # Ensure that if there are any NULLs in sample_home_bg,
    # there are no NULLs in the other bg columns
    check = dt[
        home_in_region == 1,
        is.na(sample_home_bg) & rowSums(is.na(.SD)) > 0,
        .SDcols = bg_cols
    ]

    if (any(check)) {
        return(FALSE)
    }

    return(TRUE)        
}


#' Validate >0 columns
#' @param x Column vector
#' @return Logical
#' @export
#' @examples
#' {
#' dt = data.table(x = c(1, 2, 3, 4, 5))
#' valid_gt_0(dt$x)
#' }
valid_gt_0 = function(x) {

    checks = c(
        is.numeric(x),
        !any(is.na(x)),
        !any(trimws(x) == ""),
        all(x > 0)
    )

    if (!all(checks)) {
        return(FALSE)
    }

    return(TRUE)
}