utils::globalVariables(
    c("has_synthetic_access", "is_access",
    "has_synthetic_egress", "is_egress")
)

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

#' Validate valid distance meters columns
#' @title valid_distance_meters
#' @description Validate that distance meters columns are valid, i.e., not NULL,
#' except for synthetic trips. 
#' @param dt data.table
#' @param col Target column name
#' @param ... Additional keyword arguments, i.e., locals list with data.tables
#' @return Logical
#' @import data.table
#' @export
valid_distance_meters = function(dt, col, ...) {
    # Check if synthetic trips are in the data
    synth_cols = c("has_synthetic_access", "has_synthetic_egress")
    if (!all(synth_cols %in% names(dt))) {
        check = dt[, !(!is.na(get(col)) | get(col) > 0)]
    } else {
        # Check that synthetic trip distance are NULL
        check = dt[,
            # Check that none fail
            !(
                # Distance must be not null, except for synthetic trips
                (!is.na(get(col)) | get(col) > 0) |
                # Synth trips must be null
                (
                    (is.null(get(col)) | is.na(get(col))) &
                    (
                        (has_synthetic_access == 1 & is_access == 1) |
                        (has_synthetic_egress == 1 & is_egress == 1)
                    )
                )
            )
        ]
    }

    if (any(check)) {
        return(FALSE)
    }

    return(TRUE)        
}
