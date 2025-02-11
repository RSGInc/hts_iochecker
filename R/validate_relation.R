#' @title Validate table relations
#' @description Validate table relations by recusively checking for the
#'  presence of key columns in other tables
#' @param tables_list A list of data.tables
#' @param id_key A named vector of key ID columns and their
#'  respective table names
#' @param allowed_childless A character vector of tables that are allowed to
#'  have no child records.
#'  A common entry might be c('day', 'trip', 'linked_trip', 'tour')
#'  E.g., c('days', 'trips') we allow persons with no days and no trips.
#' @return TRUE if all table relations are valid
#' @export
validate_table_relation <- function(
    tables_list,
    id_key = NULL,
    allowed_childless = NULL) {
  if (is.null(id_key)) {
    id_key <- c(
      "hh_id"             = "household",
      "person_id"         = "person",
      "day_id"            = "day",
      "trip_id"           = "trip",
      "linked_trip_id"    = "linked_trip",
      "tour_id"           = "tour"
    )
  }

  # Drop tables that are not in the list
  id_key <- id_key[id_key %in% names(tables_list)]

  fail <- FALSE
  for (table_name in names(tables_list)) {
    # Get ID columns in table
    dt <- tables_list[[table_name]]
    id_cols <- intersect(names(dt), names(id_key))

    # Check for presence of parent record
    for (id in id_cols) {
      # Skip self
      if (id_key[id] == table_name) next

      # Check that every record has a parent
      has_parent <- dt[[id]] %in% tables_list[[id_key[id]]][[id]]
      fail <- report_parent(fail, has_parent, table_name, id, id_key)

      # Skip children check?
      if (table_name %in% allowed_childless) next

      # Check that every parent has a child
      has_children <- tables_list[[id_key[id]]][[id]] %in% dt[[id]]
      fail <- report_children(fail, has_children, table_name, id, id_key)
    }
  }

  if (fail) {
    stop("Table relations are not valid")
  }
}

#' @title Report parent
#' @description Helper function to report the parent record
#' @param fail logical, current fail status
#' @param has_parent logical vector, whether the parent record is present
#' @param table_name character, the name of the table
#' @param id character, the ID column
#' @param id_key named vector, the ID key
#' @return logical, the updated fail status
report_parent <- function(fail, has_parent, table_name, id, id_key) {
  msg <- paste(
    "Checking if", table_name, "in parent", id_key[id], "on", id
  )

  # Get the maximum column name width for whitespace padding
  max_col_name <- 30 + max(nchar(id_key)) + 2 * max(nchar(names(id_key)))
  ws <- max_col_name - nchar(msg)
  msg <- paste(msg, strrep("-", ws))

  if (all(has_parent)) {
    message(paste0(msg, "> OK"))
  } else {
    message(
      msg, "> FAIL -- ",
      sum(!has_parent), " ", table_name, " records are missing a ", id_key[id]
    )
    fail <- TRUE
  }

  return(fail)
}

#' @title Report children
#' @description Helper function to report the children records
#' @param fail logical, current fail status
#' @param has_children logical vector, whether the children records are present
#' @param table_name character, the name of the table
#' @param id character, the ID column
#' @param id_key named vector, the ID key
#' @return logical, the updated fail status
report_children <- function(fail, has_children, table_name, id, id_key) {
  msg <- paste(
    "Checking if", id_key[id], "has children in", table_name, "on", id
  )
  # Get the maximum column name width for whitespace padding
  max_col_name <- 30 + max(nchar(id_key)) + 2 * max(nchar(names(id_key)))
  ws <- max_col_name - nchar(msg)
  msg <- paste(msg, strrep("-", ws))

  if (all(has_children)) {
    message(paste0(msg, "> OK"))
  } else {
    message(
      msg, "> FAIL -- ",
      sum(!has_children), " ", id_key[id], " records have no ", table_name, " records"
    )
    fail <- TRUE
  }

  return(fail)
}
