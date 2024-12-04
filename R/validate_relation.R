#' @title Validate table relations
#' @description Validate table relations by recusively checking for the presence of key columns in other tables
#' @param tables_list A list of data.tables
#' @param id_key A named vector of key ID columns and their respective table names
#' @return TRUE if all table relations are valid
#' @export
validate_table_relation = function(tables_list, id_key = NULL) {

    if (is.null(id_key)) {
        id_key = c(
            "hh_id" = "household",
            "person_id" = "person",
            "day_id" = "day",
            "trip_id" = "trip",
            "linked_trip_id" = "linked_trip",
            "tour_id" = "tour"
        )
    }
    
    for (table_name in names(tables_list)) {                    
        # Get ID columns in table
        dt = tables_list[[table_name]]
        id_cols = intersect(names(dt), names(id_key))

        # Check for presence of parent record
        for (id in id_cols) {
            
            # Skip self
            if (id_key[id] == table_name) {
                next
            }

            # Check if the ID is in the parent table
            message("Checking that ", table_name, "$", id, " is in ", id_key[id])
            all(dt[[id]] %in% tables_list[[id_key[id]]][[id]])
        }
    }

}