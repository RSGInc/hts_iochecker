# Helper functions ===========================================================
fetch_tables = function(
    table_names,
    dbname,
    dbschema,
    cache_dir = file.path(getwd(), "test_cache")
    ) {

    if (!dir.exists(cache_dir)) {
        dir.create(cache_dir, recursive = TRUE)
    }

    drv = RPostgres::Postgres()
    con = DBI::dbConnect(
        drv,
        host = 'pops.rsginc.com',
        user = Sys.getenv('pops.user'),
        password = Sys.getenv('pops.password'),
        dbname = dbname,
        port = "5432",
        sslcert = "",
        sslkey = "",
        sslmode = "require",
        bigint = c("numeric")
    )

    tables_list = list()
    for (table_name in names(table_names)) {
        message("Reading table: ", table_name)

        if (file.exists(file.path(cache_dir, paste0(table_name, ".rds")))) {
            tables_list[[table_name]] = readRDS(file.path(cache_dir, paste0(table_name, ".rds")))
            next
        }


        query = paste0(
            "SELECT * FROM ", dbschema, ".", table_names[[table_name]]
        )

        result = DBI::dbGetQuery(con, query)
        dt = data.table::setDT(result)

        saveRDS(dt, file.path(cache_dir, paste0(table_name, ".rds")))

        tables_list[[table_name]] = dt
    }

    DBI::dbDisconnect(con)

    return(tables_list)

}

# Set parameters =============================================================
table_names = c(
    'household' = 'ex_hh',
    'person' = 'ex_person',
    'day' = 'ex_day',
    'trip' = 'ex_trip_revised_20240918',
    # 'linked_trip',
    # 'tour',
    'variable_list' = 'ex_variable_list',
    'value_labels' = 'ex_value_labels'
)
dbschema = 'wave3'
dbname = 'metcouncil'


# LOAD DATA ==================================================================
tables_list = fetch_tables(table_names, dbname, dbschema)


# Load table schema ----------------------------------------------------------
schemas_list = load_rsg_default_schema()


# PREPROCESSING THAT SHOULD HAPPEN IN UPSTREAM PIPELINE =======================
# All this annoying business logic should be handled upstream

# Drop nulls in labels ------------------------------------------------------
tables_list[['variable_list']] = tables_list[['variable_list']][
    !is.na(variable) & !is.na(description),
    .(variable, description)
]
tables_list[['value_labels']] = tables_list[['value_labels']][!is.na(value) & !is.na(label)]


# Drop incompletes ----------------------------------------------------------
tables_list[['household']] = tables_list[['household']][hh_is_complete_a == 1 & hh_is_complete_b]

complete_hh = tables_list[['household']][, hh_id]
for (table in c('person', 'day', 'trip', 'linked_trip', 'tour')) {
    if (!table %in% names(tables_list)) {
        next
    }
    tables_list[[table]] = tables_list[[table]][hh_id %in% complete_hh]

    # Make all _id columns character
    id_cols = grep('_id$', names(tables_list[[table]]), value = TRUE)
    tables_list[[table]][, (id_cols) := lapply(.SD, as.character), .SDcols = id_cols]
}


# Recalculate num_trips per day ---------------------------------------------
tables_list[['day']][, num_trips_orig := num_trips]
tables_list[['day']][
    tables_list[['trip']][, .N, by = day_id],
    num_trips := i.N,
    on = 'day_id'
]
tables_list[['day']][is.na(num_trips) & day_is_complete != 1, num_trips := 0]

# Drop incomplete persons ---------------------------------------------------
tables_list[['person']] = tables_list[['person']][
    person_is_complete_a == 1 & person_is_complete_b == 1
]


# Annoying renames ----------------------------------------------------------
tables_list[['household']][, is_complete := ifelse(hh_is_complete_a & hh_is_complete_b, 1, 0)]
tables_list[['household']][, income_followup := income_broad]

tables_list[['person']][, is_complete := ifelse(person_is_complete_a & person_is_complete_b, 1, 0)]
tables_list[['day']][, is_complete := day_is_complete]


# Table relation checks ======================================================
testthat::expect_no_error(validate_table_relation(tables_list))

# Validate all tables ========================================================
testthat::expect_no_error(validate_all_tables(tables_list, schemas_list))
