skip_if(
    Sys.getenv('pops.user') == "",
    "No database credentials found. Skipping tests."
)

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
    'trip' = 'ex_trip',
    'linked_trip' = 'ex_linked_trip',
    'tour' = 'ex_tour',
    'variable_list' = 'ex_variable_list',
    'value_labels' = 'ex_value_labels'
)
dbschema = 'combined_2023'
dbname = 'sw_wash_hts'


# LOAD DATA ==================================================================
tables_list = fetch_tables(table_names, dbname, dbschema)


# Load table schema ----------------------------------------------------------
schemas_list = load_rsg_default_schema()


# Table relation checks ======================================================
testthat::expect_no_error(validate_table_relation(tables_list))

# Check invidual tables ======================================================
for (table_name in names(tables_list)) {
    message("Validating table: ", table_name)
    testthat::expect_no_error(
        validate_table(
            dt = tables_list[[table_name]],
            schema = schemas_list[[table_name]],
            trim = TRUE
        )
    )
}

# Validate all tables ========================================================
testthat::expect_no_error(validate_all_tables(tables_list, schemas_list))

# TODO: Need to add tests for expected fails

# TODO: Need to add tests for individual functions
