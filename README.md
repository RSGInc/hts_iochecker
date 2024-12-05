# HTS Table Input/Output Checker
This package is a table validator, it checks that your HTS tables (e.g., households, persons, days, trips, linked trips, and tours) pass basic validation tests as part of an RSG standard. The RSG standard ensures that the data is still compatible for downstream pipelines, i.e., weighting.

This checker is an R package so that it is portable, ensuring consistent tests are used at multiple points in our workflow. For example, it should be used at the end of the export table scripts before weighting so that the data are prepared for weighting, but also at the start of weighting to verify this.

## Implemented validations
[x] Field schema - Checks that every field passes some tests, e.g., no_null or greater than 0.
[x] Table relations - Checks that there are no orphaned records, e.g., every person has a household.

## Installation
To use the HTS IO Checker, you must install it as a package. To do this, make sure your github account is setup on your local machine in order to access our private RSGInc remote repository.

If you are using `renv` to manage your R environment (as you should be!), you may install it using:
```
renv::install('RSGInc/hts_iochecker')
```

If not using renv, the next easiest method is using `devtools`:
```
devtools::install_github('RSGInc/hts_iochecker')
```

## Usage
In R, a schema is a nested list, similar to a JSON, this is so it can store multiple tests per field. Here is an example:

```
simple_table_pass = data.table(
    field_a = c(1, 2, 3, 4, 5),
    field_b = c("A", "B", "C", "A", "B")
)

simple_table_fail = data.table(
    field_a = c(-1, 2, 3, 4, 5),
    field_b = c("A", "B", "C", "D", "E")
)

simple_schema = list(
        field_a = list(
            tests = function(x) {
                return(all(x > 0))
            },
            data_type = "integer",
            coerce = TRUE,
            required = TRUE,
            description = "Field A is an integer greater than 0"
        ),
        field_b = list(
            tests = list(
                function(x) {
                    return(all(x %in% c("A", "B", "C")))
                },
                "valid_no_null" # Custom function from checks.R in this package.
            ),
            data_type = "character",
            coerce = TRUE,
            required = TRUE,
            description = "Field B is a character in A, B, C"
        )
    )

# This should fail! :(
validate_table(simple_table_fail, simple_schema)

# This should pass! :)
validate_table(simple_table_pass, simple_schema)
```

To avoid writing more loops, you can also validate multiple tables and schemas in one test by passing two named lists to the `validate_all_tables` function. 
```
table_list = list(
  table_1 = simple_table_pass,
  table_2 = simple_table_fail
)

schema_list = list(
  table_1 = simple_schema,
  table_2 = simple_schema
)

validate_all_tables(table_list, schema_list)

```


This package has a default schema list for all typical RSG HTS tables.

```
# Default RSG schema coded into the IO Checker package
schemas_list = hts.iochecker::load_rsg_default_schema()

# Load tables to be validated ===============================================
table_names = intersect(names(schemas_list), names(get("hts_table_map", settings)))
tables_list = lapply(table_names, fetch_hts_table, settings)
names(tables_list) = table_names


# Validate tables ===========================================================

# Validate table relation structure -----------------------------------------
hts.iochecker::validate_table_relation(schemas_list)

# Validate tables schema ----------------------------------------------------
hts.iochecker::validate_all_tables(tables_list, schemas_list)
```
