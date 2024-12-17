# testthat::test_dir("tests/testthat", filter = "simple_schema")

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
                "col_vals_not_null" # Custom function from checks.R in this package.
            ),
            data_type = "character",
            coerce = TRUE,
            required = TRUE,
            description = "Field B is a character in A, B, C"
        )
    )


test_that("validate_table() works", {
    expect_error(validate_table(simple_table_fail, simple_schema, report = FALSE))
    expect_no_error(validate_table(simple_table_pass, simple_schema, report = FALSE))
})
