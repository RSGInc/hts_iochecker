# testthat::test_dir("tests/testthat", filter = "simple_relation")

table_list_pass <- list(
  "household" = data.table(
    hh_id = c(1, 2, 3)
  ),
  "person" = data.table(
    person_id   = c(1, 2, 3, 4, 5),
    hh_id       = c(1, 1, 2, 2, 3)
  ),
  "day" = data.table(
    day_id      = c(1, 2, 3, 4, 5, 6, 7, 8),
    person_id   = c(1, 1, 2, 2, 3, 3, 4, 5),
    hh_id       = c(1, 1, 1, 1, 2, 2, 2, 3)
  ),
  "trip" = data.table(
    trip_id     = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16),
    day_id      = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8),
    person_id   = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 5, 5),
    hh_id       = c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3)
  )
)

# Create an orphaned table list ----------------------------------------------
table_list_orphaned <- copy(table_list_pass)
table_list_orphaned$household <- table_list_orphaned$household[-1]
table_list_orphaned$day <- table_list_orphaned$day[day_id != 5]

# Create a childless table list ----------------------------------------------
table_list_dinks <- copy(table_list_pass)
table_list_dinks$person <- table_list_dinks$person[person_id != 5]
table_list_dinks$day <- table_list_dinks$day[person_id != 5]
table_list_dinks$trip <- table_list_dinks$trip[person_id != 5]


# Test that the function works as expected -----------------------------------
testthat::expect_no_error(validate_table_relation(table_list_pass))
testthat::expect_error(validate_table_relation(table_list_orphaned))
testthat::expect_error(validate_table_relation(table_list_dinks))
