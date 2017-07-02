test_that("assert_is_string raises an error for invalid input", {
    expect_error(assert_is_string(123))
    expect_error(assert_is_string(list(a="test string")))
    expect_error(assert_is_string(c("a", "b", "c")))
})

test_that("assert_is_string does not raise an error for valid input", {
    expect_message(assert_is_string("test string"), NA)
})

test_that("assert_is_character raises an error for invalid input", {
    expect_error(assert_is_character(123))
    expect_error(assert_is_character(list(a="test_string")))
        expect_error(assert_is_character(NULL, null.ok=FALSE))
})

test_that("assert_is_character does not raise an error for valid input", {
    expect_message(assert_is_character("test string"), NA)
    expect_message(assert_is_character(c("a", "b", "c")), NA)
    expect_message(assert_is_character(NULL, null.ok=TRUE), NA)
})

test_that("assert_is_integer raises an error for invalid input", {
    expect_error(assert_is_integer(c(1, 2, 3)))
    expect_error(assert_is_integer(list(a=123)))
    expect_error(assert_is_integer("test string"))
    expect_error(assert_is_integer(1.5))
    expect_error(assert_is_integer(1, lower=2))
    expect_error(assert_is_integer(1, upper=0))
})

test_that("assert_is_integer does not raise an error for valid input", {
    expect_message(assert_is_integer(123), NA)
    expect_message(assert_is_integer(-123), NA)
    expect_message(assert_is_integer(0), NA)
    expect_message(assert_is_integer(0, lower=0, upper=1), NA)
    expect_message(assert_is_integer(1, lower=0, upper=1), NA)
})

test_that("assert_subset raises an error for invalid input", {
    expect_error(assert_subset(c("a", "b", "c"), NULL))
    expect_error(assert_subset(c("a", "b", "c"), c("a", "d")))
    expect_error(assert_subset(c(1, 2, 3), c(1, 4)))
})

test_that("assert_subset does not raise an error for valid input", {
    expect_message(assert_subset(c("a", "b", "c"), NULL, null.ok=TRUE), NA)
    expect_message(assert_subset(c("a", "b", "c"), c("a", "c")), NA)
    expect_message(assert_subset(c(1, 2, 3), c(1, 3)), NA)
})

test_that("assert_file_exists raises an error for invalid file", {
    expect_error(assert_file_exists("./files/empty_file.invalid"))
})

test_that("assert_file_exists does not an error for valid file", {
    expect_message(assert_file_exists("./files/empty_file.valid"), NA)
})

test_that("assert_file_is_json raises an error for invalid file", {
    expect_message(assert_file_exists("./files/empty_file.valid"), NA)
    expect_error(assert_file_is_json("./files/empty_file.valid"))
})

test_that("assert_file_is_json does not raise an error for valid file", {
    expect_message(assert_file_is_json("./files/empty_file.json"), NA)
})
