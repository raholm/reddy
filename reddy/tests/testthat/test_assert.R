test_that("assert_is_string raises an error for invalid input", {
    expect_error(assert_is_string(123))
    expect_error(assert_is_string(list(a="test string")))
    expect_error(assert_is_string(c("a", "b", "c")))
})

test_that("assert_is_string does not raise an error for valid input", {
    expect_message(assert_is_string("test string"), NA)
})

test_that("assert_is_integer raises an error for invalid input", {
    expect_error(assert_is_integer(c(1, 2, 3)))
    expect_error(assert_is_integer(list(a=123)))
    expect_error(assert_is_integer("test string"))
    expect_error(assert_is_integer(1.5))
})

test_that("assert_is_integer does not raise an error for valid input", {
    expect_message(assert_is_integer(123), NA)
    expect_message(assert_is_integer(-123), NA)
    expect_message(assert_is_integer(0), NA)
})

test_that("assert_file_exists raises an error for invalid file", {
    expect_error(assert_file_exists("./files/file.invalid"))
})

test_that("assert_file_exists does not an error for valid file", {
    expect_message(assert_file_exists("./files/file.valid"), NA)
})

test_that("assert_file_is_json raises an error for invalid file", {
    expect_message(assert_file_exists("./files/file.valid"), NA)
    expect_error(assert_file_is_json("./files/file.valid"))
})

test_that("assert_file_is_json does not raise an error for valid file", {
    expect_message(assert_file_is_json("./files/file.json"), NA)
})
