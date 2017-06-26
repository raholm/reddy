test_that("assert_is_string raises an error for invalid input", {
    expect_error(assert_is_string(123))
    expect_error(assert_is_string(list(a="test string")))
    expect_error(assert_is_string(c("a", "b", "c")))
})

test_that("assert_is_string does not raise an error for valid input", {
    expect_success(assert_is_string("test string"))
})

test_that("assert_file_exists raises an error for invalid file", {
    expect_error(assert_file_exists("file.invalid"))
})

test_that("assert_file_exists does not raise an error for valid file", {
    expect_success(assert_file_exists("file.valid"))
})

test_that("assert_file_is_json raises an error for invalid file", {
    expect_success(assert_file_exists("file.invalid"))
    expect_error(assert_file_is_json("file.invalid"))
})

test_that("assert_file_is_json does not raise an error for valid file", {
    expect_success(assert_file_is_json("file.json"))
})
