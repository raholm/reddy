test_that("filter_attrs raises an error for invalid attrs", {
    corpus <- read_reddit_raw("./files/read_raw_valid.json")
    expect_error(filter_attrs(corpus, "kalaspuffar"))
    expect_error(filter_attrs(corpus, 123))
})

test_that("filter_attrs removes the input attribute", {
    corpus <- read_reddit_raw("./files/read_raw_valid.json")
    corpus <- filter_attrs(corpus, "body")

    expect_true(!("body" %in% names(corpus)))
})

test_that("filter_attrs removes the input attributes", {
    corpus <- read_reddit_raw("./files/read_raw_valid.json")
    corpus <- filter_attrs(corpus, c("body", "author"))

    expect_true(all(!(c("body", "author") %in% names(corpus))))
})

test_that("filter_attrs with negative=TRUE keeps the input attribute", {
    corpus <- read_reddit_raw("./files/read_raw_valid.json")
    corpus <- filter_attrs(corpus, "body", negative=TRUE)

    expect_true("body" %in% names(corpus))
})

test_that("filter_attrs with negative=TRUE keeps the input attributes", {
    corpus <- read_reddit_raw("./files/read_raw_valid.json")
    corpus <- filter_attrs(corpus, c("body", "author"), negative=TRUE)

    expect_true(all(c("body", "author") %in% names(corpus)))
})

test_that("filter_digits raises an error for invalid input", {
  expect_error(filter_digits(data.frame()))
})

test_that("filter_digits removes digits from input", {
    corpus <- data.frame(body=c("123 hello 123", "123 h3ll0 321a"))
    actual <- filter_digits(corpus)
    expected <- data.frame(body=c("hello", "h3ll0 321a"))
    expect_equal(actual, expected)
})

test_that("filter_non_alphanumeric raises an error for invalid input", {
  expect_error(filter_non_alphanumeric(data.frame()))
})

test_that("filter_non_alphanumeric removes non-alphanumeric characters from input", {
    corpus <- data.frame(body=c("hello{@\\[}}}^^.,-^'", "^^. there's a bu^gger!"))
    actual <- filter_non_alphanumeric(corpus)
    expected <- data.frame(body=c("hello", "theres a bugger"))
    expect_equal(actual, expected)
})
