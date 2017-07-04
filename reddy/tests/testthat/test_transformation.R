test_that("remove_attrs raises an error for invalid attrs", {
    corpus <- read_reddit_raw("./files/read_raw_valid.json")
    expect_error(remove_attrs(corpus, "kalaspuffar"))
    expect_error(remove_attrs(corpus, 123))
})

test_that("remove_attrs removes the input attribute", {
    corpus <- read_reddit_raw("./files/read_raw_valid.json")
    corpus <- remove_attrs(corpus, "body")

    expect_true(!("body" %in% names(corpus)))
})

test_that("remove_attrs removes the input attributes", {
    corpus <- read_reddit_raw("./files/read_raw_valid.json")
    corpus <- remove_attrs(corpus, c("body", "author"))

    expect_true(all(!(c("body", "author") %in% names(corpus))))
})

test_that("remove_attrs with negative=TRUE keeps the input attribute", {
    corpus <- read_reddit_raw("./files/read_raw_valid.json")
    corpus <- remove_attrs(corpus, "body", negative=TRUE)

    expect_true("body" %in% names(corpus))
})

test_that("remove_attrs with negative=TRUE keeps the input attributes", {
    corpus <- read_reddit_raw("./files/read_raw_valid.json")
    corpus <- remove_attrs(corpus, c("body", "author"), negative=TRUE)

    expect_true(all(c("body", "author") %in% names(corpus)))
})

test_that("remove_digits raises an error for invalid input", {
  expect_error(remove_digits(data.frame()))
})

test_that("remove_digits removes digits from input", {
    corpus <- data.frame(body=c("123 hello 123", "123 h3ll0 321a"), stringsAsFactors=FALSE)
    actual <- remove_digits(corpus)
    expected <- data.frame(body=c("hello", "h3ll0 321a"), stringsAsFactors=FALSE)
    expect_equal(actual, expected)
})

test_that("remove_non_alphanumeric raises an error for invalid input", {
  expect_error(remove_non_alphanumeric(data.frame()))
})

test_that("remove_non_alphanumeric removes non-alphanumeric characters from input", {
    corpus <- data.frame(body=c("hello{@\\[}}}^^.,-^'", "^^. there's a bu^gger!"), stringsAsFactors=FALSE)
    actual <- remove_non_alphanumeric(corpus)
    expected <- data.frame(body=c("hello", "theres a bugger"), stringsAsFactors=FALSE)
    expect_equal(actual, expected)
})
