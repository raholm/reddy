test_that("rm_attrs raises an error for invalid attrs", {
    corpus <- read_reddit_raw("./files/read_raw_valid.json")
    expect_error(rm_attrs(corpus, "kalaspuffar"))
    expect_error(rm_attrs(corpus, 123))
})

test_that("rm_attrs removes the input attribute", {
    corpus <- read_reddit_raw("./files/read_raw_valid.json")
    corpus <- rm_attrs(corpus, "body")

    expect_true(!("body" %in% names(corpus)))
})

test_that("rm_attrs removes the input attributes", {
    corpus <- read_reddit_raw("./files/read_raw_valid.json")
    corpus <- rm_attrs(corpus, c("body", "author"))

    expect_true(all(!(c("body", "author") %in% names(corpus))))
})

test_that("rm_attrs with negative=TRUE keeps the input attribute", {
    corpus <- read_reddit_raw("./files/read_raw_valid.json")
    corpus <- rm_attrs(corpus, "body", negative=TRUE)

    expect_true("body" %in% names(corpus))
})

test_that("rm_attrs with negative=TRUE keeps the input attributes", {
    corpus <- read_reddit_raw("./files/read_raw_valid.json")
    corpus <- rm_attrs(corpus, c("body", "author"), negative=TRUE)

    expect_true(all(c("body", "author") %in% names(corpus)))
})
