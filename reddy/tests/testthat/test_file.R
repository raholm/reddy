test_that("read_reddit_stream raises an error for invalid filetype", {
    expect_error(read_reddit_stream("./files/empty_file.valid"))
})

test_that("read_reddit_stream raises an error for invalid syntax", {
    expect_error(read_reddit_stream("./files/read_stream_present_object_separation.json"))
    expect_error(read_reddit_stream("./files/read_stream_present_object_enclosure.json"))
})

## test_that("read_reddit_stream does not raise an error for valid input", {
##     expect_message(read_reddit_stream("./files/read_stream_valid.json"), NA)
## })

test_that("read_reddit_raw raises an error for invalid filetype", {
    expect_error(read_reddit_raw("./files/empty_file.valid"))
})

test_that("read_reddit_raw raises an error for invalid syntax", {
    expect_error(read_reddit_raw("./files/read_raw_missing_object_separation.json"))
    expect_error(read_reddit_raw("./files/read_raw_missing_object_enclosure.json"))
})

test_that("read_reddit_raw does not raise an error for valid input", {
    expect_message(read_reddit_raw("./files/read_raw_valid.json"), NA)
})
