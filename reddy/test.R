library(jsonlite)

assert_json_has_fields <- function(json, fields, ...) {
    for (field in fields) {
        if (!exists(field, json)) {
            stop(paste("Field", field, "does not exist."))
        }
    }
}

assert_reddit_comment_record_is_valid <- function(record, ...) {
    assert_is_integer(record$created_utc)
    assert_is_string(record$subreddit)
    assert_is_string(record$body)
    assert_is_string(record$author)
}

format_reddit_comment_record <- function(record, ...) {
    record$created_utc <- as.numeric(record$created_utc)
    record$subreddit <- as.character(record$subreddit)
    record$body <- as.character(record$body)
    record$author <- as.character(record$author)
    record
}

create_reddit_comment_record <- function(line, ...) {
    required_fields <- c("created_utc", "subreddit", "body", "author")
    json <- fromJSON(line, simplifyDataFrame = TRUE)

    assert_json_has_fields(json, required_fields)

    record <- list()

    for (field in required_fields) {
        record[[field]] <- json[[field]]
    }

    record <- format_reddit_comment_record(record)
    assert_reddit_comment_record_is_valid(record)
    record
}

data <- try(read_json("../inst/RC_2008_01.json", simplifyVector = TRUE), FALSE)

if (!is.data.frame(data)) {
    error_message <- data[[1]]
    print(error_message)

    if (grepl("EOF", error_message)) {
        print("The error probabily means that the json-objects are not enclosed in '[]' and separated by ','")
    }

}
