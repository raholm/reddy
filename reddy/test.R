library(jsonlite)

assert_json_has_fields <- function(json, fields, ...) {
    for (field in fields) {
        if (!exists(field, json)) {
            stop(paste("Field", field, "does not exist."))
        }
    }
}

assert_reddit_comment_record_is_valid <- function(record, ...) {
    assert_integer(record$created_utc)
    assert_string(record$subreddit)
    assert_string(record$body)
    assert_string(record$author)
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

library(reddy)
library(mallet)

corpus <- read_reddit_raw("../data-raw/RC_2008_01_1000_raw.json") %>%
    filter_attrs(c("id", "author", "body"), negative=TRUE) %>%
    filter_digits() %>% filter_non_alphanumeric()

corpus <- read_reddit_stream("../data-raw/RC_2008_01_1000_stream.json") %>%
    filter_attrs(c("id", "author", "body"), negative=TRUE) %>%
    filter_digits() %>% filter_non_alphanumeric()


## corpus$id <- as.character(corpus$id)
corpus$body <- as.character(corpus$body)

mallet_instances <- mallet.import(corpus$id, corpus$body, "../data-raw/stopwords.txt")

topic_model <- MalletLDA(num.topics=20)
topic_model$loadDocuments(mallet_instances)
vocabulary <- topic_model$getVocabulary()
word_freqs <- mallet.word.freqs(topic_model)

topic_model$setAlphaOptimization(20, 50)
topic_model$train(200)

topic_model$maximize(10)

doc_topics <- mallet.doc.topics(topic_model, smoothed=T, normalized=T)
topic_words <- mallet.topic.words(topic_model, smoothed=T, normalized=T)

mallet.top.words(topic_model, topic_words[1,])
