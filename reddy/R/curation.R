#' @export
tokenize <- function(corpus, rare_word_limit=10, stopwords=NULL, token="words", to_lower=FALSE, ...) {
    checkr::assert_subset(names(corpus), c("id", "body"))
    checkr::assert_character(corpus$id)
    checkr::assert_character(corpus$body)
    checkr::assert_character(stopwords, null.ok=TRUE)

    corpus <- dplyr::data_frame(id=as.factor(corpus$id), text=corpus$body)

    tokenized_corpus <- corpus %>%
        tidytext::unnest_tokens(token, text, token=token, to_lower=to_lower, ...)

    words_to_remove <- NULL

    if (rare_word_limit > 0) {
        words_to_remove <- .get_rare_words(corpus, rare_word_limit, to_lower)
    }

    if (!is.null(stopwords)) {
        words_to_remove <- c(words_to_remove, stopwords)
    }

    if (!is.null(words_to_remove)) {
        words_to_remove <- dplyr::data_frame(token=words_to_remove)

        if (token == "words") {
            tokenized_corpus <- tokenized_corpus %>%
                mutate(row=row_number()) %>%
                anti_join(words_to_remove, by=c("token")) %>%
                arrange(row) %>%
                mutate(row=NULL)
        } else {

        }
    }

    tokenized_corpus
}

#' @keywords internal
.get_rare_words <- function(corpus, tokenized_corpus=NULL, rare_word_limit=10, to_lower=FALSE) {
    if (!is.null(tokenized_corpus)) {
        rare_words <- tokenized_corpus %>%
            filter(n <= rare_word_limit) %>%
            mutate(n=NULL)
    } else {
        rare_words <- corpus %>%
            tidytext::unnest_tokens(word, text, token="words", to_lower=to_lower, ...) %>%
            filter(n <= rare_word_limit) %>%
            mutate(n=NULL)
    }

    rare_words$word
}

#' @export
collapse_tokenized_corpus <- function(tokenized_corpus) {
    checkr::assert_type(tokenized_corpus, "tbl_df")
    checkr::assert_subset(names(tokenized_corpus), c("id", "token"))

    tokenized_corpus %>%
        group_by(id) %>%
        mutate(text=paste(token, sep="", collapse=" "),
               token=NULL) %>%
        filter(row_number() == 1) %>%
        ungroup() %>%
        mutate(id=as.character(document))
}
