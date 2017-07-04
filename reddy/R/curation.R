#' @export
tokenize <- function(corpus, rare_word_limit=10, stopwords=NULL) {
    assert_subset(names(corpus), "body")

    text <- dplyr::data_frame(id=as.factor(corpus$id), text=corpus$body)
    tidytext::unnest_tokens(text, word, text, token=stringr::str_split, pattern=" ", to_lower=FALSE)

    if (rare_word_limit > 0) {
        corpus <- .remove_rare_words(corpus, rare_word_limit)
    }

    if (!is.null(stopwords)) {
        corpus <- .remove_stopwords(corpus, stopwords)
    }

    corpus
}

#' @keywords internal
.remove_rare_words <- function(corpus, rare_word_limit) {
    corpus
}

#' @keywords internal
.remove_stopwords <- function(corpus, stopwords) {
    corpus
}
