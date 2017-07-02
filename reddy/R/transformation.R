#' Filter attributes from a corpus
#'
#' @param corpus A dataframe of the corpus
#' @param attrs A character with the attributes to remove
#' @param negative A boolean, if true then the input \code{attrs} are instead kept.
#' @return A filtered corpus
#'
#' @export
filter_attrs <- function(corpus, attrs=NULL, negative=FALSE) {
    assert_is_character(attrs, null.ok=TRUE)
    assert_subset(names(corpus), attrs)

    if (negative) {
        attrs <- names(corpus)[!(names(corpus) %in% attrs)]
    }

    .remove_attrs(corpus, attrs)
}

#' @keywords internal
.remove_attrs <- function(corpus, attrs) {
    corpus %>% dplyr::select(-dplyr::one_of(attrs))
}

#' Filter digits from a corpus
#'
#' It does not remove numbers embedded in words such as 'hello123', '123hello', 'h3ll0'.
#'
#' @param corpus A dataframe of the corpus
#' @return A filtered corpus
#'
#' @export
filter_digits <- function(corpus) {
    assert_subset(names(corpus), "body")
    .remove_digits(corpus)
}

#' @keywords internal
.remove_digits <- function(corpus) {
    .remove_digits_helper <- function(text) {
        as.factor(stringr::str_trim(gsub("\\b\\d+\\b", "", text)))
    }

    corpus %>% dplyr::mutate(body=.remove_digits_helper(body))
}

#' Filter non-alphanumeric characters from a corpus
#'
#' @param corpus A dataframe of the corpus
#' @return A filtered corpus
#'
#' @export
filter_non_alphanumeric <- function(corpus) {
    assert_subset(names(corpus), "body")
    .remove_non_alphanumeric(corpus)
}

#' @keywords internal
.remove_non_alphanumeric <- function(corpus) {
    .remove_non_alphanumeric_helper <- function(text) {
        as.factor(stringr::str_replace_all(text, "[[:punct:]^]", ""))
    }

    corpus %>% dplyr::mutate(body=.remove_non_alphanumeric_helper(body))
}
