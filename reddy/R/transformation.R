#' Removes attributes from a corpus
#'
#' @param corpus A dataframe of the corpus
#' @param attrs A character with the attributes to remove
#' @param negative A boolean, if true then the input \code{attrs} are instead kept.
#' @return A filtered corpus
#'
#' @export
remove_attrs <- function(corpus, attrs=NULL, negative=FALSE) {
    checkr::assert_character(attrs, null.ok=TRUE)
    checkr::assert_subset(attrs, names(corpus))

    if (negative) {
        attrs <- names(corpus)[!(names(corpus) %in% attrs)]
    }

    .remove_attrs(corpus, attrs)
}

#' @keywords internal
.remove_attrs <- function(corpus, attrs) {
    corpus %>% dplyr::select(-dplyr::one_of(attrs))
}

#' Removes digits from a corpus
#'
#' It does not remove numbers embedded in words such as 'hello123', '123hello', 'h3ll0'.
#'
#' @param corpus A dataframe of the corpus
#' @return A filtered corpus
#'
#' @export
remove_digits <- function(corpus) {
    checkr::assert_subset("body", names(corpus))
    .remove_digits(corpus)
}

#' @keywords internal
.remove_digits <- function(corpus) {
    .remove_digits_helper <- function(text) {
        stringr::str_trim(gsub("\\b\\d+\\b", "", text))
    }

    corpus %>% dplyr::mutate(body=.remove_digits_helper(body))
}

#' Removes non-alphanumeric characters from a corpus
#'
#' @param corpus A dataframe of the corpus
#' @return A filtered corpus
#'
#' @export
remove_non_alphanumeric <- function(corpus) {
    checkr::assert_subset("body", names(corpus))
    .remove_non_alphanumeric(corpus)
}

#' @keywords internal
.remove_non_alphanumeric <- function(corpus) {
    .remove_non_alphanumeric_helper <- function(text) {
        stringr::str_trim(stringr::str_replace_all(text, "[[:punct:]^]", ""))
    }

    corpus %>% dplyr::mutate(body=.remove_non_alphanumeric_helper(body))
}

#' Removes emails from a corpus
#'
#' @export
remove_emails <- function(corpus)  {
    checkr::assert_subset("body", names(corpus))
    .remove_emails(corpus)
}

#' @keywords internal
.remove_emails <- function(corpus) {
    .remove_emails_helper <- function(text) {
        stringr::str_trim(gsub('\\S+@\\S+.\\S+', '', text))
    }

    corpus %>% dplyr::mutate(body=.remove_emails_helper(body))
}
