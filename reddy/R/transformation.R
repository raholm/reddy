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

    corpus %>% dplyr::select(-dplyr::one_of(attrs))
}
