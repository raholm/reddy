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
