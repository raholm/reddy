#' Reads a json file containing Reddit comments in the form of a stream.
#' It assumes that every line in the file is a comment record in the form of a json object.
#'
#' @param filename The filename
#' @param pagesize The number of rows read in each iteration
#' @return A dataframe containing the Reddit comments
#'
#' @export
read_reddit_stream <- function(filename, pagesize=1000) {
    assert_file_is_json(filename)

    data <- try(jsonlite::stream_in(file(filename), pagesize=pagesize, verbose=FALSE), silent=TRUE)

    .check_reddit_data(data)
}

#' Reads a json file containing Reddit comments in a single pass.
#' Assumes that the comments are enclosed in '[]' and separated by ','.
#'
#' @note This is very memory hungry. For larger files it is recommended to use \code{\link{read_reddit_stream}} instead.
#'
#' @param filename The filename
#' @return A dataframe constaining the Reddit comments
#'
#' @export
read_reddit_raw <- function(filename) {
    assert_file_is_json(filename)

    data <- try(jsonlite::read_json(filename, simplifyVector = TRUE), silent=TRUE)

    .check_reddit_data(data)
}

#' @keywords internal
.check_reddit_data <- function(data) {
    if (!is.data.frame(data)) {
        error_message <- data[[1]]
        hint <- NULL

        if (grepl("EOF", error_message) |
            grepl("',' or ']'", error_message)) {
            hint <- "The error probabily means that the json-objects are not enclosed in '[]' and/or separated by ','"
        }

        if (!is.null(hint))
            error_message <- paste(error_message, paste("Hint:", hint, sep=" "), sep="\n")

        stop(error_message)
    }

    data
}
