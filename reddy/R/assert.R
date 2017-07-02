#' Asserts that the input is string.
#'
#' @param text Input that is checked for being a string.
assert_is_string <- function(text, ...) {
    if (!(is.character(text) & length(text) == 1)) {
        stop(paste("Input", text, "is not a string", sep=" "))
    }
}

#' Asserts that input is integer
#'
#' @param number Input that is checked for being an integer.
assert_is_integer <- function(number, ...) {
    if (!(length(number) == 1)) {
        stop(paste("Input", number, "is not a integer", sep=" "))
    }

    if (!(number %% 1 == 0 )) {
        stop(paste("Input", number, "is not a integer", sep=" "))
    }
}

#' Asserts that the input is an existing file.
#'
#' @param filename Input that is checked for being an existing file.
assert_file_exists <- function(filename, ...) {
    assert_is_string(filename)

    if (!file.exists(filename)) {
        stop(paste("File", filename, "does not exist!", sep=" "))
    }
}

#' Asserts that the input is an existing json file
#'
#' @param filename Input that is chcked for being an existing json file.
assert_file_is_json <- function(filename, ...) {
    assert_file_exists(filename)

    if (!endsWith(filename, ".json")) {
        stop(paste("File", filename, "does not end with .json", sep=" "))
    }
}
