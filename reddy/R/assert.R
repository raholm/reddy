#'
assert_is_string <- function(text, ...) {
    if (!(is.character(text) & length(text) == 1)) {
        stop(paste("Input", text, "is not a string", sep=" "))
    }
}

#'
assert_file_exists <- function(filename, ...) {
    assert_is_string(filename)

    if (!file.exists(filename)) {
        stop(paste("File", filename, "does not exist!", sep=" "))
    }
}

#'
assert_file_is_json <- function(filename, ...) {
    assert_file_exists(filename)

    if (!endsWith(filename, ".json")) {
        stop(paste("File", filename, "does not end with .json", sep=" "))
    }
}
