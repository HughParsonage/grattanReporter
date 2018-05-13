
read_lines <- function(...) {
  if (requireNamespace("readr", quietly = TRUE)) {
    tryCatch(readr::read_lines(...), error = function(e) {dir(dirname(..1)); stop(e$m)})
  } else {
    readLines(...)
  }
}

write_lines <- function(...) {
  if (requireNamespace("readr", quietly = TRUE)) {
    readr::write_lines(...)
  } else {
    writeLines(...)
  }
}

