


isbn_table <- function() {
  if (anyNA(dropbox.path <- dropbox_path())) {
    message("Unable to retrieve ISBNs.")
    return(data.table())
  }

  if (!requireNamespace("readxl", quietly = TRUE)) {
    message("Unable to use the readxl package.")
    return(data.table())
  }

  ISBN.xlsx <- file.path(dropbox.path, "ISBN", "Grattan-ISBNs.xlsx")
  if (!file.exists(ISBN.xlsx)) {
    message("Connected to Dropbox successfully but no './ISBN/Grattan-ISBN.xlsx' ",
            "file. Request access from grattan-admin.")
    return(data.table())
  }

  ISBN_updated_at <- file.mtime(ISBN.xlsx)

  out <-
    readxl::read_excel(file.path(dropbox.path, "ISBN", "Grattan-ISBNs.xlsx")) %>%
    as.data.table

  stopifnot("Title" %in% names(out))
  setattr(out, "isbn_updated_at", ISBN_updated_at)
  return(out)
}

next_isbn <- function() {
  the_isbn_table <- isbn_table()
  if (nrow(the_isbn_table)) {
    age <-
      difftime(Sys.time(),
               attr(the_isbn_table, "isbn_updated_at"),
               units = "days") %>%
      as.integer
    Title <- NULL
    out <- isbn_table()[which.max(is.na(Title))][["ISBN"]]
    setattr(out, "isbn_age", age)
    return(out)
  } else {
    ""
  }
}





