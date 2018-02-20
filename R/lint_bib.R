#' Tidy bibliography so equals signs align
#' @param bib_file The bib file to tidy.
#' @param outfile Optionally, the tidied bib file to write to.
#' @param leading_spaces The number of spaces before each field within an entry.
#' @details Aligns the equals signs in \code{bib_file}, standardizes the case of entry types and fields.
#' @export

lint_bib <- function(bib_file, outfile = bib_file, leading_spaces = 2L){
  stopifnot(length(bib_file) == 1L, grepl("\\.bib$", bib_file, perl = TRUE))

  bib <- readLines(bib_file, encoding = "UTF-8", warn = FALSE)
  # Correct fields
  # coalesce journal --> journaltitle 
  bib <- gsub("^\\s*journaltitle\\s*[=]", "  journal =", bib, perl = TRUE)
  # Remove things like type = {report}, which are redundant
  bib <- bib[!grepl("^\\s*type\\s*[=]", bib, perl = TRUE)]
  
  is_field <- grepl("=", bib, fixed = TRUE)
  is_entry_type <- grepl("^@", bib, perl = TRUE)
  
  field_width <- nchar(trimws(gsub("[=].*$", "", bib, perl = TRUE)))

  widest_field <- max(field_width[is_field])
  
  field_match_nocase <- sprintf("^@%s", c("article", 
                                          "book", 
                                          "techreport",
                                          "report",
                                          "inproceedings",
                                          "incollection",
                                          "inbook",
                                          "misc",
                                          "phdthesis",
                                          "unpublished"))
  
  field_replacements <- sprintf("@%s", c("Article", 
                                         "Book", 
                                         "TechReport",
                                         "TechReport",
                                         "InProceedings",
                                         "InCollection",
                                         "InBook",
                                         "Misc",
                                         "PhDThesis",
                                         "Unpublished"))
  
  out <- bib

  # Vectorized gsub:
  for (line in seq_along(bib)){
    
    if (is_entry_type[line]) {
      # Standardize entry type:
      for (fj in seq_along(field_match_nocase)) {
        out[line] <- gsub(field_match_nocase[fj], field_replacements[fj], out[line], perl = TRUE, ignore.case = TRUE)
      }
    }
    
    # Replace every field line with
    # two spaces + field name + spaces required for widest field + space
    if (is_field[line]){
      spaces_req <- widest_field - field_width[line]
      out[line] <-
        gsub("^\\s*(\\w+)\\s*[=]\\s*\\{",
             paste0(paste0(rep(" ", leading_spaces), collapse = ""),
                    "\\L\\1\\E",
                    paste0(rep(" ", spaces_req), collapse = ""),
                    " = {"),
             bib[line],
             perl = TRUE)
    }
  }
  
  # Add commas: 
  out[is_field] <- gsub("\\}$", "\\},", out[is_field], perl = TRUE)
  
  writeLines(out, outfile, useBytes = TRUE)
}
