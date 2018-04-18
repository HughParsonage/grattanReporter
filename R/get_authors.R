#' Extract authors from report
#' @param filename The filename whose preamble contains the names of the authors.
#' @param include_editors (logical) Should editorial members of staff be included?
#' @param rstudio Should the RStudio API be used?
#' @return The names of Grattan staff who were authors in \code{filename}.
#' @export

get_authors <- function(filename, include_editors = TRUE, rstudio = FALSE) {
  lines <- read_lines(filename)
  file_path <- dirname(filename)

  lines_before_begin_document <-
    lines[1:grep("\\begin{document}", lines, fixed = TRUE)]
  
  if (any(grepl("\\input", lines_before_begin_document, fixed = TRUE))) {
    # Ensure the only input in acknowledgements is tex/acknowledgements
    acknowledgements <- 
      paste0(lines_before_begin_document, collapse = " ") %>%
      gsub("^.*\\\\(acknowledgements)", "", ., perl = TRUE)
    
    if (any(grepl("\\input", acknowledgements, fixed = TRUE))){
      inputs <-
        gsub("^.*\\\\(?:(?:input)|(?:include(?!(?:graphics))))[{]([^\\}]+(?:\\.tex)?)[}].*$",
             "\\1",
             acknowledgements,
             perl = TRUE)
      
      if (inputs[[1]] != "tex/acknowledgements"){
        stop("The only permitted \\input in \\acknowledgements is \\input{tex/acknowledgements}")
      }
      
      lines_before_begin_document <- 
        c(lines_before_begin_document,
          readLines(file.path(file_path, "./tex/acknowledgements.tex"),
                    encoding = "UTF-8",
                    warn = FALSE))
    }
  }

  regex_possible_names <-
    sprintf("(%s)",
            paste0("(?:",
                   Grattan_staff[["name"]],
                   ")",
                   collapse = "|"))

  possible_lines_split <-
    grep(regex_possible_names, lines_before_begin_document, value = TRUE) %>%
    # Need to split names.
    strsplit(split = paste0("\\W(?=", regex_possible_names, ")"),
             perl = TRUE) %>%
    unlist %>%
    grep(regex_possible_names,
         .,
         value = TRUE)

  possible_names <-
    possible_lines_split %>%
    gsub(sprintf("^.*(%s).*$",
                 regex_possible_names),
         "\\1",
         .) %>%
    unique
  
  if (any(grepl("^[%] add_author_to_recommended_citation: ", lines_before_begin_document, perl = TRUE))){
    possible_names <-
      c(possible_names,
        gsub("^.*add_author_to_recommended_citation: ",
             "",
             grep("^[%] add_author_to_recommended_citation: ", lines_before_begin_document, perl = TRUE,
                  value = TRUE)))
  }
  
  if (any(grepl("^[%] add_author_to_recommended_citation_at: ", lines_before_begin_document, perl = TRUE))) {
    line_nos_with_add_author_ats <-
      grep("^[%] add_author_to_recommended_citation_at: ",
           lines_before_begin_document,
           perl = TRUE)
    
    lines_with_add_author_ats <-
      lines_before_begin_document %>%
      .[line_nos_with_add_author_ats] %>%
      trimws
    
    if (!all(grepl("[0-9]$", lines_with_add_author_ats))) {
      line_no <-
        line_nos_with_add_author_ats %>%
        .[first(!grepl("[0-9]$", lines_with_add_author_ats))]
      
      report2console(file = filename,
                     line_no = line_no,
                     column = nchar(lines_before_begin_document[line_no]),
                     context = lines_before_begin_document[line_no],
                     error_message = paste0("% add_to_author_recommended_citation_at:",
                                            "  is used, but the line does not end with",
                                            " a number as required."),
                     advice = paste0("Either use\n\t `add_author_to_recommended_citation: <author>`\n", 
                                     "or\n\t `add_author_to_recommended_citation_at: <author> <order>`\n",
                                     "where <order> is an integer, specifying the order."),
                     rstudio = rstudio)
    }
    
    for (author_with_order in lines_with_add_author_ats) {
      the_order <- as.integer(sub("^.* ([0-9]+)$", 
                                  "\\1",
                                  lines_with_add_author_ats,
                                  perl = TRUE))
      
      the_author <- sub("^.*add_author_to_recommended_citation_at: (.*) [0-9]+$",
                        "\\1",
                        lines_with_add_author_ats,
                        perl = TRUE)
      
      possible_names <-
        insert(possible_names, the_order, the_author)
    }
    
    
    
    
    
  }
  
  possible_names <- trimws(possible_names)
  
  if (include_editors){
    return(possible_names)
  } else {
    if (any(grepl("^[%] editorial_author_only: [A-Z]", lines_before_begin_document, perl = TRUE))){
      editorial_authors <-
        gsub("^[%] editorial_author_only: (.+)\\s*$", 
             "\\1", 
             grep("^[%] editorial_author_only: [A-Z]",
                  lines_before_begin_document,
                  value = TRUE,
                  perl = TRUE))
      return(setdiff(possible_names, editorial_authors))
    } else {
      return(possible_names)
    }
  }
}
