#' Fix badly-prefixed labels
#' @param filename The filename whose contents is to be fixed.
#' @param out.file The filename where the fixed version of \code{filename} is to be written. By default, \code{filename}; i.e. \code{filename} is overwritten by this process.
#' @export

fix_labels <- function(filename, out.file = filename) {
  out <- lines <- read_lines(filename)
  
  figure <- FALSE
  table <- FALSE
  
  is_label_line <- grepl("\\label", lines, fixed = TRUE)
  # Use for loop to maintain awareness of environs
  i <- 0
  for (line in lines) {
    i <- i + 1
    if (grepl("\\begin{figure}", line, fixed = TRUE)) {
      figure <- TRUE
    }
    if (grepl("\\begin{table}", line, fixed = TRUE)) {
      table <- TRUE
    }
    if (grepl("\\end{figure}", line, fixed = TRUE)) {
      figure <- FALSE
    }
    if (grepl("\\end{table}", line, fixed = TRUE)) {
      table <- FALSE
    }
    
    if (is_label_line[i]) {
      label_contents <- sub("^.*\\\\label\\{(.*?)\\}","\\1", line, perl = TRUE)
      # Do not replace the label if it contains a colon (suggestive of a prefix)
      if (!grepl(":", label_contents, fixed = TRUE)) {
        command <- sub("^.([a-z]+)\\{.*$", "\\1", line, perl = TRUE)
        switch(command, 
               "caption" = if (figure) {
                 out[i] <- sub("^(.*)\\\\label\\{(.*)$",
                               "\\1\\\\label{fig:\\2",
                               line,
                               perl = TRUE)
               } else if (table) {
                   out[i] <- sub("^(.*)\\\\label\\{(.*)$",
                                 "\\1\\\\label{tbl:\\2",
                                 line,
                                 perl = TRUE)
               }, 
               
               "chapter" = {
                 out[i] <- sub("^(.*)\\\\label\\{(.*)$",
                               "\\1\\\\label{chap:\\2",
                               line,
                               perl = TRUE)
               }, 
               
               "section" = {
                 out[i] <- sub("^(.*)\\\\label\\{(.*)$",
                               "\\1\\\\label{sec:\\2",
                               line,
                               perl = TRUE)
               }, 
               
               "subsection" = {
                 out[i] <- sub("^(.*)\\\\label\\{(.*)$",
                               "\\1\\\\label{subsec:\\2",
                               line,
                               perl = TRUE)
               })
      }
    }
  }
  readr::write_lines(out, out.file)
}
