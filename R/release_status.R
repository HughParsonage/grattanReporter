

release_status <- function(filename) {
  lines <- read_lines(filename)
  
  lines_before_begin_document <-
    lines[1:grep("\\begin{document}", lines, fixed = TRUE)]

  if(any(grepl("^[%]\\s*[Rr]elease(:| [=])\\s*[Tt]rue\\s*$", lines_before_begin_document))){
    c("release", "pre_release", "compile")
    } else {if(any(grepl("^[%]\\s*[Pp]re_[Rr]elease(:| [=])\\s*[Tt]rue\\s*$", lines_before_begin_document))){
    c("pre_release", "compile")
    } else {if(any(grepl("^[%]\\s*[Cc]ompile(:| [=])\\s*[Tt]rue\\s*$", lines_before_begin_document))){
          "compile"}}}
}
