#' Position of strings
#' @param tex_line_split A split line (via \code{strsplit(x, split = "")}).
#' @param command_split The string the position of which is desired, split (via \code{strsplit(x, split = "")}).
#' @param end (logical) Should the position of the \strong{end} of the string.
#' By default, \code{TRUE}; otherwise, the start of the string is chosen.
#' @return The end (or start if end = FALSE) of the location of command
# name in tex line.
position_of_string <- function(tex_line_split, command_split, end = TRUE){
  not_found <- TRUE
  for (i in seq_along(tex_line_split)){
    if (not_found && command_split[1] == tex_line_split[i]){
      j <- 2
      while (not_found && command_split[j] == tex_line_split[i + j - 1]){
        j <- j + 1
        if (j > length(command_split)){
          not_found <- FALSE
          out <- i
        }
      }
    }
  }
  if (not_found){
    return(-1)
  }
  if (end){
    out + length(command_split)
  } else {
    out
  }
}

#' @rdname position_of_string
#' @param tex_line A line of text.
#' @param command_name The string the position of which is desired.
positions_of_all_strings <- function(tex_line, command_name, end = TRUE){
  stopifnot(length(tex_line) == 1L,
            length(command_name) == 1L)
  
  tex_line_split <- strsplit(tex_line, split = "", fixed = TRUE)[[1]]
  command_split <- strsplit(command_name, split = "", fixed = TRUE)[[1]]
  
  n_occurences <- stri_count_fixed(tex_line, command_name)
  if (n_occurences == 1L){
    out <- position_of_string(tex_line_split, command_split, end = end)
  } else {
    out <- integer(n_occurences)
    start_at <- 0
    for (o in seq_along(out)){
      out[o] <- position_of_string(tex_line_split, command_split, end = end) - 1
      start_at <- position_of_string(tex_line_split, command_split, end = TRUE)
      tex_line_split <- tex_line_split[-c(1:(start_at - 1))]
    }
  }
  out + cumsum(shift(out)) + 1
}

stri_count_fixed <- function(str, pattern) {
  if (requireNamespace("stringi", quietly = TRUE)) {
    stringi::stri_count_fixed(str, pattern)
  } else {
    relevant_line_nos <- grep(pattern, str, fixed = TRUE)
    relevant_lines <- str[relevant_line_nos]
    count_on_relevant <- 
      if (nchar(pattern) == 1L) {
        split_lines <- strsplit(relevant_lines, split = "", fixed = TRUE)
        vapply(split_lines, function(x) sum(x == pattern), integer(1L))
      } else {
        # If you wanted speed, you should have used stringi!
        vapply(relevant_lines, function(line) {
          count <- 0L
          # How many times do we have to cut 'pattern' away?
          while (grepl(pattern, line, fixed = TRUE)) {
            count <- count + 1L
            # Bear in mind 'aaaaa' where pattern = 'aa'
            line <- sub(pattern, replacement = "", line, fixed = TRUE)
          }
          count
        }, 
        FUN.VALUE = integer(1L))
      }
    out <- integer(length(str))
    out[relevant_line_nos] <- count_on_relevant
    out
  }
}







