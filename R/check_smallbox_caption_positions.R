#' Check smallboxes intruding on chapter line
#' @param path A path to a report that has been compiled.
#' @param .report_error Function to produce error messages.
#' @return Called for its side-effect: errors in the unusual case when a smallbox of a particular size
#' @export 

check_smallbox_caption_positions <- function(path = ".", .report_error) {
  if (missing(.report_error)){
    .report_error <- function(...) report2console(...)
  }
  
  aux_file <- dir(path = path, pattern = "\\.aux$", full.names = TRUE)[1]
  
  aux_contents <- read_lines(aux_file)
  
  posy <- NULL
  smallbox_locations <-
    grep("zref@newlabel{smallbox@@@", aux_contents, fixed = TRUE, value = TRUE) %>%
    {
      data.table(
        posy = as.integer(gsub("^.*posy[{]([0-9]+)[}].*$", "\\1", x = ., perl = TRUE)),
        smallbox = gsub("^.*smallbox@@@([^\\}]+)[}].*$", "\\1", x = ., perl = TRUE)
      )
    }
  
  page <- NULL
  smallbox_pages <- 
    grep("smallbox@@@.*@cref", aux_contents, perl = TRUE, value = TRUE) %>%
    {
      data.table(
        smallbox = sub("@cref", "",
                       fixed = TRUE,
                       sub("^.*smallbox@@@([^\\}]+)[}].*$", "\\1",
                           x = .,
                           perl = TRUE)),
        # Flexible for new version of clevref
        page = sub("^.*[^0-9]([0-9]+)[}][}]$", "\\1", x = ., perl = TRUE)
      )
    }
  
  chapter_pages <- 
    grep("newlabel\\{chap[:].*@cref", aux_contents, perl = TRUE, value = TRUE) %>%
    gsub("^.*[^0-9]([0-9]+)[}][}]$", "\\1", x = ., perl = TRUE)
  
  problem_smallboxes <- 
    smallbox_pages %>%
    .[page %in% chapter_pages] %>%
    .[smallbox_locations, on = "smallbox", nomatch=0L] %>%
    .[posy > 31000000]
  
  if (nrow(problem_smallboxes)) {
    .report_error(error_message = "Smallbox intrudes on chapter heading", 
                  advice = paste0("The smallbox with label ", 
                                  problem_smallboxes[["smallbox"]][1],
                                  "on page ",
                                  problem_smallboxes[["page"]][1], 
                                  " appears to be too high. Considering ",
                                  "changing \\begin{smallbox} to \\begin{verysmallbox}[p]"))
    stop("Smallbox ", problem_smallboxes[["smallbox"]][1], " intrudes on chapter heading")
  }
}

