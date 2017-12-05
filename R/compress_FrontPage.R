#' Compress the FrontPage
#' @param path The path of a report, under which is a file \code{FrontPage/FrontPage.pdf}.
#' @param error If the compression does not succeed (for what ever reason), should an error be given? 
#' By default, \code{FALSE}.
#' @param msg.if.error Should a message be printed at all, if \code{error} is \code{FALSE}? If \code{TRUE}, the default,
#' the error message is printed via \code{message}, not \code{stop}, so allows subsequent code to continue. Otherwise, 
#' provided \code{error = FALSE}, the function returns \code{NULL}, invisibly.
#' @return 
#' Called for its side-effect: attempts to compress \code{./FrontPage/FrontPage.pdf} using Ghostscript, in order to 
#' make the front page prettier than normal, but still a reasonable size.
#' 
#' Returns \code{NULL}, invisibly.
#' @export

compress_FrontPage <- function(path = ".", error = FALSE, msg.if.error = TRUE) {
  get_wd <- getwd()
  setwd(path)
  on.exit(get_wd)
  
  if (file.exists(file.path("FrontPage", "FrontPage.pdf"))) {
    setwd("FrontPage")
    if (nzchar(Sys.getenv("R_GSCMD"))) {
      if (.Platform$OS.type == "windows") {
        shell(paste(shQuote(Sys.getenv("R_GSCMD")), 
                    "-sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/printer",
                    "-dNOPAUSE -dQUIET -dBATCH", "-sOutputFile=FrontPage-gs.pdf",
                    "FrontPage.pdf"))
      } else {
        system(paste(shQuote(Sys.getenv("R_GSCMD")),
                     "-sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/printer",
                     "-dNOPAUSE -dQUIET -dBATCH", "-sOutputFile=FrontPage-gs.pdf",
                     "FrontPage.pdf"))
      }
      if (file.exists("FrontPage-gs.pdf")) {
        remove_success <- file.remove("FrontPage.pdf")
        rename_success <- file.rename("FrontPage-gs.pdf", "FrontPage.pdf")
        if (!AND(remove_success, rename_success)) {
          err.msg <-
            paste0("Did not succesfully run. ",
                   "\n",
                   "Check gs installation and that R can access FrontPage/FrontPage.pdf.")
          if (error) {
            stop(err.msg)
          } else {
            if (msg.if.error) {
              message(err.msg)
            }
          }
        }
      }
    } else {
      err.msg <-
        paste0("Ghostscript is required but R_GSCMD is not set. ",
               "Ensure Ghostscript is installed then set R_GSCMD, e.g.\n\t",
               "Sys.setenv(R_GSCMD = 'C:/Program Files/gs/gs9.20/bin/gswin64c.exe')")
      if (error) {
        stop(err.msg)
      } else {
        if (msg.if.error) {
          message(err.msg)
        }
      }
    }
  } else {
    err.msg <- "FrontPage/FrontPage.pdf does not exist."
    if (error) {
      stop(err.msg)
    } else {
      message(err.msg)
    }
  }
  setwd(get_wd)
  invisible(NULL)
}
