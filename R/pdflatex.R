#' Run 'pdflatex' regardless of platform or TinyTeX presence
#' @param filename.tex The LaTeX file.
#' @param interaction The interaction mode of \code{pdflatex}.
#' @param halt_on_error If \code{FALSE}, do not halt on error.
#' @return Status code.


pdflatex <- function(filename.tex, interaction = "batchmode", halt_on_error = TRUE) {
  if (requireNamespace("tinytex", quietly = TRUE) && nzchar(Sys.which("tlmgr"))) {
    options("tinytex.clean" = TRUE)
    options("tinytex.bib_engine" = "biber")
    tinytex::latexmk(filename.tex, engine = "pdflatex", bib_engine = "biber")
  } else {
    if (WINDOWS()) {
      if (halt_on_error) {
        shell("pdflatex -interaction=%s -halt-on-error %s", interaction, filename.tex)
      } else {
        shell("pdflatex -interaction=%s %s", interaction, filename.tex)
      }
    } else {
      system2("pdflatex",
              args = c(sprintf("-interaction=%s", interaction),
                       if (halt_on_error) "-halt-on-error",
                       filename.tex),
              stdout = tempfile("pdflatexstdout", fileext = ".log"),
              stderr = tempfile("pdflatexstderr", fileext = ".log"))
    }
  }
}



