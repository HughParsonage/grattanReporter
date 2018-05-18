#' Convert word to tex
#' @param path Path containing the \code{.docx} file.
#' @param overwrite (logical, default: \code{TRUE}) Overwrite existing tex files.
#' @export word2tex
#'


word2tex <- function(path = ".", overwrite = TRUE) {
  if (!nzchar(Sys.which("pandoc"))) {
    stop("pandoc not found on the system path.")
  }

  current_wd <- getwd()
  setwd(normalizePath(path.expand(path)))
  on.exit(setwd(current_wd))

  file.docx <- dir(path = ".",
                   pattern = "\\.docx$",
                   full.names = TRUE)

  if (length(file.docx) != 1L) {
    if (length(file.docx) == 0L) {
      stop("No .docx files found in `path`.")
    } else {
      if (any(startsWith(basename(file.docx), "~$"))) {
        warning(".docx file found starting with '~$'. ",
                "Likely reason: file is open in Word. ",
                "Close the file and try again.")
      }
      stop("Multiple .docx files found in `path`.")
    }
  }

  out.tex <- sprintf("%s.tex", tools::file_path_sans_ext(file.docx))

  if (!overwrite && file.exists(out.tex)) {
    stop("`overwrite = FALSE` but `", out.tex, "` is present in `path`.")
  }

  if (tolower(.Platform$OS.type) == "windows") {
    shell(sprintf("pandoc --wrap=none --top-level=chapter -s %s -o %s", file.docx, out.tex))
  } else {
    system(sprintf("pandoc --wrap=none --top-level=chapter -s %s -o %s", file.docx, out.tex))
  }

  out_tex_lines <- read_lines(out.tex)

  preamble <- !cummax(grepl("\\begin{document}", out_tex_lines, fixed = TRUE))
  preamble_lines <- out_tex_lines[preamble]

  title <- extract_LaTeX_argument(preamble_lines, "title")


  out_tex_lines[preamble] <- c("\\documentclass{grattan}",
                               "\\usepackage{longtable}",
                               "",
                               "",
                               rep_len("", sum(preamble) - 4L))

  out_tex_lines <- gsub("\\\\texorpdfstring\\{.*?\\}", "", out_tex_lines, perl = TRUE)
  out_tex_lines <- gsub("\\\\section\\{\\s*[0-9]\\.[0-9](.*?)\\}", "\\\\section{\\1}", out_tex_lines)
  out_tex_lines <- gsub("\\\\subsection\\{\\s*[0-9]\\.[0-9](.*?)\\}", "\\\\subsection{\\1}", out_tex_lines)


  out_tex_lines <- gsub("(\\\\includegraphics[^\\{]*\\{.*?\\})(.*)$", "%% \\1\n\\2", out_tex_lines)
  write_lines(out_tex_lines, out.tex)
  setwd(current_wd)
}



