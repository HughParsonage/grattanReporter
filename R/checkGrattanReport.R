#' Check Grattan Report
#' @description Check Grattan reports written is LaTeX for typing errors, significant warnings,
#' and inconsistent style.
#' @param path Path to search for the tex source file.
#' @param compile Should \code{pdflatex} be run on the report so the logs may be checked?
#' @param pre_release Should the document be assumed to be final? Runs additional checks.
#' @param release Should a final pdf be prepared for publication?
#' @param .proceed_after_rerun On the occasions where infinitely many passes of \code{pdflatex}
#' are required, include this to skip the error. Note that this will result in false cross-references
#' or incompletely formatted bibliographies.
#' @param .no_log Make no entry in the log file on the check's outcome. (Deprecated.)
#' @param .log_psv If \code{TRUE}, return a table of the error details as pipe separated table.
#' @param embed If \code{FALSE}, not attempt to embed the fonts using Ghostscript is attempted. Useful if Ghostscript cannot easily be installed.
#' Set to \code{TRUE} for debugging or repetitive use (as in benchmarking).
#' @param rstudio Use the RStudio API if available.
#' @param update_grattan.cls Download \code{grattan.cls} from \url{https://github.com/HughParsonage/grattex/blob/master/grattan.cls}?
#' Set to \code{FALSE} when checking the \code{grattex} repo itself. Also downloads the logos associated with the repository.
#' @param filename If provided, the \code{.tex} file inside \code{path} to check. By default, \code{NULL} so the
#' set to the sole \code{.tex} file within \code{path}.
#' @return Called for its side-effect.
#' @export checkGrattanReport checkGrattanReports
#' @import data.table
#' @importFrom hutils if_else
#' @importFrom hutils coalesce
#' @importFrom hutils %notin%
#' @importFrom hutils %pin%
#' @importFrom magrittr %>%
#' @importFrom magrittr and
#' @importFrom magrittr or
#' @importFrom magrittr not
#' @importFrom methods hasArg
#' @importFrom clisymbols symbol
#' @importFrom crayon green red bgGreen bgRed bold
#' @importFrom grDevices embedFonts
#' @importFrom utils download.file
#' @importFrom stats complete.cases
#' @importFrom stats setNames
#' @import TeXCheckR
#'
#' @details
#' Options in the package
#' \describe{
#' \item{\code{grattanReporter.quiet.progress = FALSE}}{Can be set to \code{TRUE} to avoid printing the output during the check.}
#' }
#'
#'
#'
#'

checkGrattanReport <- function(path = ".",
                               compile = FALSE,
                               pre_release = FALSE,
                               release = FALSE,
                               .proceed_after_rerun,
                               .no_log = TRUE,
                               .log_psv = FALSE,
                               embed = TRUE,
                               rstudio = FALSE,
                               update_grattan.cls = pre_release,
                               filename = NULL) {
  if (Sys.getenv("TRAVIS") == "true") {
    print(utils::packageVersion("grattanReporter"))
  }

  if (release && (!pre_release || !compile)){
    stop("release = TRUE but pre_release and compile are not both TRUE also.")
  }

  if (pre_release && !compile){
    stop("pre_release = TRUE but compile = FALSE.")
  }

  if (compile && Sys.which("pdflatex") == "") {
    stop("pdflatex not on system path. Ensure you have LaTeX installed (MiKTeX, MacTeX, TeXLive) and that it is searchable on PATH. ",
         "(Did you install but leave programs open?)")
  }

  if (embed && release) {
    gsexe <- tools::find_gs_cmd()
    if (!nzchar(gsexe)) {
      stop("Ghostscript is required but R_GSCMD is not set. Ensure Ghostscript is installed then set R_GSCMD, e.g.\n\t",
           "Sys.setenv(R_GSCMD = 'C:/Program Files/gs/gs9.20/bin/gswin64c.exe')")
    }
  }

  if (getOption("grattanReporter.quiet.progress", FALSE)) {
    cat <- function(...) NULL
  }



  current_wd <- getwd()
  setwd(path)
  on.exit(setwd(current_wd))



  if (is.null(filename)) {
    tex_file <- dir(path = ".", pattern = "\\.tex$")
    if (length(tex_file) != 1L) {
      stop("`path` must contain one and only one .tex file.")
    }
    filename <- tex_file[[1]]
  }

  if (!dir.exists("./travis/grattanReport/")){
    stop("./travis/grattanReport/ does not exist. Create this directory and try again.")
  }

  if (missing(compile) &&
      missing(pre_release) &&
      missing(release) &&
      !is.null(the_release_status <- release_status(filename))) {
    if ("compile" %in% the_release_status) {
      compile = TRUE
    }

    if ("pre_release" %in% the_release_status) {
      pre_release = TRUE
    }

    if ("release" %in% the_release_status) {
      release = TRUE
    }
  }

  file_remove <- function(x) {
    file.exists(x) && file.remove(x)
  }

  if (compile) {
    file.create("./travis/grattanReport/compile")
  } else {
    file_remove("./travis/grattanReport/compile")
  }
  if (pre_release) {
    file.create("./travis/grattanReport/pre_release")
  } else {
    file_remove("./travis/grattanReport/pre_release")
  }
  if (release) {
    file.create("./travis/grattanReport/release")
  } else {
    file_remove("./travis/grattanReport/release")
  }







  if (pre_release &&
      update_grattan.cls &&
      !identical(tolower(Sys.getenv("TRAVIS_REPO_SLUG")), "hughparsonage/grattex")){
    download_failure <-
      download.file("https://raw.githubusercontent.com/HughParsonage/grattex/master/grattan.cls",
                    destfile = "grattan.cls",
                    quiet = TRUE)

    if (download_failure) {
      stop("grattan.cls failed to download from master branch (and be updated).")
    }
  }

  if (update_grattan.cls) {
    hutils::provide.dir("logos")
    logos <-
      c("Bhp.pdf", "GrattanSVGLogo.pdf", "UOM-Pos_S_PMS.pdf", "Vic_Gov_Logo-2016.pdf", "aus-gov-logo-stacked-black.pdf")

    for (l in logos) {
      download_logos_failure <-
        download.file(paste0("https://raw.githubusercontent.com/HughParsonage/grattex/master/logos/", l),
                      destfile = file.path(".", "logos", l),
                      mode = "wb",
                      quiet = TRUE)

      if (download_logos_failure) {
        stop(l, " failed to download from master branch. (May be out-of-date.)")
      }
    }
  }

  if (release){
    if (!dir.exists("RELEASE")){
      dir.create("RELEASE")
    } else {
      if (length(list.files(path = "./RELEASE", pattern = "\\.pdf$")) > 0){
        # If there are any files in the RELEASE directory, move them (file.rename)
        # to a subdirectory named by their creation time.
        invisible({
          lapply(list.files(path = "./RELEASE", pattern = "\\.pdf$", full.names = TRUE),
                 function(file){
                   date_created <- format(file.info(file)$ctime, format = "%Y-%m-%d-%H%M")
                   if (!dir.exists(file.path("RELEASE", date_created))){
                     dir.create(file.path("RELEASE", date_created))
                   }
                   file.rename(file, file.path("RELEASE", date_created, basename(file)))
                 })
        })
        message("RELEASE contained pdf files. These have been moved.")
      }
    }
  }

  if (pre_release){
    if (!dir.exists("PRE-RELEASE")){
      dir.create("PRE-RELEASE")
    } else {
      if (length(list.files(path = "./PRE-RELEASE", pattern = "\\.pdf$")) > 0){
        invisible(lapply(list.files(path = "./PRE-RELEASE", pattern = "\\.pdf$", full.names = TRUE), file.remove))
        message("PRE-RELEASE contained pdf files. These have been deleted.")
      }
    }
  }

  if (.no_log) {
    if (.log_psv) {
      .report_error <- function(...) {
        if (hasArg(file)) {
          report2console(...,
                         rstudio = rstudio,
                         log_file = "./travis/grattanReport/error-log.psv",
                         log_file_sep = "|")
        } else {
          report2console(...,
                         rstudio = rstudio,
                         file = filename,
                         log_file = "./travis/grattanReport/error-log.psv",
                         log_file_sep = "|")
        }
      }
    } else {
      .report_error <- function(...){
        report2console(..., rstudio = rstudio, file = filename)
      }
    }
  } else {
    if (.log_psv) {
      stop("`.log_psv = TRUE`, yet `.no_log = TRUE`. ",
           "They must be mutually exclusive.")
    }
    .report_error <- function(...) {
      if (hasArg(file)) {
        report2console(...,
                       log_file = "./travis/grattanReport/error-log.tsv",
                       rstudio = rstudio)
      } else {
        report2console(...,
                       log_file = "./travis/grattanReport/error-log.tsv",
                       rstudio = rstudio,
                       file = filename)
      }
    }
  }

  report_name <- gsub("^(.*)\\.tex$", "\\1", filename)

  # Actual checking begins here
  notes <- 0L


  check_preamble(filename, .report_error, pre_release = pre_release, release = release)

  the_authors <-
    get_authors(filename)

  if (length(the_authors) == 0L){
    stop("No authors detectable in document.")
  }

  cat("I see the following as authors:",
      the_authors, sep = "\n   ")

  cat("\n")

  cat(green(symbol$tick, "Preamble OK.\n"), sep = "")

  check_input <- function(filename){

    inputs <- inputs_of(filename)
    if (length(inputs) > 0){
      for (input in inputs) {
        if (rstudio) {
          .report_error <- function(...) report2console(..., file = input, rstudio = TRUE)
        }

        check_input(input)
        cat(input)

        check_cite_pagerefs(input, .report_error = .report_error)
        cat(".")
        check_escapes(input, .report_error = .report_error)
        cat(".")
        check_dashes(input, .report_error = .report_error)
        cat(".")
        check_spacing(input, .report_error = .report_error)
        cat(".")
        check_quote_marks(input, .report_error = .report_error)
        cat(".")
        check_footnote_typography(input, .report_error = .report_error)
        cat(".")
        check_labels(input)
        cat(".")
        check_literal_xrefs(input, .report_error = .report_error)
        cat(".")
        check_xrefs(input, .report_error = .report_error)
        cat(".")
        check_sentence_ending_periods(input, .report_error = .report_error)
        cat(".")
        TeXCheckR:::check_unclosed_parentheses(input, rstudio = rstudio)
        cat(".")
        cat(" OK\n")
      }
    }
  }
  check_input(filename)

  check_cite_pagerefs(filename, .report_error = .report_error)
  cat(green(symbol$tick, "Cite and pagerefs checked.\n"), sep = "")

  check_escapes(filename, .report_error = .report_error)
  cat(green(symbol$tick, "No unescaped $.\n"))

  check_dashes(filename, .report_error = .report_error)
  cat(green(symbol$tick, "Dashes correctly typed.\n"))

  check_spacing(filename, .report_error = .report_error)
  cat(green(symbol$tick, "No spacing issues around abbreviations.\n"))

  check_quote_marks(filename, .report_error = .report_error)
  cat(green(symbol$tick, "Opening quotes correctly typed.\n"))

  check_footnote_typography(filename, .report_error = .report_error)
  cat(green(symbol$tick, "Footnote typography checked.\n"))

  check_literal_xrefs(filename, .report_error = .report_error)
  check_xrefs(filename)
  cat(green(symbol$tick, "No repetitive xrefs.\n"))

  check_sentence_ending_periods(filename, .report_error = .report_error)
  cat(green(symbol$tick, "Sentence-ending periods ok.\n"))

  TeXCheckR:::check_unclosed_parentheses(filename, rstudio = rstudio)
  cat(green(symbol$tick, "No obviously unbalanced parentheses.\n"))

  # To check the bibliography
  bib_files <-
    read_lines(filename) %>%
    .[grepl("\\addbibresource", ., fixed = TRUE)] %>%
    trimws %>%
    gsub("^\\\\addbibresource[{](.+\\.bib)[}]$", "\\1", .)

  bib_files_still_ok <- logical(length(bib_files))
  i <- 0
  for (bib_file in bib_files) {
    i <- i + 1
    hutils::provide.dir(paste0("travis/grattanReport/md5/", dirname(bib_file)))
    full_bib_file <- normalizePath(bib_file)
    md5 <- as.character(tools::md5sum(normalizePath(bib_file)))
    md5_record <- file.path("travis", "grattanReport", "md5", bib_file)
    if (file.exists(md5_record)) {
      md5.ok <- scan(file.path("travis", "grattanReport", "md5", bib_file),
                     what = character(),
                     n = 1,
                     quiet = TRUE)
      if (md5 == md5.ok) {
        bib_files_still_ok[i] <- TRUE
        cat(green(symbol$tick, bib_file, "previously validated.\n"))
        next
      }
    } else {
      validate_bibliography(file = bib_file, rstudio = rstudio)
    }
    if (grepl("ropbox", full_bib_file, fixed = TRUE)) {
      cat("N: Not marking MD5 sum of valid file as the project",
          "\n   is on Dropbox and writing files directly is unwise.",
          "\n")
    } else {
      cat(md5, file = md5_record)
    }
    cat(green(symbol$tick, bib_file, "validated.\n"))
  }

  if (all(bib_files_still_ok)) {
    cat(green(symbol$tick, "Duplicates previously checked.\n"))
  } else {
    tryCatch(any_bib_duplicates(bib.files = bib_files),
             error = function(e) {
               for (bib_file in bib_files) {
                 file.remove(file.path("travis", "grattanReport", "md5", bib_file))
               }
               stop(e)
             })
    cat(green(symbol$tick, "No obvious duplicates in bibliography.\n"))
  }

  check_spelling(filename,
                 .report_error = .report_error,
                 known.correct = c(grattan_correctly_spelled_words,
                                   grattan_CORRECTLY_SPELLED_WORDS_CASE_SENSITIVE),
                 ignore_spelling_in = c("topref"),
                 ignore_spelling_in_nth = list("Chaprefrange" = 1:2,
                                               "Chapsref" = 1:2,
                                               "Chaprefand" = 1:2),
                 pre_release = pre_release,
                 bib_files = bib_files,
                 rstudio = rstudio)
  if (!pre_release &&
      exists("authors_in_bib_and_doc") &&
      not_length0(authors_in_bib_and_doc)) {
    notes <- notes + 1L

    authors_in_bib_and_doc <-
      authors_in_bib_and_doc[seq.int(1L, min(length(authors_in_bib_and_doc), 5L))]

    cat("NOTE: Skipped spell check for authors in bibliography.",
        "Please use \\citeauthor{} (preferred) or include the line\n\n\t% add_to_dictionary:", paste0(authors_in_bib_and_doc, collapse = " "),
        "\n\nin your .tex file if the names have been spelled correctly. (Author names will NOT be skipped at pre-release.)\n")
  }
  cat(green(symbol$tick, "Spellcheck complete.\n"))

  check_labels(filename)

  cat(green(symbol$tick, "Labels checked.\n"))

  unrefd_figs_tbls <- figs_tbls_unrefd(filename, check.labels = FALSE)

  if (is.null(unrefd_figs_tbls)) {
    cat(green(symbol$tick, "All figures and tables have a Xref.\n"))
  } else {
    if (!pre_release) {
      notes <- notes + 1L
      cat(if (compile) "WARNING:" else  "NOTE:",
          "Not all figures and tables referenced. ",
          unrefd_figs_tbls)
    } else {
      .report_error(error_message = "Unreferenced figure or table",
                    advice = paste0("Couldn't find a xref to ", unrefd_figs_tbls, "."))

      stop("Couldn't find a xref to ", unrefd_figs_tbls, ".")
    }
  }

  cat("\n")

  if (compile){
    options("tinytex.clean" = FALSE)
    full_dir_of_path <- getwd()
    md5_filename <- paste0(substr(tools::md5sum(filename), 0, sample.int(10, size = 1) + 2),
                           substr(tools::md5sum(bib_file), 0, sample.int(10, size = 1) + 2))
    temp_dir <- file.path(tempdir(), md5_filename)
    md5_iter <- 1
    while (dir.exists(temp_dir)){
      md5_filename <- gsub("^(.)(.+)$", "\\2\\1", md5_filename)
      temp_dir <- file.path(tempdir(), md5_filename)
      md5_iter <- md5_iter + 1
      if (md5_iter > 30){
        cat(tempdir())
        stop("Emergency stop: temporary directory full.")
      }
    }
    dir.create(temp_dir)
    move_to(temp_dir)
    if (file.exists(gsub("\\.tex", ".pdf", filename))){
      file.remove(gsub("\\.tex", ".pdf", filename))
    }

    cat("   Invoking pdflatex... ")
    current_warn <-  getOption("warn")
    on.exit(options(warn = current_warn), add = TRUE)
    options(warn = 2)
    WIN <- .Platform$OS.type == "windows"
    do_pdflatex <- function() {
      if (WIN) {
        shell(paste("pdflatex -interaction=batchmode -halt-on-error", filename), intern = TRUE)
      } else {
        system2(command = "pdflatex",
                args = c("-interaction=batchmode", "-halt-on-error", filename),
                stdout = tempfile())
      }
    }

    do_biber <- function(file.tex) {
      file. <- sub("\\.tex$", "", file.tex)
      if (WIN) {
        shell(paste("biber --onlylog -V", file.), intern = TRUE)
      } else {
        system2(command = "biber",
                args = c("--onlylog", "-V", file.),
                stdout = tempfile())
      }
    }

    do_pdflatex()
    cat("complete.\n")
    cat("   Invoking biber...\n")
    do_biber(filename)

    check_biber()
    cat(green(symbol$tick, "biber validated citations.\n"))

    cat("   Rerunning pdflatex. Starting pass number 1")
    do_pdflatex()

    cat(" 2 ")
    do_pdflatex()

    log_result <- check_log(check_for_rerun_only = TRUE)
    reruns_required <- 2
    while (pre_release && !is.null(log_result) && log_result == "Rerun LaTeX."){
      cat(reruns_required + 1, " ", sep = "")
      do_pdflatex()
      log_result <- check_log(check_for_rerun_only = TRUE)

      reruns_required <- reruns_required + 1
      if (!missing(.proceed_after_rerun) && reruns_required > .proceed_after_rerun){
        cat("\nW: Skipping checking of LaTeX rerun.")
        break
      }

      if (missing(.proceed_after_rerun) && reruns_required > 3){
        check_log(check_for_rerun_only = FALSE)
      }
    }
    cat("\n")
    cat(green(symbol$tick, ".log file checked.\n"))

    if (!length(dir(pattern = "\\.aux$"))) {
      cat(filename)
      if (requireNamespace("tinytex", quietly = TRUE)) {
        tinytex::pdflatex(filename, clean = FALSE)
      }
    }
    check_smallbox_caption_positions()

    if (pre_release) {
      CenturyFootnote_suspect <- NULL
      if (getOption("check.CenturyFootnote", TRUE)) {
        check_CenturyFootnote()
        if (!CenturyFootnote_suspect){
          cat(green(symbol$tick, "\\CenturyFootnote correctly placed.\n"))
        } else {
          notes <- notes + 1
        }
      }

      if (release) {
        cat("Now preparing a release...\n")
        if (!dir.exists("RELEASE")){
          dir.create("RELEASE")
        }

        new_filename <-
          read_lines(filename) %>%
          grep("^\\\\title\\{", ., perl = TRUE, value = TRUE) %>%
          gsub("^\\\\title\\{(.+)\\}$", "\\1", ., perl = TRUE) %>%
          gsub("[^A-Za-z]", "-", ., perl = TRUE) %>%
          paste0(".pdf")

        # Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.20/bin/gswin64c.exe")
        if (embed){
          embedFonts(gsub("\\.tex$", ".pdf", filename),
                     outfile = file.path(full_dir_of_path, "RELEASE", new_filename))
          cat(green(symbol$tick, "Fonts embedded.\n"))
        } else {
          file.copy(gsub("\\.tex$", ".pdf", filename),
                    file.path(full_dir_of_path, "RELEASE", new_filename))
          cat("NOTE: Fonts not embedded, as requested.\n")
          notes <- notes + 1
        }

      } else {
        file.copy(paste0(report_name, ".pdf"),
                  file.path(full_dir_of_path,
                            "PRE-RELEASE",
                            paste0(report_name, ".pdf")))
      }

      setwd(full_dir_of_path)
      options(warn = current_warn)
      cat("\n")
    }
  }

  cat(bgGreen(symbol$tick, "Report checked.\n"))
  if (pre_release){
    if (release){
      cat("Releaseable pdf written to ", file.path(path, "RELEASE", new_filename))
      cat("\nDONE.")

      lines <- read_lines(filename)
      if (!any(grepl("FrontPage", lines))){
        cat("\n\nNOTE: Did you forget to add the FrontPage to \\documentclass{grattan}?")
      }
      if (any(grepl("XX", lines[!or(grepl("tabularx", lines, perl = TRUE),
                                    grepl("^%", lines, perl = TRUE))]))){
        cat("\nWARNING: Found XX in document.")
      }
    } else {
      cat("Pre-release version written to ", file.path(path, "PRE-RELEASE", gsub("\\.tex$", ".pdf", filename)))
      cat("\nDONE.")
    }
  }

  if (!.no_log){
    if (file.exists("./travis/grattanReport/error-log.tsv")){
      prev_build_status <-
        fread("./travis/grattanReport/error-log.tsv") %>%
        last %>%
        .[["build_status"]]

      if (is.null(prev_build_status) ||
          prev_build_status %notin% c("None", "Broken", "Still failing")) {
        prev_build_status <- "None"
      }

      append <- TRUE
    } else {
      prev_build_status <- "None"
      append <- FALSE
    }

    if (prev_build_status %in% c("None", "Broken", "Still failing")){
      build_status <- "Fixed"
    } else {
      build_status <- "OK"
    }


    data.table(Time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
               build_status = build_status,
               error_message = "NA") %>%
      fwrite("./travis/grattanReport/error-log.tsv",
             sep = "\t",
             append = append)
    if (notes > 0) {
      if (notes > 1) {
        cat("\n\tThere were", notes, "notes.")
      } else {
        cat("\n\tThere was 1 note.")
      }
    }
  }

  invisible(NULL)
}


#' @rdname checkGrattanReport
checkGrattanReports <- function(path = ".",
                                compile = FALSE,
                                pre_release = FALSE,
                                release = FALSE,
                                .proceed_after_rerun,
                                .no_log = TRUE,
                                embed = TRUE,
                                rstudio = FALSE,
                                update_grattan.cls = TRUE) {
  current_wd <- getwd()
  setwd(path)
  on.exit(setwd(current_wd))

  for (filename in dir(pattern = "\\.tex$")) {
    cat("==== ", tools::file_path_sans_ext(filename), " ====\n")
    checkGrattanReport(compile = compile,
                       pre_release = pre_release,
                       release = release,
                       .no_log = .no_log,
                       embed = embed,
                       rstudio = rstudio,
                       update_grattan.cls = update_grattan.cls,
                       filename = filename)
  }
  setwd(current_wd)
}





