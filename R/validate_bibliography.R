#' Validate bibliography according Grattan style
#' @param path Containing the bib file.
#' @param file The bib file if specified.
#' @param .report_error How errors should be reported.
#' @param rstudio Use the RStudio API?
#' @return \code{NULL} if bibliography validated.
#' @export


validate_bibliography <- function(path = ".", file = NULL, .report_error, rstudio = FALSE) {
  if (missing(.report_error)){
    .report_error <- function(...) report2console(file = file, ..., rstudio = rstudio)
  }

  if (is.null(file)){
    bib_files <- dir(path = path, pattern = "\\.bib$", full.names = TRUE)
    stopifnot(length(bib_files) == 1L)
    bib_file <- bib_files[[1]]
  } else {
    bib_file <- file
  }

  bib <-
    read_lines(bib_file) %>%
    trimws

  # Protect from misshapen bibliography entries
  is_key <- grepl("^@", bib, perl = TRUE)
  is_field <- grepl("^[a-z]+\\s* = \\{", bib, perl = TRUE)
  is_closing = bib == "}"
  is_null <- bib == ""

  if (!all(is_key | is_field | is_closing | is_null)){
    bad_lines <- which(!(is_key | is_field | is_closing | is_null))
    first_bad_line <- bad_lines[[1]]
    .report_error(line_no = first_bad_line,
                  context = bib[first_bad_line],
                  error_message = paste0(bib_file, " contains line which is neither a key, nor field, nor closing.")
                  ,advice = paste0("Ensure every line in bibliography is one of the following:",
                                   "\n\n",
                                   "An entry type,\n\t",
                                    "@<EntryType>\n",
                                   "\n",
                                   "a field,\n\t",
                                  "field = {  \t<- including spaces around the equals sign\n",
                                   "\n",
                                  "a single closing brace,\n\t",
                                    "}\n",
                                    "\n",
                                    "or a blank line.\n",
                                    "\n\n",
                                    "If you can, run\n\t",
                                    "lint_bib('", bib_file, "')")
                  )
    stop(bib_file, " contains line which is neither a key, nor field, nor closing.")
  }

  bib <-
    bib[!grepl("% Valid", bib, fixed = TRUE)]

  if (any(grepl(".[}]$", bib, perl = TRUE))){
    line_no <- grep(".[}]$", bib, perl = TRUE)[[1]]
    .report_error(line_no = line_no,
                  context = bib[line_no],
                  error_message = "Each field line in .bib must end with a comma (to allow reordering).")
    stop("Each field line in .bib must end with a comma (to allow intra-entry reordering).")
  }

  # Abbreviated names
  inst_pattern <-
    paste0("^\\s+(author).*",
           "(?:",
           "(?:(?:Australian )?Bureau of Statistics)",
           "|",
           "(?:Australian Labor Party)",
           "|",
           "(?:Australian Institute of Health and Welfare)",
           "|",
           "(?:Australian Taxation Office)",
           "|",
           "(?:Productivity Commission)",
           "|",
           "(?:World Health Organi[sz]ation)",
           ")")

  if (any(grepl(inst_pattern, bib, perl = TRUE))){
    first_bad <-
      grep(inst_pattern,
           bib,
           perl = TRUE,
           value = TRUE) %>%
      .[1]
    cat(first_bad)
    stop(cat(crayon::bgRed(symbol$cross)), "Institutional authors should be abbreviated.")
  }


  # Ensure URLs suggesting a newspaper article only
  # appear in @Article types:
  bib_just_keys_and_urls <-
    grep("^(@|(\\s+(url)))",
         bib,
         perl = TRUE,
         value = TRUE)

  urls_not_articles <- c("http://images.theage.com.au/file/2014/07/31/5639573/Help_interest_rate_options_report.pdf")

  bib_just_keys_and_urls <-
    gsub("http://images.theage.com.au/file/2014/07/31/5639573/Help_interest_rate_options_report.pdf",
         "",
         bib_just_keys_and_urls,
         fixed = TRUE)

  newspapers_pattern <-
    paste0("^(url).*",
           "(",
           "(((theguardian)|(afr))\\.com)",
           "|",
           "(((theaustralian)|(theage)|(smh)|(canberratimes)|(greatlakesadvocate))\\.com\\.au)",
           "|",
           "(theconversation\\.((edu\\.au)|(com)))",
           "|",
           "(insidestory\\.org\\.au)",
           ")")

  should_be_Articles <-
    grep(newspapers_pattern,
         bib_just_keys_and_urls,
         perl = TRUE) - 1

  arent_Articles_but_should_be <-
    !grepl("@Article",
           bib_just_keys_and_urls[should_be_Articles],
           fixed = TRUE)

  if (any(arent_Articles_but_should_be)){
    bib_just_keys_and_urls %>%
      .[should_be_Articles] %>%
      .[arent_Articles_but_should_be] %>%
      .[1] %>%
      cat
    stop(cat(crayon::bgRed(symbol$cross)), "URL suggests the article type should be used.")
  }

  # Once we have verified all are articles, check the right journal has been included.
  just_key_journal_urls <-
    bib %>%
    .[grepl("^(@|(journal)|(url))", ., perl = TRUE) | bib == "}"]

  both_url_and_journal <- entry_no <- group_by <- is_article <- is_newspaper <-
    journal <- journal_actual <- journal_from_url <- text <- NULL

  journal_actual_vs_journal_expected <-
    data.table(text = just_key_journal_urls) %>%
    .[, entry_no := cumsum(grepl("^@", text))] %>%
    .[, is_article := any(grepl("^@Article", text)), by = entry_no] %>%
    # Journal + URL occur iff
    # @
    # TRUE
    # TRUE
    # }
    .[, both_url_and_journal := .N == 4L, by = entry_no] %>%
    .[, is_newspaper := any(grepl(newspapers_pattern, text, perl = TRUE)), by = entry_no] %>%
    .[is_article & both_url_and_journal &is_newspaper] %>%
    .[, .(key = grep("^@Article", text, perl = TRUE, value = TRUE),
          url = gsub("^url.*[{](.*)[}],?$",
                     "\\1",
                     text[grepl("^url", text, perl = TRUE)],
                     perl = TRUE),
          journal_actual = gsub("^journal.*[{](.*)[}],?$",
                                "\\1",
                                text[grepl("^journal", text, perl = TRUE)],
                                perl = TRUE),
          journal_from_url = gsub(paste0("^.*(", newspapers_pattern, ").*$"),
                                  "\\3",
                                  text[grepl("^url", text, perl = TRUE)],
                                  perl = TRUE)),
      by = entry_no] %>%
    setkey(journal_from_url) %>%
    .[newspaper_by_url]

  incorrect_journal_entries <-
    journal_actual_vs_journal_expected[journal_actual != journal]

  if (nrow(incorrect_journal_entries) > 0){
    cat(red(symbol$cross), red("Inconsistent treatment of article journal.\n"))
    print(incorrect_journal_entries)
    .report_error(error_message = "",
                  context = paste0("In entry", "\n\t",
                                   incorrect_journal_entries[1][["key"]], "\n\n",
                                   "I see:", "\n\t",
                                   "url = {", incorrect_journal_entries[1][["url"]], "}", "\n\n",
                                   "which suggests", "\n\t",
                                   "journal = {", incorrect_journal_entries[1][["journal"]], "} ,", "\n\n",
                                   "but\n\t",
                                   "journal = {", incorrect_journal_entries[1][["journal_actual"]], "} ."))
    stop("In entry", "\n\t",
         incorrect_journal_entries[1][["key"]], "\n\n",
         "I see:", "\n\t",
         "url = {", incorrect_journal_entries[1][["url"]], "}", "\n\n",
         "which suggests", "\n\t",
         "journal = {", incorrect_journal_entries[1][["journal"]], "} ,", "\n\n",
         "but\n\t",
         "journal = {", incorrect_journal_entries[1][["journal_actual"]], "} .")
  }

  check_legislation <- function(bb){
    is_bill_title <-
      and(grepl("^(?:@Misc).*(?:Bill)",
                shift(bib, type = "lag"),
                perl = TRUE,
                ignore.case = TRUE),
          grepl("^(?:title).*(?:Bill)",
                bib,
                perl = TRUE,
                ignore.case = TRUE))

    if (any(!grepl("\\textup", bb[is_bill_title], fixed = TRUE))){
      .report_error(line_no = which(is_bill_title)[1],
                    context = bb[is_bill_title[1]],
                    error_message = "Bill title in upright font.")
      stop("When citing a Bill of Parliament, the title must be in upright font.", "\n",
           "Use\n\ttitle = {\\textup{...}},\n\t\t\t\t\tin the .bib file.")
    }

  }

  check_legislation(bib)

  ## Grattan Institute

  ## All TechReports should use the /report/ url
  ## All TechReports should have a number

  check_Grattan_entries <- function(trimmed_bib){

    techReport_at <- grepl("^@TechReport", trimmed_bib, perl = TRUE, ignore.case = TRUE)
    is_closing <- trimmed_bib == "}"

    is_TechReport <- techReport_at
    is_TechReport[!or(techReport_at, is_closing)] <- NA

    is_TechReport <- fill_blanks(is_TechReport)
    is_TechReport[is.na(is_TechReport)] <- FALSE

    is_GrattanReport_url <-
      grepl("^url.*https?[:]//grattan\\.edu\\.au", trimmed_bib, perl = TRUE)

    if (any(and(is_GrattanReport_url & is_TechReport,
                !grepl("https?[:]//grattan\\.edu\\.au/report/", trimmed_bib, perl = TRUE)))){
      line_nos <-
        which(and(is_GrattanReport_url & is_TechReport,
                  !grepl("grattan\\.edu\\.au/report/", trimmed_bib, perl = TRUE)))
      for (x in line_nos)
        cat(x, ": ", trimmed_bib[[x]], "\n")
      stop("URL to Grattan Report does not use https://grattan.edu.au/report/ domain.")
    }


    if (any(and(is_GrattanReport_url,
                grepl(".pdf", trimmed_bib, fixed = TRUE)))){
      stop("URLs to Grattan Report points to pdf. The URL should be of the landing page.")
    }

  }

  # check_Grattan_entries(bib)

  # Peter Goss or Pete Goss?
  if (OR(any(grepl("Pete Goss", bib, perl = TRUE)),
         any(grepl("Goss, Pete(?!r)", bib, perl = TRUE)))){
    line_no <- which(or(grepl("Pete Goss", bib, perl = TRUE),
                        grepl("Goss, Pete(?!r)", bib, perl = TRUE)))[[1]]
    .report_error(line_no = line_no,
                  context = bib[line_no],
                  error_message = "Use 'Peter Goss', not 'Pete Goss', in bibliography.")

    stop("Use 'Peter Goss', not 'Pete Goss', in bibliography.")
  }

  # dois should not include the top-level URL
  if (any(grepl("^\\s+(doi).*https?[:][/][/]", bib, perl = TRUE))){
    cat(grep("^\\s+(doi).*https?[:][/][/]", bib, perl = TRUE, value = TRUE)[[1]])
    stop("DOI entries must be in the form", "\n\t",
         "10.1787/9789264229945-en", "\n",
         "not", "\n\t",
         "http://dx.doi.org/10.1787/9789264229945-en")
  }

  just_years_and_dates <-
    grep("^((@)|(year)|(date)|[}])",
         bib,
         perl = TRUE,
         value = TRUE)

  year_date_same_entry <-
    and(grepl("^((year)|(date))", just_years_and_dates, perl = TRUE),
        grepl("^((year)|(date))", shift(just_years_and_dates, type = "lead"), perl = TRUE))

  if (any(year_date_same_entry)){
    bad_entry <-
      just_years_and_dates[which(year_date_same_entry)[[1]] + c(-1, 0, 1, 2)]
    cat(crayon::bgRed(symbol$cross),
        bad_entry[1], "\n\t",
        bad_entry[2], "\n\t",
        bad_entry[3], "\n\t",
        bad_entry[4], "\n")
    stop("Date and year should not both appear in bibliography.")
  }

  if (utils::packageVersion("TeXCheckR") > package_version("0.4.4")) {
    field <- value <- key <- NULL
    bib_DT <- fread_bib(file.bib = bib_file,
                        check.dup.keys = FALSE,
                        strip.braces = FALSE,
                        .report_error = .report_error)

    # Issue 75
    AG_keys <-
      bib_DT[field %in% c("author", "url")] %>%
      .[or(grepl("Attorney.General.s.Department",
                 value,
                 perl = TRUE,
                 ignore.case = TRUE),
           grepl("\\bag\\.gov\\.au", value, perl = TRUE))] %>%
      .[["key"]] %>%
      unique

    AG_authors <-
      bib_DT[key %in% AG_keys] %>%
      .[field == "author"] %>%
      .[["value"]]

    if (any(AG_authors != "{{Attorney-General's Department}}")) {
      the_entry <-
        bib_DT[key %in% AG_keys] %>%
        .[field == "author"] %>%
        .[value != "{{Attorney-General's Department}}"] %>%
        .[1]

      .report_error(line_no = the_entry[["line_no"]],
                    error_message = paste0("Author needs to be:\n\t",
                                           "{{Attorney-General's Department}}\n",
                                           "precisely."))
      stop("Attorney-General's Department: author")
    }

    AG_urls <-
      bib_DT[key %in% AG_keys] %>%
      .[field == "url"] %>%
      .[["value"]]

    if (any(!grepl("\\bag\\.gov\\.au", AG_urls, perl = TRUE))) {
      the_entry <-
        bib_DT[key %in% AG_keys] %>%
        .[field == "url"] %>%
        .[!is.na(value)] %>%
        .[nzchar(value)] %>%
        .[!grepl("\\bag\\.gov\\.au", value, perl = TRUE)] %>%
        .[1]

      .report_error(line_no = the_entry[["line_no"]],
                    error_message = paste0("Reference to a URL the Cth ",
                                           "Attorney-General's Department ",
                                           "but url did not contain ",
                                           ".ag.gov.au"))
      stop("Attorney-General's Department: url")
    }
  }





  invisible(NULL)
}
