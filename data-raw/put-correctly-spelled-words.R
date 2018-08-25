library(magrittr)
library(hutils)
correctly_spelled_words_txt <- readLines("./data-raw/correctly_spelled_words.txt", skipNul = TRUE)
correctly_spelled_words_txt <- correctly_spelled_words_txt[order(correctly_spelled_words_txt)]

verboten <- c(correctly_spelled_words_txt,
              "anywheres")

correctly_spelled_words_fixed_txt <- NULL

if (identical(Sys.getenv("USERNAME"), "hughp") &&
    path.expand("~") == "C:/Users/hughp/Documents") {
  for (xd in list.dirs(path = "~", recursive = FALSE, full.names = TRUE)) {
    if (length(dir(path = xd, pattern = ".tex$"))) {
      for (file.tex in dir(path = xd, pattern = ".tex$", full.names = TRUE)) {
        Report.tex <- readr::read_lines(file.tex)
        additions <-
          Report.tex %>%
          .[startsWith(., "% add_to_dictionary:")] %>%
          sub("% add_to_dictionary: ", "", x = ., fixed = TRUE) %>%
          strsplit(" ", fixed = TRUE) %>%
          unlist %>%
          .[. %notin% verboten] %>%
          grep("^[A-Za-z]+$", x = ., value = TRUE)
        cat(basename(xd),
            if (length(additions)) crayon::green(length(additions)) else crayon::silver(length(additions)),
            "\n",
            sep = "\t")
        correctly_spelled_words_fixed_txt <- c(correctly_spelled_words_fixed_txt, additions)
      }

    }
  }
}

readr::write_lines(sort(unique(correctly_spelled_words_fixed_txt)), "./data-raw/fixed-words.txt")

CORRECTLY_SPELLED_WORDS_CASE_SENSITIVE <-
  grep("[A-Z]", correctly_spelled_words_txt, value = TRUE)

correctly_spelled_words <- setdiff(correctly_spelled_words_txt, CORRECTLY_SPELLED_WORDS_CASE_SENSITIVE)


grattan_correctly_spelled_words <- correctly_spelled_words
grattan_CORRECTLY_SPELLED_WORDS_CASE_SENSITIVE <- CORRECTLY_SPELLED_WORDS_CASE_SENSITIVE

devtools::use_data(grattan_correctly_spelled_words,
                   grattan_CORRECTLY_SPELLED_WORDS_CASE_SENSITIVE,
                   correctly_spelled_words_fixed_txt,
                   overwrite = TRUE)
