.onLoad <- function(libname = find.package("grattanReporter"), pkgname = "grattanReporter"){

  # CRAN Note avoidance
  if (getRversion() >= "2.15.1") {
    utils::globalVariables(c(".",
                             "grattan_correctly_spelled_words",
                             "wrongly_spelled_words",
                             "valid_English_contractions",
                             "grattan_CORRECTLY_SPELLED_WORDS_CASE_SENSITIVE",
                             "not_all_figs_tbls_refd",
                             "not_all_figs_tbls_refd.lab"))
  }

  op <- .Options
  op.grattanReporter <- list(
    "grattanReporter.quiet.progress" = FALSE
  )
  toset <- !(names(op.grattanReporter) %in% names(op))
  if (any(toset)) options(op.grattanReporter[toset])


}
