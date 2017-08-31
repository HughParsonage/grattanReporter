library(testthat)
if (requireNamespace("devtools", quietly = TRUE)) {
  if (!requireNamespace("TeXCheckR", quietly = TRUE) || packageVersion("TeXCheckR") < package_version("0.2.2")) {
    devtools::install_github('hughparsonage/TeXCheckR')
  }
  library(grattanReporter)
  
  test_check("grattanReporter")
}
