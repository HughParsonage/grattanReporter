library(testthat)
if (requireNamespace("devtools", quietly = TRUE)) {
  if (!(requireNamespace("TeXCheckR", quietly = TRUE) && 
        packageVersion("TeXCheckR") >= package_version("0.2.2"))) {
    devtools::install_github('hughparsonage/TeXCheckR')
  }
  
  if (!(requireNamespace("hutils", quietly = TRUE) && 
        packageVersion("hutils") >= package_version("0.6.0"))) {
    devtools::install_github('hughparsonage/hutils')
  }
  library(grattanReporter)
  
  test_check("grattanReporter")
}
