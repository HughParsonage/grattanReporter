library(testthat)
if (!requireNamespace("TeXCheckR", quietly = TRUE) || packageVersion("TeXCheckR") < package_version("0.2.1")) {
  requireNamespace("devtools", quietly = TRUE) || install.packages("devtools")
  devtools::install_github("HughParsonage/TeXCheckR")
}
library(grattanReporter)

test_check("grattanReporter")
