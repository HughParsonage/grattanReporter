
check_timing <- function(comment = "", date = Sys.time()) {
  old_op <- getOption("grattanReporter.quiet.progress")
  options("grattanReporter.quiet.progress" = TRUE)
  stopifnot(dir.exists("./timings/"),
            dir.exists("./tests/testthat/SchoolFunding/"),
            requireNamespace("data.table", quietly = TRUE))
  library(data.table)
  library(magrittr)
  checkCircuiBreaker <- function(){
    checkGrattanReport("./tests/testthat/SchoolFunding/")
  }

  timed <- microbenchmark::microbenchmark(checkCircuiBreaker(), times = 30L)
  options("grattanReporter.quiet.progress" = old_op)
  timed %>%
    setDT %>%
    .[, date := date] %>%
    .[, comment := comment] %>%
    fwrite("./timings/checkGrattanReport.csv", append = TRUE)
}

check_timing()
