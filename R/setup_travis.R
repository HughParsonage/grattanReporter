#' Setup Travis-CI for grattan reports
#' @description Should not be invoked by end-users. Only to be run within the \code{.travis.yml} file
#' while in travis
#' @export

setup_travis <- function() {
  system("source ./travis/texlive.sh")
  system("tlmgr install acronym bigfoot blindtext chngcntr cmap nowidow mdframed navigator needspace tablefootnote tocloft xpatch multirow bbding mathastext isomath relsize")
  system("tlmgr update --all")
  system("sudo apt-get update")
  system("sudo apt-get install texlive-bibtex-extra")
}
