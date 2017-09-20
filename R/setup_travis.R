#' Setup Travis-CI for grattan reports
#' @description Should not be invoked by end-users. Only to be run within the \code{.travis.yml} file
#' while in travis
#' @export

setup_travis <- function() {
  if (!file.exists("travis_trattex_built") ||
      as.double(difftime(Sys.time(), file.mtime("travis_trattex_built"), units = "days")) > 30) {
    system("source ./travis/texlive.sh")
    system("tlmgr install acronym bigfoot blindtext chngcntr cmap nowidow mdframed navigator needspace tablefootnote tocloft xpatch multirow bbding mathastext isomath relsize")
    system("tlmgr update --all")
    system("sudo apt-get update")
    system("sudo apt-get install texlive-bibtex-extra")
    system('wget "https://sourceforge.net/projects/biblatex-biber/files/biblatex-biber/current/binaries/Linux/biber-linux_x86_64.tar.gz"')
    system('tar xzf biber-linux_x86_64.tar.gz')
    system('export PATH=$PATH:$PWD')
    system('tlmgr update biber')
    if (nzchar(Sys.which("biber"))) {
      file.create("travis_grattex_built")
    }
  }
}
