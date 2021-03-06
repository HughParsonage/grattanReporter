#' Setup Travis-CI for grattan reports
#' @description Should not be invoked by end-users. Only to be run within the \code{.travis.yml} file
#' while in travis
#' @export

setup_travis <- function() {
  travis_cache <- "/home/travis/texlive/texmf-var/web2c/tlmgr.log"
  
  should_rebuild <- 
    tryCatch({
      
      system("tlmgr list --only-installed --data name > installed_texlive_packages.csv")
      installed_packages <- fread("installed_texlive_packages.csv", header = FALSE)
      print(installed_packages)
      required_packages  <- 
        c("acronym",
          "bigfoot",
          "blindtext",
          "chngcntr",
          "cmap",
          "nowidow",
          "mdframed",
          "navigator",
          "needspace",
          "tablefootnote",
          "tocloft",
          "xpatch",
          "multirow",
          "bbding",
          "mathastext",
          "isomath",
          "relsize")
      
      if (any(required_packages %notin% installed_packages[["name"]])) {
        cat(setdiff(required_packages, installed_packages[["name"]]))
        TRUE
      } else {
        FALSE
      }
    },
    error = function(e) {
      cat(e$message)
      invisible(TRUE)
    },
    warning = function(e) {
      cat(e$message)
      invisible(TRUE)
    })
  
  cat(should_rebuild)
  cat("\n")
  
  print(!file.exists(travis_cache))
  print(as.double(difftime(Sys.time(), file.mtime(travis_cache), units = "days")) > 30)
  
  if (OR(should_rebuild,
         OR(!file.exists(travis_cache),
            as.double(difftime(Sys.time(), file.mtime(travis_cache), units = "days")) > 30))) {
    cat("\ngrattanReporter requested rebuild...\n\n")
    
    system("tlmgr install acronym bigfoot blindtext chngcntr cmap nowidow mdframed navigator needspace tablefootnote tocloft xpatch multirow bbding mathastext isomath relsize")
    system("tlmgr update --all")
    system("sudo apt-get update")
    system("sudo apt-get install texlive-bibtex-extra")
    # # system('wget "https://sourceforge.net/projects/biblatex-biber/files/biblatex-biber/current/binaries/Linux/biber-linux_x86_64.tar.gz"')
    # system('tar xzf biber-linux_x86_64.tar.gz')
    # system('export PATH=$PATH:$PWD')
    # system('tlmgr update biber')
    if (!nzchar(Sys.which("biber"))) {
      cat("\nno biber\n")
    }
    # file.create(travis_cache)
  }
}
