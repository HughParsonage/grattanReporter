# Ensure you are running R as an Administrator (right click on program in the start menu and select "run as administrator")

# Point to location where R is installed (unique to each computer)
.libPaths("C:/Program Files/R/R-3.2.3/library") 

# Install Grattan Reporter package and dependencies
install.packages(c("data.table", "dplyr", "hunspell", "twitteR", "magrittr", "stringr"))
install.packages("grattanReporter", repos = "https://hughparsonage.github.io/drat", type = "source")

# Check report
library(grattanReporter)
checkGrattanReport("filename")
checkGrattanReport("C:/Users/griffithsk/Dropbox (Grattan Institute)/Apps/ShareLaTeX/NEM-capacity-markets")
# e.g. checkGrattanReport("C:/Users/griffithsk/Dropbox (Grattan Institute)/Apps/ShareLaTeX/NEM-stabilising-the-grid-paper/")
# report any inappropriate error (bug) to Hugh Parsonage <hugh.parsonage@grattaninstitute.edu.au>

# Pre-release checks:
# Must have Miktex installed (ask Hugh)
checkGrattanReport("filename", compile = TRUE)
checkGrattanReport("C:/Users/griffithsk/Dropbox (Grattan Institute)/Apps/ShareLaTeX/NEM-capacity-markets", compile = TRUE)
# checkGrattanReport("C:/Users/griffithsk/Dropbox (Grattan Institute)/Apps/ShareLaTeX/NEM-stabilising-the-grid-paper/", compile = TRUE)
checkGrattanReport("filename", compile = TRUE, pre_release = TRUE)
checkGrattanReport("C:/Users/griffithsk/Dropbox (Grattan Institute)/Apps/ShareLaTeX/NEM-capacity-markets", compile = TRUE, pre_release = TRUE)
# checkGrattanReport("C:/Users/griffithsk/Dropbox (Grattan Institute)/Apps/ShareLaTeX/NEM-stabilising-the-grid-paper/", compile = TRUE, pre_release = TRUE)

### TIPS ------
# Open Report.tex in another window in R to fix report errors as you go (avoids switching back to ShareLaTeX and compiling)
# Use Alt-Shift-G to search report in R by line number 
# If checkGrattanReport() is not working, separate components can be checked e.g. check_spelling() - see list below
# If check_spelling() needs to be over-ruled use "% add_to_dictionary: NewWord" in the report


# Optional: individual checks if checkGrattanReport() is not working (an interim solution) -----
check_preamble("filename/Report.tex")
check_escapes("filename/Report.tex")
check_dashes("filename/Report.tex")
check_quote_marks("filename/Report.tex")
check_footnote_typography("filename/Report.tex")
check_labels("filename/Report.tex")
check_literal_xrefs("filename/Report.tex")
check_spelling("filename/Report.tex")
check_biber("filename/Report.tex")
# Use ignore.lines as a work-around for inappropriate errors e.g. check_spelling("C:/Users/griffithsk/Dropbox (Grattan Institute)/Apps/ShareLaTeX/NEM-stabilising-the-grid-paper/Report.tex", ignore.lines = 644)

# Note other checks can only be done through checkGrattanReport:
# check_cite_pagerefs(filename)
# check_spacing(filename)
# check_xrefs(filename)
# check_sentence_ending_periods(filename)
# check_CenturyFootnote(filename)
# check_all_figs_tbls_refd(filename)

