context("check preamble")

test_that("todonotes even if disabled not acceptable", {
  # expect_error(check_preamble("./check-preamble/todonotes.tex", release = TRUE, pre_release = TRUE))
  # expect_error(check_preamble("./check-preamble/todonotes-disable.tex", release = TRUE, pre_release = TRUE))
  expect_true(TRUE)
})

test_that("Working paper inconsistency", {
  expect_error(check_preamble("./check-preamble/working-paper/working-paper-with-report-not.tex"),
               regexp = "Working paper / Report inconsistency")
  expect_error(check_preamble("./check-preamble/working-paper/report-with-working-paper.tex"),
               regexp = "ReportOrWorkingPaper not set to.*Working Paper")
  expect_error(check_preamble("./check-preamble/working-paper/almost-working-paper.tex"),
               regexp = "Working paper / Report inconsistency")
})

test_that("Background paper ok", {
  expect_null(check_preamble("./check-preamble/background-paper/background-paper.tex",
                             pre_release = TRUE))
})

test_that("Technical Report ok", {
  tech.tex <- tempfile("tech", fileext = ".tex")
  c("\\documentclass{grattan}",
    "% https://www.sharelatex.com/6843951114zdkqvpbnsvnp",
    "% pre_release: true",
    "% Comments are deployed by the % sign; everything after % is ignored by the compiler.",
    "% Please do not put comments before \\documentclass as these are reserved for TeX directives.",
    "% add_to_dictionary: ICSEA ACARA Rasch VCAA XXX Cairns Townsville EYL ICSEAs mis ANU",
    "\\addbibresource{bib/MappingStudentProgress_TechReport.bib}",
    "\\author{Peter Goss and Owain Emslie}",
    "\\title{Measuring student progress: A state-by-state report card -- Technical Report -- DRAFT -- NOT FOR DISTRIBUTION}",
    "\\ReportOrWorkingPaper{Technical Report}",
    "\\GrattanReportNumber{2018-00}",
    "\\acknowledgements{%",
    "This technical report was written by Dr Peter Goss, Grattan Institute School Education Program Director and Owain Emslie, Associate. Julie Sonnemann, School Education Fellow, provided extensive research assistance and made substantial contributions to the technical report.",
    "We would like to thank the members of Grattan Institute's School Education Program Reference Group for their helpful comments, as well as numerous government and industry participants and officials for their input.",
    "The opinions in this report are those of the authors and do not necessarily represent the views of Grattan Institute's founding members, affiliates, individual board members, reference group members or reviewers.",
    "Any remaining errors or omissions are the responsibility of the authors.",
    "Grattan Institute is an independent think-tank focused on Australian public policy.",
    "Our work is independent, practical and rigorous.",
    "We aim to improve policy outcomes by engaging with both decision makers and the community.",
    "For further information on the Institute's programs, or to join our mailing list, please go to: \\textcolor{blue}{\\url{http://www.grattan.edu.au/}}.",
    "{\\footnotesize",
    "This technical report may be cited as:",
    "Goss, P., Emslie, O., and Sonnemann, J\\@. (2018). \\emph{\\mytitle}. Grattan Institute.",
    "ISBN: 978-0-6482307-2-4",
    "All material published or otherwise created by Grattan Institute is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License\\par",
    "}",
    "}",
    "\\begin{document}",
    "foo",
    "\\end{document}", "") %>%
    writeLines(tech.tex)

  expect_null(check_preamble(tech.tex))


})


