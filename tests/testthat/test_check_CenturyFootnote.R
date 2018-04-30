context("CenturyFootnote")

test_that("Passes on valid input", {
  expect_null(check_CenturyFootnote("./check-CenturyFootnote/correctly-placed/"))
})

test_that("Errors when expected.", {
  expect_error(check_CenturyFootnote("./check-CenturyFootnote/fewer-than-100/"), regexp = "fewer than 100")
  expect_error(check_CenturyFootnote("./check-CenturyFootnote/not-used/"), regexp = "not used")
  expect_error(check_CenturyFootnote("./check-CenturyFootnote/too-early/", strict = TRUE), regexp = "CenturyFootnote fell")
  expect_error(check_CenturyFootnote("./check-CenturyFootnote/too-late/", strict = TRUE), regexp = "occurs after the 100th")
})

test_that("CompetitionReport had it erroneously placed", {
  skip_on_travis()
  skip_on_cran()
  hutils::provide.dir("./check-CenturyFootnote/rprt1/travis/grattanReport/")
  if (!file.exists("./check-CenturyFootnote/rprt1/travis/grattanReport/log.txt")) {
    file.create("./check-CenturyFootnote/rprt1/travis/grattanReport/log.txt")
  }
  failures <- FALSE
  atlas_pdfs <-
    c("Barriers.pdf", "Charts.pdf", "ChartsF.pdf", "ChartsL.pdf",
      "ChartsL2.pdf", "ChartsLong_lucy.pdf", "ChartsLucy.pdf",
      "ChartsLucy_extrastunted.pdf",
      "ChartsLucy_stunted.pdf", "chartslucy_v2.pdf", "ChartsW.pdf",
      "ChartsWhole.pdf", "ChartsWholePage.pdf", "ChartsWide.pdf", "Fig2_1.pdf",
      "HR_conc.pdf", "Markups.pdf", "NM_conc.pdf", "SE_conc.pdf")
  for (a.pdf in atlas_pdfs) {
    url <- paste0('https://github.com/grattan/zzz-2017-CompetitionReport/raw/master/atlas/',
                  a.pdf)
    hutils::provide.dir("./check-CenturyFootnote/rprt1/atlas")
    fail <-
      download.file(url,
                    destfile = file.path("./check-CenturyFootnote/rprt1/atlas", a.pdf),
                    mode = "wb",
                    quiet = TRUE)
    if (fail) failures <- TRUE
  }
  skip_if(failures)
  expect_error(checkGrattanReport("./check-CenturyFootnote/rprt1/",
                                  compile = TRUE,
                                  pre_release = TRUE),
               regexp = "CenturyFootnote occurs after the 100th footnote")
  unlink("./check-CenturyFootnote/rprt1/atlas")
  file.remove("./check-CenturyFootnote/rprt1/travis/grattanReport/md5/bib/Grattan-Master-Bibliography.bib")
  file.remove("./check-CenturyFootnote/rprt1/travis/grattanReport/md5/bib/Concentration.bib")

})
