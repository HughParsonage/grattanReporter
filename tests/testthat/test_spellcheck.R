context("Spellchecker")

test_that("School funding report checks out", {
  expect_null(check_spelling("./SchoolFunding/SchoolFunding.tex",
                             known.correct = c("SRS", "SE.XPD.TOTL.GD.XS", "WDI", "SSNP", "underfunded", "overfund[a-z]*", "NMS", "WPI", "DET", "phas", "NP", "SATs", "ENG", "th", "stds", "RCTs", "CAGR"), ignore.lines = 1551))
})

test_that("Check spelling of multiple input document", {
  expect_error(check_spelling("./spellcheck_multi_input/spellcheck_multi_input.tex"),
               regexp = "failed on above line")
})

test_that("Abbreviations", {
  expect_error(check_spelling("spellcheck-abbrevs.tex"))
})

test_that("Initalisms", {
  expect_null(check_spelling("./spelling/abbrev/abbrev-defd-ok.tex"))
  expect_null(check_spelling("./spelling/abbrev/abbrev-defd-ok-2.tex"))
  expect_null(check_spelling("./spelling/abbrev/HILDA-ok.tex"))
  expect_equal(extract_validate_abbreviations(readLines("./spelling/abbrev/abbrev-defd-ok-stopwords.tex")),
               c("QXFEoC", "AIAS"))
  expect_equal(extract_validate_abbreviations(readLines("./spelling/abbrev/abbrev-plural.tex")),
               c("LVR"))
})

test_that("Initialism checking doesn't fail if at start of sentence", {
  expect_null(check_spelling("./spelling/abbrev/abbrev-at-line-start.tex"))
})

test_that("Add to dictionary, ignore spelling in", {
  expect_error(check_spelling("./spelling/add_to_dictionary-wrong.tex"), regexp = "[Ss]pellcheck failed")
  expect_error(check_spelling("./spelling/ignore_spelling_in-wrong.tex", pre_release = FALSE), regexp = "[Ss]pellcheck failed")

  expect_null(check_spelling("./spelling/add_to_dictionary-ok.tex"))
  expect_null(check_spelling("./spelling/ignore_spelling_in-ok.tex", pre_release = FALSE))
  expect_null(check_spelling("./spelling/ignore_spelling_in-ok-2.tex", pre_release = FALSE))

  expect_error(check_spelling("./spelling/ignore_spelling_in-ok.tex"), regexp = "pre_release = TRUE")
})

test_that("Stop if present", {
  expect_error(check_spelling("./stop_if_present/should-stop.tex"), regexp = "skillset")
  expect_error(check_spelling("./stop_if_present/should-stop-2.tex"), regexp = "skillset")
  expect_error(check_spelling("./stop_if_present/stop_even_if_added.tex"), regexp = "skillset")
  expect_error(check_spelling("./stop_if_present_inputs/stop-if-held-in-inputs.tex"), regexp = "skillset")
  expect_error(check_spelling("./stop_if_present/should-stop-3.tex"), regexp = "percent")
  expect_null(check_spelling("./stop_if_present/should-not-stop.tex"))
})

test_that("Lower-case governments should error", {
  expect_error(check_spelling("./spelling/Govt/NSWgovt.tex"), regexp = "uppercase G")
  expect_error(check_spelling("./spelling/Govt/ACTgovt.tex"), regexp = "uppercase G")
  expect_error(check_spelling("./spelling/Govt/NTgovt.tex"), regexp = "uppercase G")
  expect_error(check_spelling("./spelling/Govt/Queenslandgovt.tex"), regexp = "uppercase G")
  expect_error(check_spelling("./spelling/Govt/WAgovt.tex"), regexp = "uppercase G")
})

test_that("Lower-case governments ok in some cases", {
  expect_null(check_spelling("./spelling/Govt/lc-govt-ok.tex"))
  expect_null(check_spelling("./spelling/Govt/plural-ok.tex"))
})

test_that("Vrefrange keys are ok", {
  expect_null(check_spelling("./spelling/vrefrange.tex"))
})

test_that("Chaprefrange", {
  expect_null(check_spelling("./spelling/chaprefrange.tex"))
})


