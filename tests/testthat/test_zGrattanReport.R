context("GrattanReport")

test_that("SchoolFunding.tex doesn't fail", {
  expect_null(checkGrattanReport(path = "./SchoolFunding"))
})

test_that("SchoolFunding.tex", {
  skip_on_travis()
  if (!dir.exists("./SchoolFunding/PRE-RELEASE")){
    dir.create("./SchoolFunding/PRE-RELEASE")
  }
  
  if (file.exists("./SchoolFunding/PRE-RELEASE/SchoolFunding.pdf")){
    file.remove("./SchoolFunding/PRE-RELEASE/SchoolFunding.pdf")
  }
  
  checkGrattanReport(path = "./SchoolFunding", compile = TRUE, pre_release = TRUE, release = FALSE)
  
  expect_true(file.exists("./SchoolFunding/PRE-RELEASE/SchoolFunding.pdf"))
})

test_that("Engaging-students", {
  skip_on_travis()
  skip_if_not(file.exists('C:/Program Files/gs/gs9.20/bin/gswin64c.exe'))
  Sys.setenv(R_GSCMD = 'C:/Program Files/gs/gs9.20/bin/gswin64c.exe')
  if (!dir.exists("./Engaging-students/RELEASE")){
    dir.create("./Engaging-students/RELEASE")
  }
  
  if (file.exists("./Engaging-students/RELEASE/Engaging-students--creating-classrooms-that-improve-learning.pdf")){
    file.remove("./Engaging-students/RELEASE/Engaging-students--creating-classrooms-that-improve-learning.pdf")
  }
  
  checkGrattanReport(path = "./Engaging-students/", compile = TRUE, pre_release = TRUE, release = TRUE)
  
  expect_true(file.exists("./SchoolFunding/PRE-RELEASE/SchoolFunding.pdf"))
  file.remove("./Engaging-students/RELEASE/Engaging-students--creating-classrooms-that-improve-learning.pdf")
})

test_that("Check NEM 2017 Sep paper", {
  skip_on_travis()
  skip_if_not(file.exists('C:/Program Files/gs/gs9.20/bin/gswin64c.exe'))
  Sys.setenv(R_GSCMD = 'C:/Program Files/gs/gs9.20/bin/gswin64c.exe')
  expect_null(checkGrattanReport("./NEM-capacity-markets/", update_grattan.cls = FALSE))
  expect_null(checkGrattanReport("./NEM-capacity-markets/",
                                 compile = TRUE, 
                                 pre_release = TRUE, 
                                 release = TRUE))
  expect_true(file.exists("./NEM-capacity-markets/RELEASE/Next-Generation--the-long-term-future-of-the-National-Electricity-Market.pdf"))
  file.remove("./NEM-capacity-markets/RELEASE/Next-Generation--the-long-term-future-of-the-National-Electricity-Market.pdf")
})

if (file.exists("./SchoolFunding/travis/grattanReport/md5/2016-SchoolFunding.bib")) {
  file.remove("./SchoolFunding/travis/grattanReport/md5/2016-SchoolFunding.bib")
}

if (file.exists("./Engaging-students/travis/grattanReport/md5/bib/Grattan-Master-Bibliography.bib")) {
  file.remove("./Engaging-students/travis/grattanReport/md5/bib/Grattan-Master-Bibliography.bib")
}

if (file.exists("./Engaging-students/travis/grattanReport/md5/bib/Grattan-Master-Bibliography.bib")) {
  file.remove("./Engaging-students/travis/grattanReport/md5/bib/Grattan-Master-Bibliography.bib")
}

if (file.exists("./NEM-capacity-markets/travis/grattanReport/md5/bib/Grattan-Master-Bibliography.bib")) {
  file.remove("./NEM-capacity-markets/travis/grattanReport/md5/bib/Grattan-Master-Bibliography.bib")
}


