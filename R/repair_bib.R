

repair_bib <- function(filename, outfile = NULL) {
  if (length(filename) != 1L) {
    stop("`filename` must be length-one.")
  }
  if (file.exists(filename)) {
    stop("`filename` provided but does not exist.")
  }

  bib_tbl <- fread_bib(filename)
  bib_tbl[, entry_type := tolower(entry_type)]
  setindexv(bib_tbl, "entry_type")
  setindexv(bib_tbl, "field")

  # TechReports:
  #   - must have institution
  #      - not institute
  #   - must not have organization
  #   - no type

  # Article:
  #   - no publisher
  #   - needs journal



  bib_tbl[, new_field := copy(field)]
  bib_tbl[entry_type == "techreport" &
            field %in% c("organization",
                         "organisation",
                         "institute"),
          new_field := "institution"]

  bib_tbl[entry_type == "techreport",
          institution_absent := "institution" %notchin% field,
          by = "key"]
  bib_tbl[, institution_absent := coalesce(institution_absent, FALSE)]
  bib_tbl[(institution_absent), new_entry_type := "misc"]


  bib_tbl[entry_type == "article",
          journal_absent := "journal" %notchin% field]

}



