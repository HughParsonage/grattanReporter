
compare_xrefs_to_anchors <- function(path = ".",
                                     pattern = NULL,
                                     float = c("figure", "table", "boxe")) {
  get_wd <- getwd()
  setwd(path)
  on.exit(setwd(get_wd))
  project_files <- dir(path = ".", pattern = pattern, full.names = TRUE)
  project_root <- grep("\\.tex$", project_files, value = TRUE, perl = TRUE)
  if (length(project_root) != 1L) {
    stop("`path` and `pattern` do not specify a unique root file.")
  }


  inputs_of_project <- inputs_of(project_root)

  AUX <- caption_id <- page <-
    fig_no <- caption <- NULL
  aux_files <- paste0(tools::file_path_sans_ext(c(project_root, inputs_of_project)), ".aux")
  aux_contents <-
    rbindlist(setNames(lapply(aux_files, fread, header = FALSE, sep = NULL),
                       aux_files),
              idcol = "File") %>%
    setnames(2L, "AUX") %>%
    .[]

  float <- match.arg(float)
  lab <-
    switch(float,
           "figure" = "fig",
           "table" = "tbl",
           "boxe"  = "box")

  xref_page <- fig_page <- NULL

  figure_xrefs <-
    aux_contents %>%
    .[AUX %pin% "crefa([^\\}]+)\\}.*[0-9]\\}\\}$"] %>%
    .[!grepl("^\\\\zref", AUX)] %>%
    .[, caption_id := sub("^.*crefa@@@(.*?)@@@.*$", "\\1", AUX)] %>%
    .[, page := sub("^.*[^0-9]([0-9]+)\\}\\}*$", "\\1", AUX, perl = TRUE)] %>%
    .[, .(caption_id, xref_page = as.integer(page))]

  first_figure_xref <-
    figure_xrefs[grepl(sprintf("[%s]", lab), caption_id, fixed = TRUE),
                 .(xref_page = min(xref_page)), by = "caption_id"]

  fig_no <- page_no <- caption <- href <- SINK <- NULL
  figure_captions <-
    aux_contents %>%
    .[grep(paste0("\\\\newlabel\\{", lab, "[:][^@\\}]++\\}"), AUX, perl = TRUE)] %>%
    .[, c("fig_no", "page_no", "caption", "href", "SINK") := tstrsplit(extract_LaTeX_argument(AUX, "newlabel", n = 2L)[["extract"]], split = "}{", fixed = TRUE)] %>%
    .[, fig_no := sub("^\\{", "", fig_no, perl = TRUE)] %>%
    .[, caption := trimws(sub("\\relax", "", caption, fixed = TRUE))] %>%
    .[, SINK := NULL] %>%
    .[]

  figure_locations <-
    aux_contents %>%
    .[grep(paste0("\\\\newlabel\\{", lab, "[:][^@]++@cref\\}"), AUX, perl = TRUE)] %>%
    .[, caption_id := sub("^\\\\newlabel\\{([^@]++)@.*+$", "\\1", AUX, perl = TRUE)] %>%
    .[, fig_no := sub(paste0("^.*\\{\\[",
                             float,
                             if (float == "boxe") {
                               "\\]\\[([1-9][0-9]*)\\].*$"
                             } else {
                               "\\]\\[([12]?[0-9])\\]\\[([1-9])\\].*$"
                             }),
                      "\\2.\\1",
                      AUX,
                      perl = TRUE)] %>%
    .[, page := sub("^.*[^0-9]([0-9]+)\\}\\}$", "\\1", AUX, perl = TRUE)] %>%
    .[, .(caption_id, fig_page = as.integer(page), fig_no)]

  if (requireNamespace("ggplot2", quietly = TRUE)) {
    rel_page <- label <- NULL
    p <-
      figure_locations[first_figure_xref, on = "caption_id", nomatch=0L] %>%
      .[figure_captions, on = "fig_no"] %>%
      .[, rel_page := xref_page - fig_page] %>%
      .[abs(rel_page) > 5L,
        label := substr(paste(fig_no, caption), 0, 50)] %>%
      ggplot2::ggplot(ggplot2::aes(x = xref_page, y = fig_page, label = label)) +
      ggplot2::geom_point() +
      ggrepel::geom_label_repel(na.rm = TRUE)

    print(p)
  }

  setwd(get_wd)
  figure_locations[first_figure_xref, on = "caption_id", nomatch=0L] %>%
    .[figure_captions, on = "fig_no"]
}


