
Grattan_staff <-
  fread("./data-raw/Grattan_staff.tsv", fill = TRUE) %>%
  .[, email_address := paste0(gsub(" ", ".", name, fixed = TRUE), "@grattan.edu.au")]
