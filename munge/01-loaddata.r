
# ESC ---------------------------------------------------------------------

pathesc <- "C:/Users/Lina/STATISTIK/Projects/ESC registry/data"

esc <- read_sas(
  paste0(pathesc, "/hf3_lt_fu_data_soladis_jan19.sas7bdat"),
  paste0(pathesc, "/formats.sas7bcat")
)


# Fix labels ect. ---------------------------------------------------------

esc <- esc %>%
  mutate(across(where(haven::is.labelled), haven::as_factor)) %>%
  mutate(across(where(is.factor), ~ droplevels(., exclude = "Unknown"))) %>%
  mutate(across(where(is.factor), factor)) %>% ## Something odd with num_opRyth (two diff kinds of NA). This fixes it
  mutate(num_dmHepa = recode(num_dmHepa, "A" = "Yes", .default = levels(num_dmHepa))) # uses format for other hep variable. this fixes it


# Get labels for tables ---------------------------------------------------

esclab <- sjlabelled::get_label(esc)
esclab <- tibble(variable = names(esclab), label = esclab) %>%
  mutate(label = if_else(label == "", NA_character_, label))
