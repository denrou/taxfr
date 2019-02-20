library(glue)
library(purrr)
library(dplyr)
library(janitor)
library(stringr)
library(readxl)
library(tidyr)

# Data downloading ----

download_and_extract <- function(url) {
  destfile <- tempfile(fileext = ".zip")
  cat(glue("Trying {url}"))
  try(download.file(url, destfile, quiet = TRUE), silent = TRUE)
  if (file.exists(destfile)) {
    unzip(destfile, exdir = "data-raw")
    file.remove(destfile)
  }
}
today_year  <- as.integer(strftime(Sys.Date(), "%Y")) - 1L
years       <- seq(2011, today_year, by = 1L)

# Between 2011 and 2015, filenames are in the form `ircom_XXXX_revenusXXXX.zip`
# From 2016, filenames are in the form `ircom_XXXX_revenus_XXXX.zip`
sep         <- c(rep("", 5), rep("_", length(years) - 5))
files       <- glue("ircom_{years}_revenus{sep}{years - 1L}.zip")
urls        <- glue("https://www.impots.gouv.fr/portail/www2/fichiers/statistiques/base_de_donnees/ircom/{files}")
walk(urls, download_and_extract)

# Between 2011 and 2016, files are exported in xls format which is not well imported with readxl package.
# From 2017, files are exported in xlsx format
# Therefore, libreoffice is used to convert automatically xls files into xlsx.
# This command limit the use of the package for Linux users which have libreoffice installed.
os          <- Sys.info()["sysname"]
libreoffice <- Sys.which("libreoffice")
if (os != "Linux" || libreoffice == "") {
  stop_msg <- glue(
    "All xls files must be converted to xlsx to avoid errors with `readxl::read_excel`.",
    "This operation is done using libreoffice backend on a linux system.",
    "If this error occured, this means that either you're not running it in a Linux environment, or that you don't have libreoffice installed.",
    "To follow up the process, you can manually convert all xls files inside data-raw folder and proceed to the next steps by hand.",
    .sep = "\n"
  )
  stop(stop_msg, call. = FALSE)
}
system("cd data-raw; for xls in *.xls; do libreoffice --convert-to xlsx $xls --headless; done; rm *.xls")




# Data gathering ----

# Some explanations about the structure of the excel files:
# - There is one excel file per year. For each year, some tax informations are stored based on income on the previous year
# - Taxes are separated per `communes` and are grouped by `départements` using one sheet per `département`

paths <- list.files("data-raw", pattern = ".xlsx", full.names = TRUE)

# Between 2011 and 2014, NA's where indicated as `n.c.`
# From 2015, NA's where indicated as `n.d.`
nas       <- c(rep("n.d.", 3), rep("n.c.", length(paths) - 3))
pb1       <- progress_estimated(length(paths)) # always nice to have a progress bar ;)
col_names <- c(
  "department_code",
  "municipality_code",
  "municipality",
  "reference_tax_revenue_per_tranche_in_euros",
  "number_of_households",
  "revenue_of_households",
  "tax_income",
  "number_of_taxable_households",
  "revenue_of_taxable_households",
  "number_of_households_concerned_by_pensions",
  "revenue_gain_by_pensions",
  "number_of_households_concerned_by_salaries",
  "revenue_gain_by_salaries"
)
tax_raw   <- map2_dfr(paths, nas, function(path, nas) {
  pb1$tick()$print()
  # In the recent years, there are other sheet than taxes for each `departement` to give some statistic summary in the
  # whole country. Since it is not present for all years, those sheets are discarded.
  sheets   <- str_subset(excel_sheets(path), "^[0-9]{3}$")
  year     <- as.numeric(str_extract(path, "[:digit:]{4}"))
  tax_year <- map_dfr(sheets, read_excel, path = path,  skip = 4, na = nas, col_names = col_names, progress = FALSE)
  # Since all excel files will be gathered together, we need to add year information
  tax_year %>%
    mutate(year = year)

})
pb1$stop()



# Data tidyfication ----

# Start cleaning data
tax <- tax_raw %>%
  mutate_at("municipality", str_to_title) %>%
  # Convert to factors to have the right order
  mutate_at("reference_tax_revenue_per_tranche_in_euros", str_replace, pattern = "à", replacement = "to") %>%
  mutate_at("reference_tax_revenue_per_tranche_in_euros", factor,
            levels = c("0 to 10 000", "10 001 to 12 000", "12 001 to 15 000", "15 001 to 20 000", "20 001 to 30 000",
                       "30 001 to 50 000", "50 001 to 100 000", "+ de 100 000", "Total"), ordered = TRUE) %>%
  # The column `revenu_fiscal_de_reference_par_tranche_en_euros`, is a column that must be filled.
  # We use it to remove empty lines and non necessary data
  filter(!is.na(reference_tax_revenue_per_tranche_in_euros)) %>%
  # Starting from 2016, taxes are given per thousands of euros instead of euros
  mutate_at(vars(starts_with("number"), starts_with("revenue"), "tax_income"), as.numeric) %>%
  mutate_at("revenue_of_taxable_households", list(~if_else(.data$year %in% c(2016, 2017), . * 1000, .))) %>%
  # Get INSEE code
  mutate(postal_code = paste0(str_sub(department_code, end = 2L), municipality_code)) %>%
  mutate_at("postal_code", list(~if_else(str_detect(., "NA"), NA_character_, .))) %>%
  select(department_code, municipality_code, postal_code, municipality, everything())

# Data saving ----
usethis::use_data(tax, overwrite = TRUE)
