library(rvest)
library(dplyr)
library(here)
library(stringr)

# Link to Wikipedia page of each Academy Award ceremonies
wiki <- "https://en.wikipedia.org/wiki/List_of_Academy_Awards_ceremonies"
html <- read_html(wiki)

oscars <-
    html |>
    html_element("table:nth-child(12)") |>
    html_table() |>
    janitor::clean_names() |>
    select(
        oscars_no = number,
        oscars_date = date
    ) |>
    mutate(
        oscars_no = str_extract(oscars_no, "[0-9]+"),
        oscars_date = lubridate::mdy(oscars_date)
    )

# Write oscars df to RDS
readr::write_rds(oscars, here("data", "oscars.rds"))

