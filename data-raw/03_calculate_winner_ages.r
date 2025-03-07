library(rvest)
library(dplyr)
library(purrr)
library(here)

winners <- readr::read_rds(here("data", "winners.rds"))
bdays <- readr::read_rds(here("data", "bdays.rds"))

# TODO: Get dates of each Academy Award ceremony to properly calculate ages
winners_ages <- 
    winners |>
    left_join(bdays) |>
    mutate(
        birth_year = lubridate::year(birth_day),
        age = year - birth_year
    ) |>
    select(
        oscars_no, 
        oscars_year = year,
        winner = performer,
        category,
        birth_day,
        age
    )

# Write winners ages df to RDS
readr::write_rds(winners_ages, here("data", "winners_ages.rds"))
