library(dplyr)
library(here)
library(lubridate)

winners <- readr::read_rds(here("data", "winners.rds"))
bdays <- readr::read_rds(here("data", "bdays.rds"))
oscars <- readr::read_rds(here("data", "oscars.rds"))

winners_ages <- 
    winners |>
    left_join(bdays) |>
    left_join(oscars) |>
    mutate(
        age = year(as.period(interval(birth_day, oscars_date)))
    ) |>
    select(
        oscars_no, 
        oscars_date,
        category,
        winner = performer,
        birth_day,
        age
    )

# Write winners ages df to RDS
readr::write_rds(winners_ages, here("data", "winners_ages.rds"))
