library(dplyr)
library(here)
library(lubridate)

nominees <- readr::read_rds(here("data", "nominees.rds"))
bdays <- readr::read_rds(here("data", "bdays.rds"))
oscars <- readr::read_rds(here("data", "oscars.rds"))

bdays |>
  as_tibble() |>
  distinct()

nominees |>
  left_join(bdays, by = "performer")

nominees[413,]

bdays |>
  filter(performer == "Sarah Miles")

nominees_ages <-
    nominees |>
    left_join(bdays) |>
    left_join(oscars) |>
    mutate(
        age = year(as.period(interval(birth_day, oscars_date)))
    ) |>
    select(
        oscars_no,
        oscars_date,
        category,
        nominee = performer,
        birth_day,
        age
    )

# Write nominees ages df to RDS
readr::write_rds(nominees_ages, here("data", "nominees_ages.rds"))
