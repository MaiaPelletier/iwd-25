library(rvest)
library(dplyr)
library(purrr)
library(here)

scrape_performer_bday <- function(winner) {

    # Read performer HTML
    winner_wiki <- winner$link
    winner_html <- read_html(winner_wiki)

    # Select performer's bday from Wikipedia info table
    performer_bday <- 
        winner_html |>
        html_element(".bday") |>
        html_text() |>
        as.Date()

     # Return dataframe with performer name and bday
    data.frame(performer = winner$performer, birth_day = performer_bday)
}

# Read scraped Best Actor/Actress winner data
winners <- readr::read_rds(here("data", "winners.rds"))

# Create list of performers with their wikipedia page link
winner_wikis <- 
    winners |>
    select(performer, link) |>
    distinct() |> 
    purrr::transpose()

# Create df of Performer and their bday from their Wikipedia page
# This takes a while -- it's reading ~200 actor webpages!
bdays <- map_dfr(winner_wikis, scrape_performer_bday)

# Write bday df to RDS
readr::write_rds(bdays, here("data", "bdays.rds"))