library(rvest)
library(dplyr)
library(purrr)
library(here)

scrape_performer_bday <- function(nominee) {

    # nominee <- nominee_wikis[[1]]
    print(glue::glue("Scraping {nominee$performer}'s birthday"))

    # Read performer HTML
    nominee_wiki <- nominee$link
    nominee_html <- read_html(nominee_wiki)

    # Select performer's bday from Wikipedia info table
    performer_bday <-
      nominee_html |>
      html_element(".bday") |>
      html_text() |>
      as.Date()

     # Return dataframe with performer name and bday
    data.frame(performer = nominee$performer, birth_day = performer_bday)
}

# Read scraped Best Actor/Actress winner data
nominees <- readr::read_rds(here("data", "nominees.rds"))

# Create list of performers with their wikipedia page link
nominee_wikis <-
  nominees |>
    select(performer, link) |>
    distinct() |>
    purrr::transpose()

# Create df of Performer and their bday from their Wikipedia page
# This takes a while -- it's reading ~500 actor webpages!
bdays <- map(nominee_wikis, scrape_performer_bday, .progress = TRUE) |>
  list_rbind()

nominee_wikis[[216]]

nominees |>
  filter(str_detect(performer, "Sarah"))

bdays |>
  mutate(id = row_number()) |>
  filter(str_detect(performer, "Sara"))

# Write bday df to RDS
# readr::write_rds(bdays, here("data", "bdays.rds"))
