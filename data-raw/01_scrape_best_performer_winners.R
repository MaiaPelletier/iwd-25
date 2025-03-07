# Load libraries used for scraping
library(rvest)
library(here)
library(purrr)
library(stringr)
library(dplyr)

scrape_all_wikitbls <- function(html) {
    html |>
    html_elements(".wikitable") |>
    html_table()
}

get_winners_from_tbls <- function(wikitbls) {
  # The way the wikipedia pages are set up, the second table
  # contains the first set of nominations
  # Get the column names of this table so that we can check which of the
  # rest of the tables have the same names
  nom_cols <- names(wikitbls[[2]])

  # Check which tbls contain the same names (logical vector)
  nom_tbls_index <-
    wikitbls |>
    map(\(x) all(names(x) == nom_cols)) |>
    unlist()

  # Index the list of tables to get just the tables containing the
  # nomination lists, then filter each nomination table to get just the
  # winner and select only the Year and Performer column for each
  wikitbls[nom_tbls_index]  |>
    map_dfr(\(x) x |> filter(if_any(2, str_detect, "‡")))
}

clean_winner_tbl <- function(df) {

  category <- names(df)[2]

  df |>
    select(1:2) |>
    rename(Performer = 2) |>
    mutate(
      Category = category,
      Performer = str_remove(Performer, "‡.*"),
      Performer = str_trim(Performer),
      Oscars_No = str_extract(Year, "\\(.*\\)"),
      Oscars_No = str_extract(Oscars_No, "[0-9]+"),
      Year = str_extract(Year, "[0-9]{4}")
    ) |>
    distinct()
}

get_all_wiki_links <- function(html) {
    html |>
    html_elements("td") |>
    html_elements("a") |>
    html_attrs() |>
    map_dfr(bind_rows) |>
    select(title, link = href) |>
    distinct() |>
    mutate(link = paste0("https://en.wikipedia.org", link))
}

get_winners <- function(html) {

  all_tbls <- scrape_all_wikitbls(html)
  wiki_links <- get_all_wiki_links(html)

  winner_tbl <-
    get_winners_from_tbls(all_tbls) |>
    clean_winner_tbl() |>
    left_join(wiki_links, by = c("Performer" = "title"))

  return(winner_tbl)
}

wikis <- c(
  "https://en.wikipedia.org/wiki/Academy_Award_for_Best_Actress",
  "https://en.wikipedia.org/wiki/Academy_Award_for_Best_Actor"
)

htmls <- map(wikis, read_html)
winners <- map_dfr(htmls, get_winners) |> arrange(Year)

readr::write_rds(winners, here("data", "winners.rds"))
