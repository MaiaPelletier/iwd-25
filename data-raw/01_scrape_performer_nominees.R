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

get_noms_from_tbls <- function(wikitbls) {
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
  # nomination lists
  wikitbls[nom_tbls_index]
}

identify_winners <- function(df) {

  # Determine winner by first nominee in the group and/or if the performer has
  # the symbol indicating a winning nominee (‡).
  # This is because Wikipedia denotes posthumous winners with a different symbol
  # (†), but also there exists ties in the data (i.e. rank 1 & 2 are both
  # winners), so need 2 ways to determine winners.
  # This isn't perfect -- If there was a post-humous tie, it wouldn't pick it
  # up. But there isn't one yet!

  df |>
    group_by(year) |>
    mutate(nom_rank = row_number()) |>
    mutate(award_type = ifelse(nom_rank == 1, "Win", "Nomination")) |>
    ungroup() |>
    mutate(award_type = ifelse(str_detect(performer, "‡"), "Win", award_type)) |>
    select(-nom_rank)
}

clean_nom_tbl <- function(df) {

  # df <- nom_tbl

  # Determine if category is Best Actress or Actor
  category <- names(df)[2]

  df <- df |>
    janitor::clean_names() |>
    rename(performer = 2)

  # Add column for winners vs. nominees
  df <- identify_winners(df)

  df |>
    # Any special circumstances arround nom/award ---
    mutate(award_note = case_when(
      str_detect(performer, "\\(TIE\\)") ~ "Tie",
      str_detect(performer, "†") ~ "Posthumous",
      str_detect(performer, "§") ~ "Refused award",
      str_detect(performer, "\\(Write-in\\)") ~ "Write-in",
      .default = NA
    )) |>
    # Add col for award category ---
    mutate(category = category) |>
    # Clean performer column of symbols, footnotes, etc. ---
    mutate(
      performer = str_remove(performer, "‡.*"),
      performer = str_remove(performer, "†.*"),
      performer = str_remove(performer, "§.*"),
      performer = str_remove(performer, "\\[.*\\]"),
      performer = str_remove(performer, "\\(Write-in\\)"),
      performer = str_trim(performer)
    ) |>
    # Separate the Academy Award number from the year they happened ---
    mutate(
      oscars_no = str_extract(year, "\\(.*\\)"),
      oscars_no = str_extract(oscars_no, "[0-9]+")
    ) |>
    # Use first year in the instance of a range (i.e. 1927/28) & convert to numeric ---
    mutate(
      year = str_extract(year, "[0-9]{4}"),
      year = as.numeric(year)
    ) |>
    select(-ref) |>
    relocate(oscars_no, year, category)
}

get_all_wiki_links <- function(html) {

  # Get href attribute from a tags
  link <-
    html |>
    html_elements("td") |>
    html_elements("a") |>
    html_attr("href")

  # Get title attribute from a tags
  title <-
    html |>
    html_elements("td") |>
    html_elements("a") |>
    html_attr("title")

  # Get actual linked text
  text <-
    html |>
    html_elements("td") |>
    html_elements("a") |>
    html_text() |>
    str_trim()

  # Combine as df and add wikipedia hyperlink to wiki paths
  bind_cols(text = text, title = title, link = link) |>
    mutate(
      text = str_trim(text),
      title = str_remove(title, " \\(actress\\)"),
      match = text == title
    ) |>
    distinct() |>
    mutate(link = paste0("https://en.wikipedia.org", link))

}

get_nominees <- function(html) {

  all_tbls <- scrape_all_wikitbls(html)
  wiki_links <- get_all_wiki_links(html)

  nom_tbl <-
    get_noms_from_tbls(all_tbls) |>
    map_dfr(bind_rows)

  nom_tbl <-
    nom_tbl |>
    clean_nom_tbl() |>
    left_join(wiki_links, by = c("performer" = "text"), relationship = "many-to-many") |>
    filter(match)

  return(nom_tbl)
}

wikis <- c(
  "https://en.wikipedia.org/wiki/Academy_Award_for_Best_Actress",
  "https://en.wikipedia.org/wiki/Academy_Award_for_Best_Actor"
)

htmls <- map(wikis, read_html)
# html <- htmls[[1]]

all_nominees <- map_dfr(htmls, get_nominees) |> arrange(year)

# Create table of all nominees
nominees <-
  all_nominees |>
  select(-c(role_s, film)) |>
  distinct()

# Create table for each role performers were nominated for
nominated_roles <-
  all_nominees |>
  select(oscars_no, year, category, role_s, film)

readr::write_rds(nominees, here("data", "nominees.rds"))
readr::write_rds(nominated_roles, here("data", "nominated_roles.rds"))

