# analyze family book
library(tidyverse)
library(showtext)


# Automatically use 'showtext' to render text
showtext_auto()
font_add_google("UnifrakturCook", "fraktur", regular.wt = 700, bold.wt = 700)


source("R/util_geom_vertical_band.R")
load("data/records.RData") #records

# analyze the records
# compute lifespan
age_records <- records |>
  # filter out records with "auswander" as a string in the record column
  # filter(str_detect(record,"Auswanderer")) |>
  mutate(emigrant = str_detect(record, "Auswanderer")) |>
  select(name, birth, death, emigrant) |>
  mutate(lifespan = as.numeric(death - birth) / 365.25)


# plot lifespan vs. birth year
age_records |>
  filter(year(birth) > 1750) |>
  ggplot(aes(x = birth, y = lifespan, color = emigrant)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Lifespan vs. Birth Year",
    x = "Birth Year",
    y = "Lifespan (years)"
  ) +
  theme_minimal()

gefallen <- map(records$record, \(x) grep("Gefallen", x, value = TRUE))

extract_all_dates_posix <- function(text, tag) {
  # Regular expression to match the tag word followed by a date in the format dd.mm.yyyy
  myregex <- paste0(
    tag,
    "\\s+(\\d{2}\\.\\d{2}\\.\\d{4})|",
    tag,
    "\\s+(ABT \\d{4})"
  )

  matches <- str_extract_all(text, myregex)
  if (is.na(matches)) {
    return(NA)
  } # tag not found
  if (length(matches[[1]]) > 0) {
    dates <- str_replace(matches[[1]], paste0(tag, "\\s+"), "")
    # replace ABT with jan 1
    dates_a <- dates[grepl("ABT", dates)]
    if (length(dates_a > 0)) {
      dates_a <- dates_a |>
        paste0("-01-01") |>
        str_remove("ABT ") |>
        as.Date()
    } else {
      dates_a <- NA
    }
    dates_b <- dates[!grepl("ABT", dates)] |>
      as.Date(dates, format = "%d.%m.%Y")
    dates <- as.Date(c(dates_a, dates_b))
    # remove NA values
    dates <- dates[!is.na(dates)]
  } else {
    dates <- NA
  }
  return(dates)
}

#flatten records so we can filter out emmigrants
records_flat <- records |>
  select(relatives) |>
  unnest(relatives) |>
  transmute(record = person) |>
  bind_rows(select(records, record))
#|>
#   filter(str_detect(record,"Auswanderer"))

# get all births including spouse and children
births <- records_flat$record |>
  map(\(x) extract_all_dates_posix(x, tag = "BIRT")) |>
  flatten() |>
  unlist() |>
  as.Date() |>
  year() |>
  #summarize count by year
  table() |>
  as.data.frame() |>
  as_tibble() |>
  rename(year = Var1, births = Freq) |>
  mutate(year = as.integer(as.character(year)))


deaths <- records_flat$record |>
  map(\(x) extract_all_dates_posix(x, tag = "DEAT")) |>
  flatten() |>
  unlist() |>
  as.Date() |>
  year() |>
  #summarize count by year
  table() |>
  as.data.frame() |>
  as_tibble() |>
  rename(year = Var1, deaths = Freq) |>
  mutate(year = as.integer(as.character(year)))


births_and_deaths <- full_join(births, deaths, by = "year") |>
  arrange(year) |>
  # fill in missing values
  fill(deaths, births) |>
  # replace NA with 0
  replace_na(list(deaths = 0, births = 0)) |>
  mutate(net_pop_change = births - deaths) |>
  mutate(population = cumsum(net_pop_change))

# plot of births and deaths
alpha <- 0.005
births_and_deaths |>
  # make births and deaths one column
  pivot_longer(
    cols = c(births, deaths),
    names_to = "event",
    values_to = "count"
  ) |>
  filter(year >= 1800, year <= 1947) |>
  ggplot(aes(x = year, y = count)) +
  geom_vertical_band(1941, 1947, 300, "WW II", "red", alpha = alpha) +
  geom_vertical_band(1914, 1918, 100, "WW I", "red", alpha = alpha) +
  geom_vertical_band(1895, 1900, 200, "Emigration?", "blue", alpha = alpha) +
  geom_vertical_band(1870, 1874, 250, "Cholera III", "red", alpha = alpha) +
  geom_vertical_band(1848, 1851, 350, "Cholera II", "red", alpha = alpha) +
  # geom_line(aes(color = event),linewidth = 1) +
  # scale_color_manual(values = c("green","black")) +
  geom_col(aes(fill = event), alpha = 0.6, position = "identity") +
  # use green and black for births and deaths
  scale_fill_manual(values = c("green", "black")) +
  labs(
    title = "Geburt und Tod in Neu/Alt Schowe, Batschka",
    x = "Year",
    y = "Count",
    caption = "Source: Familienbuch Neu Schowe in der Batschka\nBrigitte und Gunther Wolf (ed.)"
  ) +
  theme_light() +
  # change font of title to UnifrakturCook from google fonts
  theme(plot.title = element_text(family = "fraktur", size = 20))

# line plot of population
births_and_deaths |>
  filter(year >= 1800, year <= 1947) |>
  ggplot(aes(x = year, y = population)) +
  geom_line() +
  labs(title = "Population Over Time", x = "Year", y = "Population") +
  theme_minimal()

# Function to determine if a person is alive in a given year
is_alive <- function(birth, death, year) {
  return(year >= birth & year <= death)
}

# impute missing birth and death years to estimate population in each year
model_death <- lm(lifespan ~ birth, age_records)
model_birth <- lm(lifespan ~ death, age_records)

# for record missing either birth or death, but not both,
# predict the missing value based on regressed lifespan
age_records_est <- age_records |>
  bind_cols(lb = predict(model_death, age_records)) |>
  bind_cols(ld = predict(model_birth, age_records)) %>%
  mutate(death = if_else(is.na(death), birth + lb, death)) |>
  mutate(birth = if_else(is.na(birth), death - ld, birth)) |>
  select(-lb, -ld) |>
  mutate(lifespan = as.numeric(death - birth) / 365.25) |>
  filter(!is.na(birth))


year_recs <- age_records_est %>%
  filter(!is.na(birth)) |>
  mutate(birth = year(birth), death = year(death))

population_df <- data.frame(year = 1800:1947)

# Calculate the population for each year
population_df <- population_df %>%
  rowwise() %>%
  mutate(population = sum(is_alive(year_recs$birth, year_recs$death, year)))


# plot population over time
population_df |>
  filter(year >= 1800) |>
  ggplot(aes(x = year, y = population)) +
  geom_line(linewidth = 1) +
  labs(title = "Population Over Time", x = "Year", y = "Population") +
  # add shaded vertical band for WWII
  geom_vertical_band(1939, 1947, 100, "WW II", "red", 0.005) +
  # add shaded vertical band for WWI
  geom_vertical_band(1914, 1918, 100, "WW I", "red", 0.005) +
  # add shaded vertical band for the second cholera epidemic
  geom_vertical_band(1848, 1851, 100, "Cholera II", "red", 0.01) +
  # add shaded vertical band for the third cholera epidemic
  geom_vertical_band(1871, 1875, 150, "Cholera III", "red", 0.01) +
  theme_minimal()
