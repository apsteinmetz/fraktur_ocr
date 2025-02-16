# analyze family book

# analyze the records
# compute lifespan
records <- records |>
   filter(!is.na(birth),!is.na(death)) |>
   mutate(lifespan = (death - birth)/365.25)

# plot lifespan vs. birth year
ggplot(records,aes(x = birth,y = lifespan)) +
   geom_point() +
   geom_smooth(method = "lm",se = FALSE) +
   labs(title = "Lifespan vs. Birth Year",
        x = "Birth Year",
        y = "Lifespan (years)") +
   theme_minimal()
# plot histogram of birth year
ggplot(records,aes(x = birth)) +
   geom_histogram(binwidth = 5) +
   labs(title = "Histogram of Birth Year",
        x = "Birth Year",
        y = "Count") +
   theme_minimal()
# plot histogram of death year
ggplot(records,aes(x = death)) +
   geom_histogram(binwidth = 5) +
   labs(title = "Histogram of Death Year",
        x = "Death Year",
        y = "Count") +
   theme_minimal()
# summary table of death year
temp <- records %>%
   mutate(death_year = year(death)) |>
   summarize(.by= death_year,count = n()) %>%
   arrange(death_year)

# bar plot of death year
ggplot(temp,aes(x = death_year,y = count)) +
   geom_col() +
   labs(title = "Deaths by Year",
        x = "Death Year",
        y = "Count") +
   theme_minimal()

# table to infer population in each year
temp <- records %>%
   mutate(birth = year(birth),
          death = year(death))

years <- seq(min(temp$birth), max(temp$death))

# Function to determine if a person is alive in a given year
is_alive <- function(birth, death, year) {
   return(year >= birth & year <= death)
}

population_df <- data.frame(year = years)

# Calculate the population for each year
population_df <- population_df %>%
   rowwise() %>%
   mutate(population = sum(is_alive(temp$birth, temp$death, year)))

# plot population over time
ggplot(population_df, aes(x = year, y = population)) +
   geom_line() +
   labs(title = "Population Over Time",
        x = "Year",
        y = "Population") +
   theme_minimal()
