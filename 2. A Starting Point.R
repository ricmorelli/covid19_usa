# Gotta start somewhere: I'm not overly enthusiastic about the abundance of cumulative and absolute values in
# our data set as they are not comparable between states of varying populations. As mentioned previously,
# we will need to create new variables that are proportional to a state's population to make them useful in comparisons.
# I'm going to focus this part of my exploration on mortality (proportion of deaths to confirmed cases):

# What do I want to know?
# 1. What is the relationship between population and mortality?
# 2. Did some states observe disproportionately high or low COVID19 mortality relative to their population?

covid19_usa |>
  left_join(state_data, join_by(state)) |> 
  group_by(state, abbreviation) |> 
  mutate(
    mortality = (deaths / confirmed),
  ) |> 
  summarise(
    population = max(population, na.rm = TRUE),
    deaths = max(deaths, na.rm = TRUE),
    mortality = mean(mortality, na.rm = TRUE),
    .groups = 'drop'
  ) |> 
  ggplot(aes(x = population, y = mortality, size = deaths, label = abbreviation, colour = state)) +
  #gplot(aes(x = population, y = mortality, size = deaths, colour = if_else(abbreviation %in% c('CT', 'NJ', 'NY', 'FL', 'TX', 'CA'), state, ''), label = if_else(abbreviation %in% c('CT', 'NJ', 'NY', 'FL', 'TX', 'CA'), abbreviation, ''))) +
  geom_point(alpha = 0.3) +
  geom_label_repel(size = 3, max.overlaps = 3, show.legend = FALSE) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = scales::percent) +
  scale_size_continuous(labels = scales::unit_format(unit = "K", scale = 1e-3)) +
  guides(colour = "none") +
  labs(
    title = "COVID19 Deaths & Mortality by US State",
    subtitle = "The most populous states recorded more deaths, but not always at a comparatively higher mortality rate.",
    caption = "Data from covid19datahub.io",
    x = "Population",
    y = "Mortality Rate"
  ) +
  theme(
    plot.title = element_text(colour = "black"),
    plot.subtitle = element_text(colour = "red")
  )


# This leads our exploration down two distinct paths: 
# 1. Why might these particular states have been impacted disproportionately to others?
# 2. How did the passage of time impact mortality, does the story change?


