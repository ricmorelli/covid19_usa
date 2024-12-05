# Gotta start somewhere: I'm not overly enthusiastic about the abundance of cumulative and absolute values in
# our data set as they are not comparable between states of varying populations. As mentioned previously,
# we will need to create new variables that are proportional to a state's population to make them useful in comparisons.
# I'm going to focus this part of my exploration on mortality (proportion of deaths to confirmed cases):

# What do I want to know?
# 1. What is the relationship between population and mortality?
# 2. Did some states observe disproportionately high or low COVID19 mortality relative to their population?

# What do I expect to find?
# It seems reasonable that states with a larger population will, on average, suffer higher rates of mortality.
# Given COVID is an airborne virus, a more populated state allows more avenues for transmission.

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
  geom_point(alpha = 0.3) +
  geom_label_repel(size = 3, max.overlaps = 3, show.legend = FALSE) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = scales::percent) +
  scale_size_continuous(labels = scales::unit_format(unit = "K", scale = 1e-3)) +
  guides(colour = "none") +
  labs(
    title = "COVID19 Mortality by US State",
    caption = "Data from covid19datahub.io",
    x = "Population",
    y = "Mortality Rate"
  ) +
  theme(
    plot.title = element_text(colour = "black"),
    plot.subtitle = element_text(colour = "red")
  )


# There are two groups of results that are of immediate interest:

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
  ggplot(aes(x = population, y = mortality, size = deaths, colour = if_else(abbreviation %in% c('CT', 'NJ', 'NY', 'MA', 'MI', 'PA', 'FL', 'TX', 'CA'), state, ''), label = if_else(abbreviation %in% c('CT', 'NJ', 'NY', 'MA', 'MI', 'PA', 'FL', 'TX', 'CA'), abbreviation, ''))) +
  geom_point(alpha = 0.3) +
  geom_label_repel(size = 3, max.overlaps = 5, show.legend = FALSE) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = scales::percent) +
  scale_size_continuous(labels = scales::unit_format(unit = "K", scale = 1e-3)) +
  guides(colour = "none") +
  labs(
    title = "COVID19 Mortality by US State",
    caption = "Data from covid19datahub.io",
    x = "Population",
    y = "Mortality Rate"
  ) +
  theme(
    plot.title = element_text(colour = "black"),
    plot.subtitle = element_text(colour = "red")
  )


# Group 1: FL, TX & CA:
# Relative to my expectations, these more populated states appear to have mortality rates are in line 
# with the average. I note that these are the 3 most populous states in the country, could their ability
# to curb mortality be a result of better government support? Policy measures? Vaccine access? Or something
# far more simple...?

# Group 2: The Northeast
# Although not extreme outliers relative to the rest of the data, it is most certainly of interest that
# all states in this group are from the same geographical region. What is it about the northeast that
# caused above average mortality rates?

# It might seem reasonable to investigate these clusters independently, but what if they more closely related
# than we might think? In my initial assessment of the data set and its variables, I highlighted my aversion
# to using cumulative or absolute values in my analysis, as they limit our ability to compare data across regions.
# In using population as a metric on its own I have gone against my own thinking. Let's transform population to
# a new metric that makes it a variable that can be used in state-to-state comparison: 'population density'.

# The COVID19 package does not have land area data, so we will need to join to our standard state_data table that
# contains the land area in km(sq) for each state.

covid19_usa |>
  left_join(state_data, join_by(state)) |> 
  group_by(state, abbreviation) |> 
  mutate(
    mortality = (deaths / confirmed),
    pop_density = (population / area_kmsq)
  ) |> 
  summarise(
    deaths = max(deaths, na.rm = TRUE),
    mortality = mean(mortality, na.rm = TRUE),
    pop_density = mean(pop_density),
    .groups = 'drop'
  ) |> 
  ggplot(aes(x = pop_density, y = mortality, size = deaths, label = abbreviation, colour = state)) +
  geom_point(alpha = 0.3) +
  geom_label_repel(size = 3, max.overlaps = 5, show.legend = FALSE) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = scales::percent) +
  scale_size_continuous(labels = scales::unit_format(unit = "K", scale = 1e-3)) +
  guides(colour = "none") +
  labs(
    title = "COVID19 Mortality by US State",
    subtitle = "More densely populated states observed higher mortality rates on average",
    caption = "Data from covid19datahub.io",
    x = "Population Density (people per sq. km)",
    y = "Mortality Rate"
  ) +
  theme(
    plot.title = element_text(colour = "black"),
    plot.subtitle = element_text(colour = "red")
  )


# This leads our exploration down two distinct paths: 
# 1. Why might these particular states have been impacted disproportionately to others?
# 2. How did the passage of time impact mortality, does the story change?


