# We will be getting our data from covid19datahub.io courtesy of the 'COVID19' R package
library(COVID19)
library(tidyverse)
library(snakecase)
library(scales)
library(ggrepel)
library(ggthemes)
library(gghighlight)
library(gganimate)
library(png)
library(gifski)
library(viridis)

# Load in USA Covid19 data
covid19_usa <- covid19("United States", level = 2)

# Let's take a look at what we're working with
names(covid19_usa) <- to_snake_case(names(covid19_usa))
glimpse(covid19_usa)

# Cumulative numeric variables: confirmed, deaths, recovered... etc
  # I don't anticipate these to be too useful in isolation, more likely to be used as inputs to create new variables.
  # It is possible we could also 'de-cum' these variables to get daily values of 'new confirmed', 'new deaths' etc.

# Numeric variables associated with <date>: hosp, icu, vent
  # These variables could be immediately useful to us as they are non-cumulative and can be better used in
  # a time series. However, these absolute values refer to a respective state, so it may be more useful to transform
  # these into proportions of population to make them comparable.

# Population: This will be extremely useful for creating proportions using other numeric variables.

# Policy Measure Variables: school_closing, workplace_closing, stay_home_restrictions ... etc.
  # Although these variables are described using integers (0, 1, 2 ...etc), they should be treated as factors.
  # We will make this transformation if we choose to use any of these variables. My immediate concern is that
  # the levels are almost certainly not consistent in meaning across regions. For example, A level 2 
  # 'facial_coverings' policy in New York versus Florida are likely to be very different in reality. For this
  # reason alone, I'm not too comfortable using these variables unless comparing regions under the same
  # local or state government policy.

# Government Response Indices: stringency_index, economic_support_index,,, etc
  # I'm certainly more comfortable using these Oxford response trackers to compare across countries or regions.
  # Being calculated as an index rather than grouped into discrete levels gives the assurance that small
  # differences between regions will be more accurately described.

# Administrative Areas:
  # Likely the single most important set of variables if we aim to compare across regions.
  # There do not appear to be any major transformations required, aside from renaming.

# Coordinates & ISO codes:
  # World standard measures that will be very useful for any mapping visualisations.


# Let's conduct some minor cleaning and transformation on our dataset

# Rename ambiguous columns
covid19_usa <- 
  covid19_usa |>
  rename(
    country = administrative_area_level_1,
    state = administrative_area_level_2
  ) 

# Remove non-mainland states and regions (and D.C)
covid19_usa <- 
  covid19_usa |> 
  filter(! state %in% c('American Samoa', 'Puerto Rico', 'Guam', 'Virgin Islands', 'Northern Mariana Islands', 'District of COlumbia'))

# Subset our data to a specified time frame (we will focus on the first 3 years of COVID19 only).
covid19_usa <-
  covid19_usa |> 
  filter(date < '2023-01-01')

# Create factors for appropriate variables currently set to <int>:
fct_cols <- c('school_closing', 'workplace_closing', 'cancel_events', 'gatherings_restrictions',
              'transport_closing', 'stay_home_restrictions', 'internal_movement_restrictions',
              'international_movement_restrictions', 'information_campaigns', 'testing_policy',
              'contact_tracing', 'facial_coverings', 'vaccination_policy', 'elderly_people_protection')

covid19_usa <- 
  covid19_usa |> 
  mutate_at(fct_cols, factor)

str(covid19_usa)

# I'm going to create a new table that I can join to where needed for labeling 
# and some other ideas I want to explore.
state_data <- tibble::tribble(
  ~state,          ~abbreviation,     ~government_party,    ~area_kmsq,
  "Alabama",       "AL",              "Republican",         "135767",
  "Alaska",        "AK",              "Republican",         "1723337",
  "Arizona",       "AZ",              "Democratic",         "295234",
  "Arkansas",      "AR",              "Republican",         "137732",
  "California",    "CA",              "Democratic",         "423967",
  "Colorado",      "CO",              "Democratic",         "269601",
  "Connecticut",   "CT",              "Democratic",         "14357",
  "Delaware",      "DE",              "Democratic",         "6446",
  "Florida",       "FL",              "Republican",         "170312",
  "Georgia",       "GA",              "Democratic",         "153910",
  "Hawaii",        "HI",              "Democratic",         "28313",
  "Idaho",         "ID",              "Republican",         "216443",
  "Illinois",      "IL",              "Democratic",         "149995",
  "Indiana",       "IN",              "Republican",         "94326",
  "Iowa",          "IA",              "Republican",         "145746",
  "Kansas",        "KS",              "Republican",         "213100",
  "Kentucky",      "KY",              "Republican",         "104656",
  "Louisiana",     "LA",              "Republican",         "135659",
  "Maine",         "ME",              "Democratic",         "91633",
  "Maryland",      "MD",              "Democratic",         "32131",
  "Massachusetts", "MA",              "Democratic",         "27336",
  "Michigan",      "MI",              "Democratic",         "250487",
  "Minnesota",     "MN",              "Democratic",         "225163",
  "Mississippi",   "MS",              "Republican",         "125438",
  "Missouri",      "MO",              "Republican",         "180540",
  "Montana",       "MT",              "Republican",         "380831",
  "Nebraska",      "NE",              "Republican",         "200330",
  "Nevada",        "NV",              "Democratic",         "286380",
  "New Hampshire", "NH",              "Democratic",         "24214",
  "New Jersey",    "NJ",              "Democratic",         "22591",
  "New Mexico",    "NM",              "Democratic",         "314917",
  "New York",      "NY",              "Democratic",         "141297",
  "North Carolina","NC",              "Republican",         "139391",
  "North Dakota",  "ND",              "Republican",         "183108",
  "Ohio",          "OH",              "Republican",         "116098",
  "Oklahoma",      "OK",              "Republican",         "181037",
  "Oregon",        "OR",              "Democratic",         "254799",
  "Pennsylvania",  "PA",              "Democratic",         "119280",
  "Rhode Island",  "RI",              "Democratic",         "4001",
  "South Carolina","SC",              "Republican",         "82933",
  "South Dakota",  "SD",              "Republican",         "199729",
  "Tennessee",     "TN",              "Republican",         "109153",
  "Texas",         "TX",              "Republican",         "695662",
  "Utah",          "UT",              "Republican",         "219882",
  "Vermont",       "VT",              "Democratic",         "24906",
  "Virginia",      "VA",              "Democratic",         "110787",
  "Washington",    "WA",              "Democratic",         "184661",
  "West Virginia", "WV",              "Republican",         "62756",
  "Wisconsin",     "WI",              "Democratic",         "169635",
  "Wyoming",       "WY",              "Republican",         "253335"
)

state_data <- state_data |> 
  mutate(
    area_kmsq = as.numeric(area_kmsq)
    )

state_data$government_party <- as.factor(state_data$government_party)

str(state_data)
