# Load in USA Covid19 data
covid19_usa <- covid19("United States", level = 2)
view(covid19_usa)

# Snakecase all names
names(covid19_usa) <- to_snake_case(names(covid19_usa))

# Rename columns
covid19_usa <- 
  covid19_usa |> 
  rename(
    country = administrative_area_level_1,
    state = administrative_area_level_2
  ) 
  
view(distinct(covid19_usa, state))


# Remove non-mainland states and regions
covid19_usa <- 
  covid19_usa |> 
  filter(! state %in% c('American Samoa', 'Puerto Rico', 'Guam', 'Virgin Islands', 'Northern Mariana Islands'))

# Create standard df
state_data <- tibble::tribble(
  ~state,          ~abbreviation,     ~government_party,    ~area_kmsq,
  "alabama",       "AL",              "Republican",         "135767",
  "alaska",        "AK",              "Republican",         "1723337",
  "arizona",       "AZ",              "Democratic",         "295234",
  "arkansas",      "AR",              "Republican",         "137732",
  "california",    "CA",              "Democratic",         "423967",
  "colorado",      "CO",              "Democratic",         "269601",
  "connecticut",   "CT",              "Democratic",         "14357",
  "delaware",      "DE",              "Democratic",         "6446",
  "florida",       "FL",              "Republican",         "170312",
  "georgia",       "GA",              "Democratic",         "153910",
  "hawaii",        "HI",              "Democratic",         "28313",
  "idaho",         "ID",              "Republican",         "216443",
  "illinois",      "IL",              "Democratic",         "149995",
  "indiana",       "IN",              "Republican",         "94326",
  "iowa",          "IA",              "Republican",         "145746",
  "kansas",        "KS",              "Republican",         "213100",
  "kentucky",      "KY",              "Republican",         "104656",
  "louisiana",     "LA",              "Republican",         "135659",
  "maine",         "ME",              "Democratic",         "91633",
  "maryland",      "MD",              "Democratic",         "32131",
  "massachusetts", "MA",              "Democratic",         "27336",
  "michigan",      "MI",              "Democratic",         "250487",
  "minnesota",     "MN",              "Democratic",         "225163",
  "mississippi",   "MS",              "Republican",         "125438",
  "missouri",      "MO",              "Republican",         "180540",
  "montana",       "MT",              "Republican",         "380831",
  "nebraska",      "NE",              "Republican",         "200330",
  "nevada",        "NV",              "Democratic",         "286380",
  "new hampshire", "NH",              "Democratic",         "24214",
  "new jersey",    "NJ",              "Democratic",         "22591",
  "new mexico",    "NM",              "Democratic",         "314917",
  "new york",      "NY",              "Democratic",         "141297",
  "north carolina","NC",              "Republican",         "139391",
  "north dakota",  "ND",              "Republican",         "183108",
  "ohio",          "OH",              "Republican",         "116098",
  "oklahoma",      "OK",              "Republican",         "181037",
  "oregon",        "OR",              "Democratic",         "254799",
  "pennsylvania",  "PA",              "Democratic",         "119280",
  "rhode island",  "RI",              "Democratic",         "4001",
  "south carolina","SC",              "Republican",         "82933",
  "south dakota",  "SD",              "Republican",         "199729",
  "tennessee",     "TN",              "Republican",         "109153",
  "texas",         "TX",              "Republican",         "695662",
  "utah",          "UT",              "Republican",         "219882",
  "vermont",       "VT",              "Democratic",         "24906",
  "virginia",      "VA",              "Democratic",         "110787",
  "washington",    "WA",              "Democratic",         "184661",
  "west virginia", "WV",              "Republican",         "62756",
  "wisconsin",     "WI",              "Democratic",         "169635",
  "wyoming",       "WY",              "Republican",         "253335"
)

state_data <- state_data |> 
  mutate(
    state  = str_to_title(state),
    area_kmsq = as.numeric(area_kmsq)
    )

