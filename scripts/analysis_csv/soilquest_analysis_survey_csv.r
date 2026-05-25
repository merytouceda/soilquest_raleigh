# SoilQuest survey analysis
# of the .csv downloaded data
# Author: Mery Touceda-Suarez


# Load libraries
library(tidyverse)
library(leaflet)      # for interactive map
library(htmltools)  # for interactive nmap formatting


# ────── 0. Load data and clean columns ─────────────────────────────────────────────
raw_data <- read_csv("~/Documents/GitHub/soilquest_raleigh/data/Urban Soil_May 22, 2026_07.20_pilot.csv")

# create a question number to text key: 
question_key <- raw_data %>%
  slice((1)) %>%
  pivot_longer(cols = everything(), names_to = "question_number", values_to = "question_text")

clean_data <- raw_data %>%
  slice(-(1:2)) %>%
  # rownames are the random IDs
  column_to_rownames(var = "Random ID") %>%
  # drop columns with all missing values
  select(where(~ !all(is.na(.x))))


# ────── 1. Fix the "Other (please describe) and clean up responses ─────────────────────────────────────────────
clean_data_fixed <- clean_data %>%
  # ────── DATE
  mutate(collection_date = mdy('Q6...22')) %>%
  # ────── SOIL TYPE
  mutate(soil_type = case_when(Q2 == "Other (please describe)" ~ Q2_5_TEXT, 
                        TRUE ~ Q2)) %>%
  #further cleaning
  mutate(soil_type = case_when(
    str_detect(soil_type, regex("urban garden", ignore_case = TRUE))     ~ "Urban garden",
    str_detect(soil_type, regex("woodland|forest|virgin", ignore_case = TRUE)) ~ "Woodland/Forest",
    str_detect(soil_type, regex("lawn|grass", ignore_case = TRUE))       ~ "Lawn",
    str_detect(soil_type, regex("curbside", ignore_case = TRUE))         ~ "Urban curbside",
    TRUE ~ soil_type   # keep anything else as-is
  )) %>%
  # ────── VEGETATION
  mutate(vegetation = case_when(
    Q10 == "Other (please describe)" ~ case_when(
      str_detect(Q10_4_TEXT, regex("tree|maple", ignore_case = TRUE))        ~ "Tree",
      str_detect(Q10_4_TEXT, regex("shrub|bush", ignore_case = TRUE))        ~ "Bush",
      str_detect(Q10_4_TEXT, regex("grass|lawn", ignore_case = TRUE))        ~ "Grass",
      str_detect(Q10_4_TEXT, regex("pollinator|perennial|flower|plant|tomato", ignore_case = TRUE)) ~ "Flowering plants",
      TRUE ~ "Other"  # anything that doesn't match a pattern
    ),
    TRUE ~ Q10  # keep Tree, Bush, Grass as-is
  ))




# ── 2. Extract & fix coordinates ─────────────────────────────────────────────

coords <- clean_data_fixed %>%
  select(lat = Q1_1, lon = Q1_2, soil_type = soil_type, moisture = Q7, 
         rain48 = Q5, air_temp = 'Q6...33', sun_exposure = 'Q8...35', vegetation = vegetation) %>%
  rownames_to_column(var = "id") %>%
  mutate(
    lat = as.numeric(lat),
    lon = as.numeric(lon),
    # Raleigh, NC is ~78°W — fix missing negative signs
    lon = if_else(lon > 0, -lon, lon)
  ) %>%
  drop_na(lat, lon)



pal1 <- colorFactor(
  palette = "Set2",       # any RColorBrewer palette, or a vector of hex codes
  domain  = coords$soil_type
)

pal <- colorFactor(
  palette = "Set1",       # any RColorBrewer palette, or a vector of hex codes
  domain  = coords$vegetation
)

leaflet(coords) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    lng       = ~lon,
    lat       = ~lat, 
    radius    = 8,
    color     = ~pal(vegetation),
    fillColor = ~pal(vegetation),
    fillOpacity = 0.8,
    weight    = 1,
    popup = ~paste0(
      "<b>Soil type:</b> ", soil_type, ", ", moisture, "<br>",
      "<b>Rain < 48h:</b> ", rain48, "<br>",
      "<b>Air Temperature:</b> ", air_temp, "<br>",
      "<b>Sun Exposure:</b> ", sun_exposure, "<br>",
      "<b>Vegetation</b> ", vegetation
    ),
    label = ~soil_type   # quick hover label
  ) %>%
  addLegend("bottomright", pal = pal, values = ~vegetation, title = "Soil type")

  
