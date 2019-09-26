# Indices of Multiple Deprivation 2019 #

library(sf) ; library(tidyverse) ; library(janitor)

# Create a string object with the name of your local authority
la <- "Trafford"

# Source: Ministry of Housing, Communities and Local Government
# Publisher URL: https://www.gov.uk/government/statistics/announcements/english-indices-of-deprivation-2019
# Licence: Open Government Licence 3.0

df19 <- read.csv("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833982/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators.csv") %>% 
  clean_names()

imd19 <- filter(df19, local_authority_district_name_2019 == la) %>% 
  select(lsoa11cd = 1, 5:34) %>% 
  gather(variable, value, -lsoa11cd) %>% 
  mutate(measure = case_when(str_detect(variable, "score") ~ "score", 
                             str_detect(variable, "decile") ~ "decile", 
                             str_detect(variable, "rank") ~ "rank"),
         index_domain = case_when(str_detect(variable, "index_of_multiple_deprivation") ~ "Index of Multiple Deprivation", 
                                  str_detect(variable, "employment") ~ "Employment",
                                  str_detect(variable, "education") ~ "Education, Skills and Training",
                                  str_detect(variable, "health") ~ "Health Deprivation and Disability",
                                  str_detect(variable, "crime") ~ "Crime",
                                  str_detect(variable, "barriers") ~ "Barriers to Housing and Services",
                                  str_detect(variable, "living") ~ "Living Environment",
                                  str_detect(variable, "idaci") ~ "Income Deprivation Affecting Children",
                                  str_detect(variable, "idaopi") ~ "Income Deprivation Affecting Older People",
                                  TRUE ~ "Income")) %>% 
  select(lsoa11cd,
         measure,
         value,
         index_domain) %>% 
  spread(measure, value) %>% 
  mutate(year = "2019",
         decile = as.character(decile),
         rank = as.character(rank),
         score = as.character(score))

# write_csv(imd19, "imd19.csv")

# ------------------------------------------

# Indices of Multiple Deprivation 2015 #

# Source: Ministry of Housing, Communities and Local Government
# Publisher URL: https://www.gov.uk/government/statistics/english-indices-of-deprivation-2015
# Licence: Open Government Licence 3.0

df15 <- read.csv("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/467774/File_7_ID_2015_All_ranks__deciles_and_scores_for_the_Indices_of_Deprivation__and_population_denominators.csv") %>% 
  clean_names()

imd15 <- filter(df15, local_authority_district_name_2013 == la) %>% 
  select(lsoa11cd = 1, 5:34) %>% 
  gather(variable, value, -lsoa11cd) %>% 
  mutate(measure = case_when(str_detect(variable, "score") ~ "score", 
                             str_detect(variable, "decile") ~ "decile", 
                             str_detect(variable, "rank") ~ "rank"),
         index_domain = case_when(str_detect(variable, "index_of_multiple_deprivation") ~ "Index of Multiple Deprivation", 
                                  str_detect(variable, "employment") ~ "Employment",
                                  str_detect(variable, "education") ~ "Education, Skills and Training",
                                  str_detect(variable, "health") ~ "Health Deprivation and Disability",
                                  str_detect(variable, "crime") ~ "Crime",
                                  str_detect(variable, "barriers") ~ "Barriers to Housing and Services",
                                  str_detect(variable, "living") ~ "Living Environment",
                                  str_detect(variable, "idaci") ~ "Income Deprivation Affecting Children",
                                  str_detect(variable, "idaopi") ~ "Income Deprivation Affecting Older People",
                                  TRUE ~ "Income")) %>% 
  select(lsoa11cd,
         measure,
         value,
         index_domain) %>% 
  spread(measure, value) %>% 
  mutate(year = "2015",
         decile = as.character(decile),
         rank = as.character(rank),
         score = as.character(score))

# write_csv(imd15, "imd15.csv")

bind_rows(imd19, imd15) %>% 
  write_csv("imd.csv")

# ------------------------------------------

# Statistical geographies #

# Source: ONS Open Geography Portal 
# Publisher URL: http://geoportal.statistics.gov.uk/
# Licence: Open Government Licence 3.0

# Lower-layer Super Output Area boundaries #
lsoa <- st_read(paste0("https://ons-inspire.esriuk.com/arcgis/rest/services/Census_Boundaries/Lower_Super_Output_Areas_December_2011_Boundaries/MapServer/2/query?where=UPPER(lsoa11nm)%20like%20'%25", URLencode(toupper(la), reserved = TRUE), "%25'&outFields=lsoa11cd,lsoa11nm&outSR=4326&f=geojson")) %>% 
  st_as_sf(crs = 4326, coords = c("long", "lat")) 

# Best-fit lookup between LSOAs and wards
best_fit_lookup <- st_read(paste0("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LSOA11_WD18_LAD18_EW_LUv3/FeatureServer/0/query?where=UPPER(lad18nm)%20like%20'%25", URLencode(toupper(la), reserved = TRUE), "%25'&outFields=LSOA11CD,WD18CD,WD18NM,LAD18CD,LAD18NM&outSR=4326&f=geojson")) %>% 
  setNames(tolower(names(.)))  %>%
  st_set_geometry(NULL)

left_join(lsoa, best_fit_lookup, by = "lsoa11cd") %>% 
  st_write("best_fit_lsoa.geojson", driver = "GeoJSON")

