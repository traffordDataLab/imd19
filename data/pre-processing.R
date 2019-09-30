# English Indices of Deprivation 2019 #

library(sf) ; library(tidyverse) ; library(janitor)


# Source: Ministry of Housing, Communities and Local Government
# Publisher URL: https://www.gov.uk/government/statistics/announcements/english-indices-of-deprivation-2019
# Licence: Open Government Licence 3.0

imd19 <- read_csv("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833982/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators.csv") %>% 
  clean_names() %>% 
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
  mutate(year = "2019")

# ------------------------------------------

# English Indices of Deprivation 2015 #

# Source: Ministry of Housing, Communities and Local Government
# Publisher URL: https://www.gov.uk/government/statistics/english-indices-of-deprivation-2015
# Licence: Open Government Licence 3.0

imd15 <- read_csv("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/467774/File_7_ID_2015_All_ranks__deciles_and_scores_for_the_Indices_of_Deprivation__and_population_denominators.csv") %>% 
  clean_names() %>% 
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
  mutate(year = "2015")

# Bind IoD2019 and IoD2015
bind_rows(imd19, imd15) %>%
  write_csv("imd.csv")

# ------------------------------------------

# Statistical and administrative geographies #

# Source: ONS Open Geography Portal 
# Publisher URL: http://geoportal.statistics.gov.uk/
# Licence: Open Government Licence 3.0

# Lower-layer Super Output Area boundaries #
lsoa <- st_read("https://opendata.arcgis.com/datasets/da831f80764346889837c72508f046fa_3.geojson") %>% 
  select(lsoa11cd) %>% 
  filter(str_detect(lsoa11cd, "^E")) %>%
  st_as_sf(crs = 4326, coords = c("long", "lat"))

# Best-fit lookup between LSOAs and wards
best_fit_lookup <- read_csv("https://opendata.arcgis.com/datasets/8c05b84af48f4d25a2be35f1d984b883_0.csv") %>% 
  setNames(tolower(names(.)))  %>%
  filter(str_detect(lsoa11cd, "^E")) %>%
  select(lsoa11cd, lsoa11nm, wd18cd, wd18nm)

# Create LSOA > LA lookup for 2019 boundaries using #IoD2019 data
lookup <- read_csv("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833982/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators.csv") %>% 
  clean_names() %>% 
  select(lsoa11cd = 1, lad19cd = 3, lad19nm = 4) %>% 
  distinct(lsoa11cd, .keep_all = TRUE)

# Join and write results
left_join(lsoa, best_fit_lookup, by = "lsoa11cd") %>% 
  left_join(lookup, by = "lsoa11cd") %>%
  st_write("best_fit_lsoa.geojson")

# Electoral ward boundaries
# st_read("https://opendata.arcgis.com/datasets/a0b43fe01c474eb9a18b6c90f91664c2_3.geojson") %>% 
#   filter(str_detect(wd18cd, "^E")) %>%
#   left_join(select(best_fit_lookup, wd18cd, lad18cd, lad18nm), by = "wd18cd") %>% 
#   select(wd18cd, wd18nm, lad18cd, lad18nm, lon = long, lat) %>% 
#   st_write("wards.geojson")

