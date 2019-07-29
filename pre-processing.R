# Indices of Multiple Deprivation 2019 #

library(sf) ; library(tidyverse) ; library(janitor)

# Source: Ministry of Housing, Communities and Local Government
# Publisher URL: https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019
# Licence: Open Government Licence 3.0


# ------------------------------------------

# Indices of Multiple Deptivation 2015 #

# Source: Ministry of Housing, Communities and Local Government
# Publisher URL: https://www.gov.uk/government/statistics/english-indices-of-deprivation-2015
# Licence: Open Government Licence 3.0

df <- read.csv("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/467774/File_7_ID_2015_All_ranks__deciles_and_scores_for_the_Indices_of_Deprivation__and_population_denominators.csv") %>% 
  clean_names()

imd15 <- filter(df, local_authority_district_name_2013 == "Trafford") %>% 
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

write_csv(imd15, "data/imd15.csv")

# ------------------------------------------

# Lower-layer Super Output Area boundaries #

# Source: ONS Open Geography Portal 
# Publisher URL: http://geoportal.statistics.gov.uk/
# Licence: Open Government Licence 3.0

lsoa <- st_read("https://ons-inspire.esriuk.com/arcgis/rest/services/Census_Boundaries/Lower_Super_Output_Areas_December_2011_Boundaries/MapServer/2/query?where=UPPER(lsoa11nm)%20like%20'%25TRAFFORD%25'&outFields=lsoa11cd,lsoa11nm&outSR=4326&f=geojson") %>% 
  st_as_sf(crs = 4326, coords = c("long", "lat")) 

# best-fit lookup between LSOAs and wards
best_fit_lookup <- st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LSOA11_WD18_LAD18_EW_LUv3/FeatureServer/0/query?where=LAD18NM%20like%20'%25Trafford%25'&outFields=LSOA11CD,WD18CD,WD18NM,LAD18CD,LAD18NM&outSR=4326&f=geojson") %>% 
  setNames(tolower(names(.)))  %>%
  st_set_geometry(NULL)

left_join(lsoa, best_fit_lookup, by = "lsoa11cd") %>%
  st_write("data/best_fit_lsoa.geojson", driver = "GeoJSON")

