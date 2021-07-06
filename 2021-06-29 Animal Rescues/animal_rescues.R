library(tidyverse)
library(sf)
library(osmextract)
library(ggforce)

rescues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-29/animal_rescues.csv')

# Download London borough and ward shapefiles
link <- 'https://data.london.gov.uk/download/statistical-gis-boundary-files-london/9ba8c833-6370-4b11-abdc-314aa020d5e0/statistical-gis-boundaries-london.zip'
file <- tempfile()
download.file(link, file)
boroughs <- unzip(file, exdir=tempdir()) %>% 
  str_subset("London_Borough(.+).shp$") %>% 
  st_read()

wards <- unzip(file, exdir=tempdir()) %>% 
  str_subset("London_Ward(.+).shp$") %>% 
  st_read()

# Transform to sf object
rescues_sf <- rescues %>% 
  mutate(easting_m = ifelse(easting_m=="NULL", easting_rounded, easting_m),
         northing_m = ifelse(northing_m=="NULL", northing_rounded, northing_m)) %>% 
  st_as_sf(coords = c("easting_m", "northing_m"), crs = st_crs(boroughs)) 

# Download the Thames polygon from OSM
thames <- oe_get("Greater London",  
                 query = "SELECT * FROM 'multipolygons' WHERE name == 'River Thames'", 
                 force_download = TRUE)

 
# Compose data to visualise
df_f <- rescues_sf %>% 
  st_join(wards, join=st_within) %>% 
  mutate(group = fct_infreq(animal_group_parent)) %>% 
  filter(group %in% c("Cat", "Bird", "Horse", "Deer")) %>% 
  group_by(GSS_CODE, group) %>% 
  summarise(n = n(), .groups='drop') %>%
  st_drop_geometry() %>% 
  left_join(wards) %>% 
  st_as_sf() 

# Compose annotations
code_bird <- rescues_sf %>% 
  st_join(wards, join=st_within) %>%
  filter(animal_group_parent == "Bird") %>% 
  group_by(GSS_CODE) %>% 
  summarise(n = n()) %>%
  st_drop_geometry() %>% 
  top_n(n=3, wt=n) %>% 
  pull(GSS_CODE)

annotation_bird <- wards %>% 
  filter(GSS_CODE %in% code_bird) %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  data.frame() %>% 
  mutate(id = row_number(),
         group = "Bird")

code_deer <- rescues_sf %>% 
  st_join(wards, join=st_within) %>%
  filter(animal_group_parent == "Deer") %>% 
  group_by(GSS_CODE) %>% 
  summarise(n = n()) %>%
  st_drop_geometry() %>% 
  top_n(n=1, wt=n) %>% 
  pull(GSS_CODE)

annotation_deer <- wards %>% 
  filter(GSS_CODE %in% code_deer) %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  data.frame() %>% 
  mutate(id = row_number(),
         group = "Deer")

annotation_horse <- wards %>% 
  st_union(by_feature=FALSE) %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  data.frame() %>% 
  mutate(id = row_number(),
         group = "Horse")

# Draw a map
(gg1 <- df_f %>%
    ggplot() +
    geom_sf(aes(fill = n), color=NA) +
    geom_sf(data=boroughs, color='grey80', alpha=0) +
    geom_sf(data=thames, fill="#05C3DD", color=NA) +
    geom_mark_ellipse(data=annotation_bird,
                      aes(x=X, y=Y, label="Three bird incident hotspots"),
                      label.fill = "transparent",
                      label.fontsize = 9) +
    geom_mark_ellipse(data=annotation_deer,
                      aes(x=X, y=Y, label="Only one deer incident hotspot"),
                      label.fill = "transparent",
                      label.fontsize = 9) +
    geom_mark_ellipse(data=annotation_horse,
                      aes(x=X, y=Y, label="No incidents with horses in Central London"),
                      label.fill = "transparent",
                      label.fontsize = 9) +
    facet_wrap(vars(group), nrow=2, ncol=2) +
    scale_fill_fermenter(palette = "YlOrBr", direction=1) +
    labs(title = "Animal rescues in London, 2009-2021",
         caption = "TidyTuesday challenge | Author: Andrii Tsokol") +
    theme_minimal() +
    theme(panel.background = element_rect(fill="white", color="white"),
          plot.background = element_rect(fill="white", color="white"),
          axis.title = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank())
)

ggsave('././2021-06-29 Animal Rescues/plots/Animal incident hotspots.png', gg1)


