library(tidyverse)
library(sf)
library(ggplot2)

survey <- tidytuesdayR::tt_load("2021-05-18")$survey %>%
  janitor::clean_names() %>% 
  mutate(other_monetary_comp = as.numeric(other_monetary_comp))

temp <- tempfile()
unzip("./2021-05-18 Salary Survey/data/places.zip", exdir=temp)
places <- list.files(temp, pattern = ".shp$",full.names=TRUE) %>% 
  st_read() 

temp2 <- tempfile()
unzip("./2021-05-18 Salary Survey/data/states.zip", exdir=temp2)
states <- list.files(temp2, pattern = ".shp$",full.names=TRUE) %>% 
  st_read() %>%  
  filter(!STATEFP %in% c("02","15","60","66","69","72","78"))

temp3 <- tempfile()
unzip("./2021-05-18 Salary Survey/data/counties.zip", exdir=temp3)
counties <- list.files(temp3, pattern = ".shp$",full.names=TRUE) %>% 
  st_read() %>% 
  filter(!STATEFP %in% c("02","15","60","66","69","72","78"))

survey_geo <- survey %>% 
  inner_join(select(states, STATEFP,NAME) %>% st_drop_geometry(), by= c("state"="NAME")) %>% 
  inner_join(select(places, STATEFP,PLACEFP,GEOID,NAME), by= c("STATEFP"="STATEFP", "city"="NAME")) %>% 
  st_as_sf()

# Number of observations per county
count <- aggregate(select(survey_geo, annual_salary), 
                    counties, 
                    length) 
  
count %>% 
  ggplot(aes(fill=annual_salary)) +
  geom_sf(color = "gray70", size = 0.1) +
  scale_fill_viridis_b(n.breaks = 5, 
                       labels = scales::comma_format(accuracy = 1),
                       na.value = "grey90") +
  coord_sf(crs = st_crs(5070)) +
  theme_void()

# Median salaries by state in top industries
top <- survey %>% 
  group_by(industry) %>% 
  summarise(count=n()) %>% 
  arrange(desc(count))

survey %>% 
  mutate(salary = annual_salary + replace_na(other_monetary_comp, 0)) %>% 
  filter(industry %in% top$industry[1:4]) %>% 
  group_by(industry, state) %>% 
  summarise(salary = stats::median(salary)) %>% 
  inner_join(select(states,NAME), by= c("state"="NAME")) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(data=states, color='grey70', fill='grey90', size=0.1) +
  geom_sf(aes(fill=salary), color = "grey70", size = 0.1) +
  scale_fill_viridis_b(n.breaks = 6, 
                       labels = scales::comma_format(accuracy = 1),
                       na.value = "grey90") +
  facet_wrap(~industry) +
  coord_sf(crs = st_crs(5070)) +
  labs(title = "Median salaries") +
  theme_void()

# Median salaries by county
survey_geo %>% 
  mutate(salary = annual_salary + replace_na(other_monetary_comp, 0)) %>% 
  select(salary) %>%
  aggregate(counties, stats::median) %>% 
  ggplot(aes(fill=salary)) +
  geom_sf(color = "gray70", size = 0.1) +
  scale_fill_viridis_b(n.breaks = 5, 
                       labels = scales::comma_format(accuracy = 1),
                       na.value = "grey90") +
  coord_sf(crs = st_crs(5070)) +
  labs(title = "Median salaries in tech industry",
       subtitle = "California is the highest-paid state for IT professionals") +
  theme_void()

# Median salaries by state and by education level
survey_geo %>% 
  mutate(salary = annual_salary + replace_na(other_monetary_comp, 0)) %>% 
  mutate(education = 
           as.factor(highest_level_of_education_completed)) %>% 
  select(salary, education) %>%
  nest(data=!education) %>% 
  mutate(agg = lapply(data, function(df) aggregate(df, states, stats::median))) %>% 
  select(education, agg) %>% 
  unnest() %>%
  st_as_sf() %>% 
  ggplot(aes(fill=salary)) +
  geom_sf(color = "gray70", size = 0.1) +
  facet_wrap(~education) +
  scale_fill_viridis_b(n.breaks = 7, 
                       labels = scales::comma_format(accuracy = 1),
                       na.value = "grey90") +
  coord_sf(crs = st_crs(5070)) +
  labs(title = "Income by education level") +
  theme_void()

