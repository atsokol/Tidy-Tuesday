library(tidyverse)
library(sf)
library(mapproj)
library(patchwork)
library(stringr)
library(biscale)
library(ggtext)


temp <- tempfile()
unzip("./data/cb_2019_county.zip", exdir=temp)
counties <- list.files(temp, pattern = ".shp$",full.names=TRUE) %>% 
  st_read() %>% 
  filter(!STATEFP %in% c("02","15","60","66","69","72","78"))

income <- read_csv('./data/acs2019.csv') %>% 
  mutate(GEOID = str_extract(geoid, "(?<=US).*")) %>% 
  filter(GEOID!="") %>% 
  select(GEOID, name, income=B19013001)

broadband <- tidytuesdayR::tt_load("2021-05-11")$broadband %>%
  janitor::clean_names() %>%
  transmute(
    GEOID = as.character(county_id) %>% str_pad(5, side="left", pad="0"),
    usage = parse_number(broadband_usage, na = "-"),
    availability = parse_number(broadband_availability_per_fcc, na = "-")
  ) %>% 
  left_join(counties) %>% 
  left_join(income) %>% 
  drop_na() %>% 
  st_as_sf() %>% 
  select(GEOID, usage, availability, income) 

# Bivariate choropleth with Jenks binning
broadband_bi <- bi_class(broadband, 
                         x = income, y = usage, 
                         style = "jenks", dim = 3)

map <- ggplot() +
  geom_sf(broadband_bi, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  bi_theme() +
  coord_sf(crs = st_crs(5070))

legend<- bi_legend(pal = "GrPink",
                   dim = 3,
                   xlab = "Higher Income",
                   ylab = "Higher Usage",
                   size = 8)

(p1 <- map + 
    inset_element(legend, 0,0,0.25,0.25) +
    plot_annotation(
      title = "Broadband usage is correlated with household income",
      caption = "#TidyTuesday challenge  **Author**: Andrii Tsokol",
      theme = theme(plot.caption = element_markdown()))
  )

ggsave("./plots/bivariate.png", p1, device='png', dpi=300)


# Univariate choropleths 
classes_inc <- classIntervals(broadband$income, n = 5, style = "jenks")
classes_use <- classIntervals(broadband$usage, n = 5, style = "jenks")

broadband <- broadband %>% 
  mutate(income_class = cut(income, classes_inc$brks, include.lowest = TRUE),
         usage_class = cut(usage, classes_use$brks, include.lowest = TRUE))

plot_il <- function(variable) {
  broadband %>%
    st_as_sf() %>%
    ggplot(aes(fill = {{ variable }})) +
    geom_sf(color = "gray80", size = 0.05) +
    coord_sf(
      label_axes = "----",
      label_graticule = "----"
    ) +
    theme_void() +
    theme(panel.grid.major = element_blank())
}

(plot_il(income_class) +
    scale_fill_brewer() +
    labs(
      subtitle = "Household Income",
      fill = NULL
    ) + 

plot_il(usage_class) +
    scale_fill_brewer() +
    labs(
      subtitle = "Broadband Usage",
      fill = NULL) 
)



