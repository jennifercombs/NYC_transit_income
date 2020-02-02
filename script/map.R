# note: much of this code has come from that shared by Timo Grossenbacher
# in his blog: https://timogrossenbacher.ch/2019/04/bivariate-maps-with-ggplot2-and-sf/
# the color scheme was inspired by one shared by Joshua Stevens in his blog:
#https://www.joshuastevens.net/cartography/make-a-bivariate-choropleth-map/

#### 01 set up ####

library(tidyverse)
library(sf)
library(tidycensus)
library(ggthemes)
library(cowplot)

water1 <- st_read("data", "tl_2019_36047_areawater")
water2 <- st_read("data", "tl_2019_36081_areawater")
water3 <- st_read("data", "tl_2019_36085_areawater")
water4 <- st_read("data", "tl_2019_36061_areawater")
water5 <- st_read("data", "tl_2019_36005_areawater")
water <- rbind(water1, water2, water3, water4, water5)
rm(water1, water2, water3, water4, water5)

census_api_key("INSERT API KEY HERE")

nyc <- get_acs(geography = "tract",
               variables = c(medincome = "B19013_001",
                             total_workers = "B08006_001",
                             total_pt = "B08006_008"),
               state = "NY",
               county = c("Kings", "Queens", "Richmond", "Bronx", "New York"),
               geometry = TRUE) %>% 
  select(-moe) %>% 
  spread(key = variable, value = estimate) %>% 
  mutate(percent_pt = total_pt/total_workers)

#### 02 prep ####

quantiles_pt <- nyc %>%
  filter(!is.na(percent_pt)) %>% 
  pull(percent_pt) %>%
  quantile(probs = seq(0, 1, length.out = 4))

quantiles_inc <- nyc %>%
  filter(!is.na(medincome)) %>% 
  pull(medincome) %>%
  quantile(probs = seq(0, 1, length.out = 4))

bivariate_color_scale <- tibble(
  "3 - 3" = "#2a5a5b", # high transit, high income
  "2 - 3" = "#5a9178",
  "1 - 3" = "#73ae80", # low transit, high income
  "3 - 2" = "#567994",
  "2 - 2" = "#90b2b3", # medium transit, medium income
  "1 - 2" = "#a1c9a9",
  "3 - 1" = "#6c83b5", # high transit, low income
  "2 - 1" = "#b5c0da",
  "1 - 1" = "#d6e5e5") %>%  # low transit, low income
  gather("group", "fill")

nyc <- nyc %>%
  mutate(pt_quantiles = cut(
      percent_pt,
      breaks = quantiles_pt,
      include.lowest = TRUE),
    inc_quantiles = cut(
      medincome,
      breaks = quantiles_inc,
      include.lowest = TRUE),
    group = paste(
      as.numeric(pt_quantiles), "-",
      as.numeric(inc_quantiles))) %>% 
  left_join(bivariate_color_scale, by = "group")


#### 03 map ####

map <- 
  ggplot() +
  geom_sf(
    data = nyc,
    fill = "grey95",
    color = NA,
    size = 0.1)+
  geom_sf(
    data = nyc,
    aes(fill = fill),
    color = "white",
    size = 0.1)+
  scale_fill_identity()+
  geom_sf(data = water, fill = "white", color = NA)+
  labs(x = NULL,
       y = NULL,
       title = "Mapping NYC's Income and Transit Use",
       subtitle = paste0("Median household income and",
                   " percentage of commutes made by transit"),
       caption = "Source Data: 2018 ACS 5-year estimates, US Census Bureau\n
       Inspired by Timo Grossenbacher's blog post on bivariate maps \n
       https://timogrossenbacher.ch/2019/04/bivariate-maps-with-ggplot2-and-sf/\n
       Color scheme from https://www.joshuastevens.net/cartography/make-a-bivariate-choropleth-map/") +
  coord_sf(datum = NA)+
  theme(panel.grid = element_blank(),
    text = element_text(family = "SourceSansPro-Light", color = "grey10", lineheight = 0.5),
    plot.title = element_text(family = "SourceSansPro-Regular", size = 25, hjust = 0),
    plot.subtitle = element_text(size = 14, hjust = 0),
    plot.caption = element_text(size = 6, color = "grey50"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank()) +
  annotate( geom = "curve", 
            x = -74.05, 
            y = 40.87, 
            xend = -73.93, 
            yend = 40.84, 
            curvature = .3, 
            arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", 
           x = -74.145, 
           y = 40.88, 
           label = "Blue areas have\nhigh transit use and\nlow median incomes", 
           hjust = "left", 
           size = 2.5, 
           color = "grey30", 
           lineheight = .9)+
  annotate(geom = "curve", 
           x = -74.1, 
           y = 40.49, 
           xend = -74.2, 
           yend = 40.52, 
           curvature = .3, 
           arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text",
           x = -74.1, 
           y = 40.475, 
           label = "Green areas have\nlow transit use and\nhigh median incomes", 
           hjust = "left", 
           size = 2.5, 
           color = "grey30", 
           lineheight = .9)+
  annotate(geom = "curve", 
           x = -74.08, 
           y = 40.75, 
           xend = -73.985, 
           yend = 40.785, 
           curvature = .3, 
           arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", 
           x = -74.175, 
           y = 40.76, 
           label = "Dark teal areas have\nhigh transit use and\nhigh median incomes", 
           hjust = "left", 
           size = 2.5, 
           color = "grey30", 
           lineheight = .9)+
  annotate(geom = "curve", 
           x = -74.1, 
           y = 40.68, 
           xend = -74, 
           yend = 40.635, 
           curvature = .3, 
           arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", 
           x = -74.2, 
           y = 40.69, 
           label = "Light teal areas have\nlow transit use and\nlow median incomes", 
           hjust = "left",
           size = 2.5, 
           color = "grey30", 
           lineheight = .9)

bivariate_color_scale %<>%
  separate(group, into = c("pt", "inc"), sep = " - ") %>%
  mutate(pt = as.integer(pt),
         inc = as.integer(inc))

legend <- 
  ggplot() +
  geom_tile(
    data = bivariate_color_scale,
    mapping = aes(
      x = pt,
      y = inc,
      fill = fill)) +
  scale_fill_identity() +
  coord_fixed()+
  labs(x = expression("Higher transit use"%->%" "),
       y = expression('Higher income'%->%' '))+
  theme(text = element_text(family = "SourceSansPro-Light",
                            color = "grey10",
                            size = 7),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title.x = element_text(vjust = 4),
        axis.title.y = element_text(vjust = -3))

ggdraw() +
    draw_plot(map, 0, 0, 1, 1) +
    draw_plot(legend, 0.7425, 0.05, 0.175, 0.175)

ggplot2::ggsave("output.jpg", height = 8, width = 8, units = "in")

