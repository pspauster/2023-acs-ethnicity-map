library(tidyverse)
library(tidycensus)
library(mapgl)
library(tigris)
library(sf)

vars <- load_variables(2023, "acs5", cache = T) %>% 
  separate(name, sep = "_", into = c("name", "subname"))

eth_vars <- vars %>% filter(str_sub(name, 1,6) %in% c("B02018", "B02019",
                            "B02020", "B03001",
                            "B03002", "B04006")) %>% 
  mutate(label_short = str_extract(label, "(?<=!!)[^!!]*$")) %>% 
  mutate(count_exclamations = str_count(label, "!!")) %>% 
  filter(!str_detect(label_short, "Total"), !str_detect(label_short, ":$"),
         !str_detect(label_short, "alone"), !str_detect(label_short, "Not Hispanic or Latino"),
         !str_detect(label_short, "Other groups"), !str_detect(label_short, "Unclassified or not reported"))

vars_final <- setNames(eth_vars$name, eth_vars$label_short)

ethnicity <- get_acs("tract",
                     variables = vars_final,
                     year=2023,
                     state = "NY",
                     county = c("New York", "Bronx", "Richmond", "Kings", "Queens"),
                     geometry = T)%>%
  st_transform(26918) %>%
  erase_water(year = 2020)


ethnicity_top <- ethnicity %>% 
  group_by(GEOID) %>% 
  arrange(desc(estimate)) %>% 
  summarize(top_ethnicity = first(variable),
            top_ethnicity_value = first(estimate),
            second_eth = nth(variable, 2),
            second_eth_value = nth(estimate, 2),
            third_eth = nth(variable, 3),
            third_eth_value = nth(estimate, 3))

eth_top_clean <- ethnicity_top %>% 
  mutate(top_ethnicity = if_else(top_ethnicity_value == 0, "No data", top_ethnicity),
         html = paste("1. ","<strong>", top_ethnicity, "</strong>", " ",
                       scales::comma(top_ethnicity_value),"<br>",
                      "2. ",second_eth, scales::comma(second_eth_value), "<br>",
                      "3. ",third_eth, scales::comma(third_eth_value))
         ) %>% 
  ungroup() %>% 
  group_by(top_ethnicity) %>% 
  mutate(count = n()) %>% 
  arrange(desc(count))



ethmap <- mapboxgl(bounds = eth_top_clean) |> 
  add_fill_layer(id = "nyc_data",
                 source = eth_top_clean,
                 fill_color = match_expr(
                   column = "top_ethnicity",
                   values = unique(eth_top_clean$top_ethnicity),
                   stops = hcl(seq(15, 375, length.out = length(unique(eth_top_clean$top_ethnicity))), 100, 65)
                 ),
                 fill_opacity = 0.5,
                 tooltip = "html",
  ) %>% 
  add_categorical_legend(
    legend_title = "Ethnicities",
    values = unique(eth_top_clean$top_ethnicity),
    colors = hcl(seq(15, 375, length.out = length(unique(eth_top_clean$top_ethnicity))), 100, 65),
  )

ethmap

htmltools::save_html(ethmap, "ethnicity_map_nyc.html")

