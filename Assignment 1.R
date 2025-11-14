##***************************
##  Assignment 1
##  Lusia Lee
##***************************

# Added explicit install lines so anyone can run the script without missing package errors.

## package used ----
install.packages("tidyverse")
library(tidyverse)
install.packages("sf")
library(sf)
install.packages("dplyr")
library(dplyr)
install.packages("jtools")
library(jtools)
install.packages("ggplot2")
library(ggplot2)
install.packages("broom")
library(broom)

## theme setting ----
theme_black <- function(base_size = 12, base_family = "") {
  
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    
    theme(
      line= element_line(color="white"),
      # Specify axis options
      axis.line = element_blank(),  
      axis.text.x = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.text.y = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.ticks = element_line(color = "white", size  =  0.2),  
      axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),  
      axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),  
      axis.ticks.length = unit(0.3, "lines"),   
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "black"),  
      legend.key = element_rect(color = "white",  fill = "black"),  
      legend.key.size = unit(1.2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size*0.8, color = "white"), 
      legend.axis.line = element_line(color="white"),
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),  
      legend.position = "right",  
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "vertical",  
      legend.box = NULL, 
      # Specify panel options
      panel.background = element_rect(fill = "black", color  =  NA),  
      panel.border = element_rect(fill = NA, color = "white"),  
      panel.grid.major = element_line(color = "grey35"),  
      panel.grid.minor = element_line(color = "grey20"),  
      panel.margin = unit(0.5, "lines"),   
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = base_size*0.8, color = "white"),  
      strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = "black", fill = "black"),  
      plot.title = element_text(size = base_size*1.2, color = "white", margin=margin(b=10)),  
      plot.margin = unit(rep(1, 4), "lines")
    )
}

## Data Cleaning and Preparation ----
vespidae_raw <- read_tsv("C:/Users/lycar/OneDrive/Desktop/6210/Assignment 2/data/result.tsv")

# Having a link to the taxonomic data would have made the code more reproducible

# minimun value set up
min_bin <-10

Min_area <- 0.001 # (This is 0.001 km^2)

# filtering the data
vespidae_clean <- vespidae_raw %>%
  # extract coordination to latitude and longitude
  mutate(coord = as.character(coord)) %>%
  tidyr::extract(
    col = coord,
    into = c("lat", "lon"),
    regex = "\\[(-?[0-9.]+),\\s*(-?[0-9.]+)\\]",
    convert = TRUE
  ) %>%
  # select relevant columns only
  select(bin_uri, species, lat, lon, country_iso) %>%
  # filter out NA value rows
  filter(
    !is.na(bin_uri),
    !is.na(lat),
    !is.na(lon)
  ) %>%
  # filter only north american country
  filter(country_iso %in% c("CA", "US", "MX")) %>%
  # number of records per bin and filter based on the minimum value
  group_by(bin_uri) %>%
  add_count(name = "records_per_bin") %>%
  filter(records_per_bin >= min_bin) %>%
  # Ungroup to be safe for next steps
  ungroup()

# calculate the range
vespidae_range <- function(vespidae_clean) {
  tryCatch({
    area_m2 <- vespidae_clean %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
      st_union() %>%                                  
      st_convex_hull() %>%                          
      st_transform(crs = 102008) %>%                    
      st_area()                                 
    # Convert from m^2 to km^2
    as.numeric(area_m2 / 1000000)
  }, error = function(e) {
    NA_real_
  })
}

## Additional Map Plot

#

install.packages("rnaturalearth")
library(rnaturalearth)
install.packages("rnaturalearthdata")
library(rnaturalearthdata)

# Get basemap (USA, CAN, MEX)
na_countries <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  dplyr::filter(adm0_a3 %in% c("USA","CAN","MEX"))

# Make points as sf
vespidae_pts <- sf::st_as_sf(vespidae_clean, coords = c("lon","lat"), crs = 4326)

# Plot
p_map <- ggplot() +
  geom_sf(data = na_countries, fill = "grey95", color = "grey70", linewidth = 0.3) +
  geom_sf(data = vespidae_pts, alpha = 0.6, size = 0.8, color = "steelblue") +
  coord_sf(xlim = c(-170, -50), ylim = c(10, 75), expand = FALSE) +
  labs(title = "Vespidae records across North America", x = NULL, y = NULL) +
  theme_black()

print(p_map)


## Calculate Variables for Hypothesis Testing ----
bin_summary_1 <- vespidae_clean %>%
  group_by(bin_uri) %>%
  summarise(
    latitudinal_range = max(lat) - min(lat),
    median_latitude = median(lat),
    n_records = n()
  )

bin_summary_2 <- vespidae_clean %>%
  group_by(bin_uri) %>%
  filter(n_distinct(paste(lat, lon)) >= 3) %>%
  mutate(median_latitude = median(lat)) %>%
  nest(points = c(lat, lon)) %>%
  mutate(range_area_km2 = map_dbl(points, vespidae_range)) %>%
  filter(!is.na(range_area_km2)) %>%
  filter(!is.na(range_area_km2)) %>%
  filter(range_area_km2 > Min_area) %>%
  select(bin_uri, median_latitude, range_area_km2) %>%
  ungroup()

## Run Statistical Test ----
rapoport_model_1 <- lm(latitudinal_range ~ median_latitude, data = bin_summary_1)
summary(rapoport_model_1)

rapoport_model_2<- lm(log(range_area_km2) ~ median_latitude, data = bin_summary_2)
summary(rapoport_model_2)

#summ(rapoport_model_1)
#summ(rapoport_model_2)

## Visualize the Result ----
rapoport_plot_1 <- ggplot(bin_summary_1, aes(x = median_latitude, y = latitudinal_range)) +
  geom_point(alpha = 0.6, color = "yellow") +

## Visualize the Result ----
summ(rapoport_model_1)
summ(rapoport_model_2)

rapoport_plot_1 <- ggplot(bin_summary_1, aes(x = median_latitude, y = latitudinal_range)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Testing Rapoport's Rule in North American Vespidae",
    subtitle = "Each point represents one BIN (n >= 10 records)",
    x = "Median Latitude of BIN (°N)",
    y = "Latitudinal Range of BIN (max - min latitude)"
  ) +
  theme_black()

print(rapoport_plot_1)

rapoport_plot_2 <- ggplot(bin_summary_2, aes(x = median_latitude, y = log(range_area_km2))) +
  geom_point(alpha = 0.6, color = "purple") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Testing Rapoport's Rule in North American Vespidae",
    subtitle = "Each point represents one BIN (n >= 10 records)",
    x = "Median Latitude of BIN (°N)",
    y = "log(Minimum Convex hull of BIN) (km^2)"
  ) +
  theme_black()

print(rapoport_plot_2)

## Simple regression assumption check
# Checks if residuals look random and roughly normal
par(mfrow = c(2, 2))  # make a 2x2 grid of plots
plot(rapoport_model_1)  # base R diagnostic plots for model 1
plot(rapoport_model_2)  # base R diagnostic plots for model 2
par(mfrow = c(1, 1))    # reset plotting layout

#model1_lats <- bin_summary_1 %>%
  #select(median_latitude) %>%
  #mutate(model_source = "Model 1 (1D Range)") 

#model2_lats <- bin_summary_2 %>%
 # select(median_latitude) %>%
 # mutate(model_source = "Model 2 (2D Area)") 

#combined_lats <- bind_rows(model1_lats, model2_lats) # Combine them into one data frame

#ggplot(combined_lats, aes(x = median_latitude, fill = model_source)) +
 # geom_density(alpha = 0.4) +  # alpha adds transparency
  #labs(
   # title = "Distribution of Median Latitudes Used in Each Model",
    #x = "Median Latitude of BIN (°N)",
    #y = "Density",
    #fill = "Model"
  #) +
  #theme_black()
  #theme_minimal()

