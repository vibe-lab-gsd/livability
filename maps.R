## Maps showing variation in 
#   * Housing affordability: Average housing cost as a share of income
#   * Employment: Unemployment rate
#   * Transportation: Ratio of non-car access to car access
##################
options(tigris_use_cache = TRUE)


library(here)
library(tidyverse)
library(sf)
library(tidycensus)
library(tigris)
library(rnaturalearth)
library(ggthemes)

sf_use_s2(FALSE)

## List of required variables

acs_vars <- c(med_house_mo = "B25105_001",
              med_inc_year = "B21004_001",
              num_workers = "B23025_003",
              num_unempl = "B23025_005")

## Map city locations

# NOTE: Removed Putnam Connecticut, Essex Vermont, Colchester Vermont
place_list <- here("cities-plan-pts.csv") |>
  read_csv() |>
  mutate(append_borough = paste0(Place, " borough"),
         append_city = paste0(Place, " city"),
         append_village = paste0(Place, " village"),
         append_town = paste0(Place, " town"),
         append_CDP = paste0(Place, " CDP")) |> 
  arrange(Place, State)

states <- place_list |>
  group_by(State) |>
  summarise(num = n())

all_cities <- vector("list", nrow(states))

for (state in 1:nrow(states)) {
  places_in_state <- place_list |>
    filter(State == states$State[state])
  
  city_pattern <- 
    paste(
      paste(places_in_state$append_borough,
            collapse = "|"),
      paste(places_in_state$append_city,
            collapse = "|"),
      paste(places_in_state$append_village,
             collapse = "|"),
      paste(places_in_state$append_town,
            collapse = "|"),
      paste(places_in_state$append_CDP,
            collapse = "|"),
    sep = "|")
  
  these_cities <- get_acs(geography = "place", 
                    variables = acs_vars,
                    state = states$State[state],
                    output = "wide",
                    geometry = TRUE) 
  
  ## Update here to grab the plan points too.
  these_cities <- these_cities |>
    filter(str_detect(string = these_cities$NAME,
                      pattern = city_pattern))
  
  all_cities[[state]] <- these_cities
  
  if(states$num[state] > nrow(these_cities)) {
    print(paste0(states$num[state] - nrow(these_cities), 
                 " too few cities found in ", states$State[state]))
  }
  if(states$num[state] < nrow(these_cities)) {
    print(paste0(nrow(these_cities) - states$num[state], 
                 " too many cities found in ", states$State[state]))
  }
}

all_cities <- bind_rows(all_cities) |>
  filter(!NAME %in% c("West Chicago city, Illinois",
                      "West Peoria city, Illinois",
                      "South Jacksonville village, Illinois",
                      "South Elgin village, Illinois",
                      "North Chicago city, Illinois",
                      "New Canton town, Illinois",
                      "East Galesburg village, Illinois",
                      "East Peoria city, Illinois",
                      "Marion CDP, Indiana",
                      "Mount Auburn town, Indiana",
                      "Mount Carmel town, Indiana",
                      "Mount Sterling city, Illinois",
                      "New Goshen CDP, Indiana",
                      "New Marion CDP, Indiana",
                      "New Richmond town, Indiana",
                      "New Washington CDP, Indiana",
                      "North Terre Haute CDP, Indiana",
                      "West Terre Haute town, Indiana",
                      "East Grand Rapids city, Michigan",
                      "New Troy CDP, Michigan",
                      "North Muskegon city, Michigan",
                      "South Monroe CDP, Michigan",
                      "West Monroe CDP, Michigan",
                      "North Mankato city, Minnesota",
                      "North St. Paul city, Minnesota",
                      "South St. Paul city, Minnesota",
                      "West St. Paul city, Minnesota",
                      "West Fargo city, North Dakota",
                      "Castleton-on-Hudson village, New York",
                      "Cornwall-on-Hudson village, New York",
                      "Croton-on-Hudson village, New York",
                      "East Ithaca CDP, New York",
                      "East Kingston CDP, New York",
                      "East Rochester village, New York",
                      "East Syracuse village, New York",
                      "Grand View-on-Hudson village, New York",
                      "Hastings-on-Hudson village, New York",
                      "Malden-on-Hudson CDP, New York",
                      "Northeast Ithaca CDP, New York",
                      "North Syracuse village, New York",
                      "Northwest Ithaca CDP, New York",
                      "South Corning village, New York",
                      "South Glens Falls village, New York",
                      "University at Buffalo CDP, New York",
                      "West Elmira CDP, New York",
                      "West Glens Falls CDP, New York",
                      "Woodbury CDP, New York",
                      "East Canton village, Ohio",
                      "East Cleveland city, Ohio",
                      "East Springfield CDP, Ohio",
                      "Lower Salem village, Ohio",
                      "New Athens village, Ohio",
                      "New Springfield CDP, Ohio",
                      "North Canton city, Ohio",
                      "North Lima CDP, Ohio",
                      "North Zanesville CDP, Ohio",
                      "Putnam CDP, Connecticut",
                      "South Mount Vernon CDP, Ohio",
                      "South Salem village, Ohio",
                      "South Zanesville village, Ohio",
                      "Upper Sandusky city, Ohio",
                      "West Mansfield village, Ohio",
                      "West Portsmouth CDP, Ohio",
                      "West Salem village, Ohio",
                      "East Altoona CDP, Pennsylvania",
                      "East Berwick CDP, Pennsylvania",
                      "East Pittsburgh borough, Pennsylvania",
                      "East York CDP, Pennsylvania",
                      "New Bethlehem borough, Pennsylvania",
                      "New Lebanon borough, Pennsylvania",
                      "North Warren CDP, Pennsylvania",
                      "North York borough, Pennsylvania",
                      "South Bethlehem borough, Pennsylvania",
                      "South Williamsport borough, Pennsylvania",
                      "University of Pittsburgh Bradford CDP, Pennsylvania",
                      "University of Pittsburgh Johnstown CDP, Pennsylvania",
                      "West Reading borough, Pennsylvania",
                      "West Sunbury borough, Pennsylvania",
                      "West York borough, Pennsylvania",
                      "East Barre CDP, Vermont",
                      "North Bennington village, Vermont",
                      "Old Bennington village, Vermont",
                      "South Barre CDP, Vermont",
                      "South Woodstock CDP, Vermont",
                      "West Brattleboro CDP, Vermont",
                      "West Rutland CDP, Vermont",
                      "West Woodstock CDP, Vermont",
                      "North Fond du Lac village, Wisconsin",
                      "South Milwaukee city, Wisconsin",
                      "West Baraboo village, Wisconsin",
                      "West Milwaukee village, Wisconsin",
                      "Waukesha village, Wisconsin")) |>
  mutate(state = str_extract(NAME, "(?<=, ).*")) |>
  mutate(housing = med_house_moE * 12 / med_inc_yearE,
         unemp = num_unemplE / num_workersE) |>
  mutate(housing_trunc = ifelse(housing > 0.5, 0.5, housing),
         unemp_trunc = ifelse(unemp > 0.1, 0.1, unemp)) |>
  arrange(NAME) |>
  mutate(plan_pts = place_list$`Plan-pts`,
         check_name = place_list$Place)

states_map <- states() |>
  filter(STUSPS %in% states$State | STUSPS %in% c("RI", "NJ")) 

great_lakes <- ne_download(type = "lakes", category = "physical", scale = 50) |>
  st_transform(crs = st_crs(states_map)) |>
  st_filter(states_map)

cape_cod <- area_water(state = "MA", county = c("Barnstable", 
                                                "Plymouth", 
                                                "Suffolk",
                                                "Nantucket",
                                                "Dukes"))

long_island <- area_water(state = "NY", county = c("Suffolk", 
                                                   "Nassau",
                                                   "Queens",
                                                   "Kings",
                                                   "New York"))

nj_water <- area_water(state = "NJ", county = c("Bergen", 
                                                "Hudson",
                                                "Union",
                                                "Middlesex",
                                                "Monmouth"))

crs_frostbelt = "+proj=lcc +lat_0=39 +lon_0=-89 +lat_1=37 +lat_2=47 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +type=crs"

base_map <- ggplot(states_map) +
  geom_sf(fill = "gray90",
          color = "white") +
  geom_sf(data = great_lakes,
          color = NA, 
          fill = "white") +
  geom_sf(data = cape_cod, fill = "white", color = NA) +
  geom_sf(data = long_island, fill = "white", color = NA) +
  geom_sf(data = nj_water, fill = "white", color = NA)  +
  theme_map()

base_map +
  geom_sf(data = all_cities,
          color = NA,
          aes(fill = housing_trunc)) +
  scale_fill_viridis_c(name = "Median housing\ncosts as a share\nof median income",
                       breaks = seq(0.25, 0.5, by=0.05),
                       labels = c(paste0(seq(25, 45, by=5),"%"), "50% or more")) +
  coord_sf(crs = crs_frostbelt)

base_map +
  geom_sf(data = all_cities,
          color = NA,
          aes(fill = unemp_trunc)) +
  scale_fill_viridis_c(name = "Unemployment\nrate",
                       breaks = seq(0, 0.1, by = 0.02),
                       labels = c(paste0(seq(0, 8, by=2), "%"), "10% or more")) +
  coord_sf(crs = crs_frostbelt)

#### Transportation

access_cbsas <- here("data-access") |>
  list.dirs(recursive = FALSE, full.names = FALSE)

for (i in 1:length(access_cbsas)) {
  cbsa <- access_cbsas[i]
  
  access_vals <- here("data-access",
                      cbsa,
                      "access_calc.csv") |>
    read_csv() |>
    mutate(id = as.character(id))
  
  block_pts <- here("data-access",
                    cbsa,
                    "block-data.geojson") |>
    st_read() 
  
  which_state_poly <- states_map |>
    st_transform("WGS84") |>
    st_filter(block_pts)
  
  which_state <- which_state_poly$NAME
  
  places_in_state <- all_cities |>
    filter(state %in% which_state) |>
    st_transform("WGS84")
  
  for (j in 1:nrow(places_in_state)) {
    
    this_city_geom <- places_in_state[j,]
    
    this_city_pts <- block_pts |>
      st_filter(this_city_geom)
    
    if (nrow(this_city_pts) > 0) {
      this_city_access <- access_vals |>
        filter(id %in% this_city_pts$id) |>
        mutate(ratio = no_car_access / car_access)
      
      avg_access_ratio <- sum(this_city_access$ratio * this_city_access$n_HHs) /
        sum(this_city_access$n_HHs)
      
      if(!is.na(avg_access_ratio)) {
        all_cities$avg_access_ratio[all_cities$NAME == this_city_geom$NAME[1]] <- 
          avg_access_ratio
      }
    }
  }
  
}

all_cities <- all_cities |>
  mutate(access_trunc = ifelse(avg_access_ratio < 0.05, 0.05, 
                               avg_access_ratio))

base_map +
  geom_sf(data = all_cities,
          color = NA,
          aes(fill = access_trunc)) +
  scale_fill_viridis_c(name = "Car-free\naccessibility\n(relative to\naccess by car",
                       breaks = seq(0.05, 0.35, by = 0.05),
                       labels = c("5% or less", 
                                  paste0(seq(10, 35, by=5), "%")),
                       direction = -1) +
  coord_sf(crs = crs_frostbelt)

### 