library(here)
library(tidyverse)
library(sf)
library(tigris)

city_data <- here("data-for-figs.geojson") |>
  st_read()

all_counties <- counties(year = 2020) |>
  select(STATEFP, GEOID) 

frostbelt_counties <- all_counties |>
  filter(STATEFP %in% c("09",
                         "17",
                         "18",
                         "23",
                         "25",
                         "26",
                         "27",
                         "33",
                         "36",
                         "38",
                         "39",
                         "42",
                         "50",
                         "55") &
           !GEOID %in% c("36103",
                        "36059",
                        "36081",
                        "36085",
                        "36047",
                        "36061",
                        "36005",
                        "36119",
                        "09190",
                        "09170",
                        "09130",
                        "09180",
                        "09120",
                        "25005",
                        "25023",
                        "25007",
                        "25019",
                        "25001",
                        "25021",
                        "25025",
                        "25009",
                        "33015",
                        "33017",
                        "23003",
                        "23029",
                        "23009",
                        "23027",
                        "23013",
                        "23015",
                        "23023",
                        "23005",
                        "23031"))
           
out_of_frostbelt <- all_counties |>
  filter(!GEOID %in% frostbelt_counties$GEOID)

city_points <- city_data |>
  st_centroid() |>
  select(NAME) |>
  st_join(frostbelt_counties) |>
  filter(!is.na(GEOID))

migration <- here("county-to-county-migration.csv") |>
  read_csv(col_types = c("c", "c", "c", "c", "n")) |>
  mutate(b_fips = paste0(substr(state_fips_B, 2,3), county_fips_B),
         GEOID = paste0(substr(state_fips_A, 2,3), county_fips_A)) |>
  filter((b_fips %in% out_of_frostbelt$GEOID) & (GEOID %in% city_points$GEOID)) |>
  group_by(GEOID) |>
  summarize(in_from_out_of_frost = sum(B_to_A_migration))

city_points <- city_points |>
  left_join(migration) |>
  rename(co_GEOID = GEOID) |>
  st_drop_geometry()

city_data_mig <- city_data |>
  inner_join(city_points)

st_write(city_data_mig, here("data-for-classes.geojson"))
