library(here)
library(tidyverse)

data <- here("data-for-classes.geojson") |>
  st_read() |>
  mutate(migration_pct = in_from_out_of_frost / populationE) 

no_name_scale_data <- data |>
  st_drop_geometry() |>
  select(housing, unemp, avg_access_ratio, 
         plan_pts, populationE, migration_pct) |>
  scale()

set.seed(3775668)

kmeans_3 <- kmeans(no_name_scale_data, centers = 3)
kmeans_4 <- kmeans(no_name_scale_data, centers = 4)
kmeans_5 <- kmeans(no_name_scale_data, centers = 5)
kmeans_6 <- kmeans(no_name_scale_data, centers = 6)
kmeans_7 <- kmeans(no_name_scale_data, centers = 7)
kmeans_8 <- kmeans(no_name_scale_data, centers = 8)

table(kmeans_3$cluster)
table(kmeans_4$cluster)
table(kmeans_5$cluster)
table(kmeans_6$cluster)
table(kmeans_7$cluster)
table(kmeans_8$cluster)

# five clusters chosen to have the smallest number of clusters with no more than
# half of cities in a single cluster

data$cluster <- as_factor(kmeans_5$cluster)

st_write(data, "final_fig_data.geojson")
