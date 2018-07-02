library(tigris)
Map <- counties()
names(Map@data) <- names(Map@data) %>% tolower()

library(dplyr)
Coords <- transmute(Map@data,
                    lon = as.numeric(intptlon),
                    lat = as.numeric(intptlat))

library(sp)

Map <- SpatialPointsDataFrame(Coords, data = Map@data)

library(censusapi)

Acs <- getCensus("acs/acs5", 2016, vars = c("B01001_001E", "B19113_001E"),
          region = "county:*") %>%
  rename(statefp = state, countyfp = county)

MapT <- Map
MapT@data <- left_join(MapT@data, Acs, by = c('statefp', 'countyfp'))

MapT@data <- mutate(MapT@data, radius = sqrt(B01001_001E / pi))

saveRDS(MapT, "map.rda")




