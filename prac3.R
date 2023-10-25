library(sf)
library(here)
st_layers(here("gadm36_AUS_gpkg", "gadm36_AUS.gpkg"))
Ausoutline <- st_read(here("gadm36_AUS_gpkg", "gadm36_AUS.gpkg"), 
                      layer='gadm36_AUS_0')

print(Ausoutline)


st_crs(Ausoutline)$proj4string

Ausoutline <- Ausoutline %>%
  st_set_crs(., 4326)

Ausoutline <- st_read(here("gadm36_AUS_gpkg", "gadm36_AUS.gpkg"), 
                      layer='gadm36_AUS_0') %>% 
  st_set_crs(4326)

AusoutlinePROJECTED <- Ausoutline %>%
  st_transform(.,3112)

print(AusoutlinePROJECTED)

#From sf to sp
AusoutlineSP <- Ausoutline %>%
  as(., "Spatial")

#From sp to sf
AusoutlineSF <- AusoutlineSP %>%
  st_as_sf()

library(raster)
library(terra)
jan<-terra::rast(here("wc2.1_5m_tavg", "wc2.1_5m_tavg_01.tif"))
# have a look at the raster layer jan
jan

plot(jan)

pr1 <- terra::project(jan, "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

#or....

newproj<-"ESRI:54009"
# get the jan raster and give it the new proj4
pr1 <- jan %>%
  terra::project(., newproj)
plot(pr1)

library(fs)
dir_info("wc2.1_5m_tavg/") 




