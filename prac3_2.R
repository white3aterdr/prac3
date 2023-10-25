library(sf)
library(here)
st_layers(here("gadm36_AUS_gpkg", "gadm36_AUS.gpkg"))
library(sf)
Ausoutline <- st_read(here("gadm36_AUS_gpkg", "gadm36_AUS.gpkg"), 
                      layer='gadm36_AUS_0')

print(Ausoutline)
library(sf)
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


# set the proj 4 to a new object

pr1 <- terra::project(jan, "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

#or....

newproj<-"ESRI:54009"
# get the jan raster and give it the new proj4
pr1 <- jan %>%
  terra::project(., newproj)
plot(pr1)

pr1 <- pr1 %>%
  terra::project(., "EPSG:4326")
plot(pr1)

library(fs)
dir_info("wc2.1_5m_tavg") 

library(tidyverse)
listfiles<-dir_info("wc2.1_5m_tavg") %>%
  filter(str_detect(path, ".tif")) %>%
  dplyr::select(path)%>%
  pull()

#have a look at the file names 
listfiles


worldclimtemp <- listfiles %>%
  terra::rast()

#have a look at the raster stack
worldclimtemp
worldclimtemp[[2]]

month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

names(worldclimtemp) <- month
names(worldclimtemp)
?names
plot(worldclimtemp$Aug)


site <- c("Brisbane", "Melbourne", "Perth", "Sydney", "Broome", "Darwin", "Orange", 
          "Bunbury", "Cairns", "Adelaide", "Gold Coast", "Canberra", "Newcastle", 
          "Wollongong", "Logan City" )
lon <- c(153.03, 144.96, 115.86, 151.21, 122.23, 130.84, 149.10, 115.64, 145.77, 
         138.6, 153.43, 149.13, 151.78, 150.89, 153.12)
lat <- c(-27.47, -37.91, -31.95, -33.87, 17.96, -12.46, -33.28, -33.33, -16.92, 
         -34.93, -28, -35.28, -32.93, -34.42, -27.64)
#Put all of this inforamtion into one list 
samples <- data.frame(site, lon, lat, row.names="site")
# Extract the data from the Rasterstack for all points 
AUcitytemp<- terra::extract(worldclimtemp, samples)

Aucitytemp2 <- AUcitytemp %>% 
  as_tibble()%>% 
  add_column(Site = site, .before = "Jan")

Perthtemp <- Aucitytemp2 %>%
  filter(site=="Perth")

hist(as.numeric(Perthtemp))

library(tidyverse)
#define where you want the breaks in the historgram
userbreak<-c(8,10,12,14,16,18,20,22,24,26)
#userbreak<-c(9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26)

# remove the ID and site columns
Perthtemp <- Aucitytemp2 %>%
  filter(site=="Perth")

t<-Perthtemp %>%
  dplyr::select(Jan:Dec)


hist((as.numeric(t)), 
     breaks=userbreak, 
     col="red", 
     main="Histogram of Perth Temperature", 
     xlab="Temperature", 
     ylab="Frequency")

histinfo <- as.numeric(t) %>%
  as.numeric()%>%
  hist(.)
histinfo


plot(Ausoutline$geom)


AusoutSIMPLE <- Ausoutline %>%
  st_simplify(., dTolerance = 1000) %>%
  st_geometry()%>%
  plot()

print(Ausoutline)


#this works nicely for rasters
crs(worldclimtemp)

Austemp <- Ausoutline %>%
  # now crop our temp data to the extent
  terra::crop(worldclimtemp,.)

# plot the output
plot(Austemp)
exactAus<-terra::mask(Austemp, Ausoutline)
plot(exactAus)

#subset using the known location of the raster
hist(exactAus[[3]], col="red", main ="March temperature")

exactAusdf <- exactAus %>%
  as.data.frame()

library(ggplot2)
# set up the basic histogram
gghist <- ggplot(exactAusdf, 
                 aes(x=Mar)) + 
  geom_histogram(color="black", 
                 fill="white")+
  labs(title="Ggplot2 histogram of Australian March temperatures", 
       x="Temperature", 
       y="Frequency")
# add a vertical line to the hisogram showing mean tempearture

gghist + geom_vline(aes(xintercept=mean(Mar, 
                                        na.rm=TRUE)),
                    color="blue", 
                    linetype="dashed", 
                    size=1)+
  theme(plot.title = element_text(hjust = 0.5))

squishdata<-exactAusdf%>%
  pivot_longer(
    cols = 1:12,
    names_to = "Month",
    values_to = "Temp"
  )

twomonths <- squishdata %>%
  # | = OR
  filter(., Month=="Jan" | Month=="Jun")


meantwomonths <- twomonths %>%
  group_by(Month) %>%
  summarise(mean=mean(Temp, na.rm=TRUE))

meantwomonths

ggplot(twomonths, aes(x=Temp, color=Month, fill=Month)) +
  geom_histogram(position="identity", alpha=0.5)+
  geom_vline(data=meantwomonths, 
             aes(xintercept=mean, 
                 color=Month),
             linetype="dashed")+
  labs(title="Ggplot2 histogram of Australian Jan and Jun
       temperatures",
       x="Temperature",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

print(123)

sss