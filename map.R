library(ggplot2)
library(sf)
library(tmap)
library(ggspatial)


# Load shapefile
cities <- st_read("PAK_adm2.shp")
district <- st_read("PAK_adm3.shp")
x1 <- st_read("gis_osm_places_free_1.shp")
x <- st_read("gis_osm_places_a_free_1.shp")
zz <- st_read("pak_admbnda_adm3_wfp_20220909.shp")
z <- st_read("pak_admbndp_admALL_wfp_itos_20220909.shp")


# View the data structure
print(cities)
names(cities)

# Filter a specific city (e.g., Karachi)
karachi <- cities[cities$NAME_2 == "Karachi", ]
sindh <- district[district$NAME_1 == "Sind",]
karachi2 <- district[district$NAME_2 == "Karachi",]

zz_sindh <- zz[zz$ADM1_EN == "Sindh",]
Khi <- zz_sindh[zz_sindh$ADM2_EN %in% c("Jamshoro","Thatta","West Karachi","East Karachi","South Karachi","Central Karachi","Korangi Karachi","Malir Karachi"),]
khi2 <- Khi[!(Khi$ADM3_EN %in% c("Thatta","Keti Bunder","Ghorabari","Kotri","Manjhand","Sehwan Sharif")),]

P_khi <-ggplot() +
  geom_sf(data = khi2, fill = "gray85", color = "black")+
  annotation_scale(location = "bl", width_hint = 0.25) +
  theme(panel.background = element_rect(fill = "white"),axis.ticks=element_blank(),axis.text=element_blank())
P_khi
P_khi+geom_point(data = all_data, aes(x = lon, y = lat, size = sample_size), color = "red") 



#plot the map
P_city <-ggplot() +
  geom_sf(data = cities, fill = "gray85", color = "black")+
  geom_sf(data = karachi, fill = "blue", color = "black") +
  annotation_scale(location = "bl", width_hint = 0.25) +
  theme(panel.background = element_rect(fill = "white"),axis.ticks=element_blank(),axis.text=element_blank())
P_city

P_district <-ggplot() +
  geom_sf(data = sindh, fill = "gray85", color = "black")+
  geom_sf(data = karachi2, fill = "lightgreen", color = "black") +
  annotation_scale(location = "bl", width_hint = 0.25) +
  theme(panel.background = element_rect(fill = "white"),axis.ticks=element_blank(),axis.text=element_blank())
P_district

P_sindh <-ggplot() +
  geom_sf(data = zz_sindh, fill = "gray85", color = "black")+
  geom_sf_text(data = zz_sindh, aes(label = ADM3_EN), size = 3, color = "blue") +  # Add city names
  annotation_scale(location = "bl", width_hint = 0.25) +
  theme(panel.background = element_rect(fill = "white"),axis.ticks=element_blank(),axis.text=element_blank())
P_sindh




# Load road shapefile
roads <- st_read("PAK_roads.shp")

# View the first few rows of the shapefile
head(roads)

#Query Roads from OpenStreetMap
library(osmdata)

# Query road data for Karachi
karachi_roads <- opq("Karachi") %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()

# Extract road polygons
roads <- karachi_roads$osm_lines
roads1 <- roads[!is.na(roads$name), ]

P_city_road <-ggplot() +
  geom_sf(data = karachi, fill = "blue", color = "black") +
  annotation_scale(location = "bl", width_hint = 0.25) +
  geom_sf(data = roads1, color = "black", size = 0.3) +
  theme(panel.background = element_rect(fill = "white"),axis.ticks=element_blank(),axis.text=element_blank())
P_city_road
