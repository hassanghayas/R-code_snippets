library(ggplot2)
library(sf)
library(tmap)
library(ggspatial)

# download shape file
# https://data.humdata.org/dataset/cod-ab-pak

# Load shapefile
Pak.shp.lv1 <- st_read("pak_admbnda_adm0_wfp_20220909.shp") # country
Pak.shp.lv2 <- st_read("pak_admbnda_adm1_wfp_20220909.shp") # states
Pak.shp.lv3 <- st_read("pak_admbnda_adm2_wfp_20220909.shp") # district
Pak.shp.lv4 <- st_read("pak_admbnda_adm3_wfp_20220909.shp") # town

# Pak map, state level
Pak_map.L2 <- ggplot() +
  geom_sf(data = Pak.shp.lv2, fill = "gray85", color = "black")+
  geom_sf_text(data = Pak.shp.lv2, aes(label = ADM1_EN), size = 2, color = "black")+
  annotation_scale(location = "bl", width_hint = 0.25) +
  theme(panel.grid = element_line(color = "black",linewidth = 0.2),
        panel.background = element_rect(fill = "white", color="black"),
        axis.title = element_blank())

# Pak_map.L2 <- ggplot() +
#   geom_sf(data = Pak.shp.lv2, fill = "gray85", color = "black")+
#   geom_sf_text(data = Pak.shp.lv2, aes(label = ADM1_EN), size = 2, color = "black")+
#   annotation_scale(location = "bl", width_hint = 0.25) +
#   theme(panel.background = element_rect(fill = "white"),axis.ticks=element_blank(),axis.text=element_blank())

Pak_map.L2

# filter sindh and Karachi data at district level
sindh.L3 <- Pak.shp.lv3[Pak.shp.lv3$ADM1_EN == "Sindh",]
khi.L3 <- Pak.shp.lv3[Pak.shp.lv3$ADM2_EN %in% c("West Karachi",
                                                 "East Karachi",
                                                 "South Karachi",
                                                 "Central Karachi",
                                                 "Korangi Karachi",
                                                 "Malir Karachi"),]
# sindh map with Karachi highlighted
sindh_map.L3 <- ggplot() +
  geom_sf(data = sindh.L3, fill = "gray85", color = "black")+
  geom_sf(data = khi.L3, fill = "lightgreen", color = "black")+
  annotate("text",x=67.2,y=25, label="karachi", size=2.5)+
  annotation_scale(location = "bl", width_hint = 0.25) +
  theme(panel.grid = element_blank(),panel.background = element_rect(fill = "white", color="black"))
sindh_map.L3

# filter karachi data at town level
khi.L4 <- Pak.shp.lv4[Pak.shp.lv4$ADM2_EN %in% c("West Karachi",
                                                 "East Karachi",
                                                 "South Karachi",
                                                 "Central Karachi",
                                                 "Korangi Karachi",
                                                 "Malir Karachi"),]

# Karachi map at town level
khi_map.L4 <- ggplot() +
  geom_sf(data = khi.L4, fill = "grey85", color = "black")+
  annotate("text",x=67.2,y=25, label="karachi", size=3.5)+
  annotation_scale(location = "bl", width_hint = 0.25) +
  theme(panel.grid = element_blank(),panel.background = element_rect(fill = "white", color="black"))
khi_map.L4

Pak_map.L2 +sindh_map.L3 +khi_map.L4

ggsave("map.svg",dpi = 1200, width = 9, height = 4.7)
