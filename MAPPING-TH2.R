library(raster)
library(maptools)
library(plyr)
library(ggplot2)
library(rgdal)
library(ggmap)
library(scales)
library(extrafont)
library(ggthemes)
library(ggrepel)
library(nlme)
library(tidyverse)

loadfonts(device = "win", quiet = T)

## Get the data and wrangling

RSA2 <- raster::getData('GADM', country="ZA", level=2) 

summary(RSA2)
RSA2 <- getData("GADM", country="ZA", level=2)

points2 <- data.frame(long = c(28.983, 29.724720, 28.150, 28.583333, 27.912),  # specific towns I was looking at 
                      lat = c(-30.917, -30.761669, -32.333, -32.166667, -33.015), 
                      name = c("Mount Frere", "Mbizana", "Butterworth", "Ingquza Hill", "Nxarhuni village"))

SA_EC_Rural <- data.frame(name = c("Mount frere", "Mbizana", "Butterworth", "Ingquza Hill", "Nxarhuni village"),
                          
                          lat = c(-30.917, -30.761669,-32.333, -32.166667,  -33.015),
                          
                          long = c(28.983, 29.724720, 28.150, 28.583333, 27.912))


plot(RSA2)

RSA2_UTM <-spTransform(RSA2, CRS("+init=EPSG:32735"))  # chose not to project the map because I was having trouble adding my labels

RSA2@data$NAME_1

easterncape <- RSA2[RSA2@data$NAME_1 == "Eastern Cape",]
easterncape_df<-fortify(easterncape)

Gauteng <- RSA2[RSA2@data$NAME_1 == "Gauteng",]
Gauteng_df<-fortify(Gauteng)

kzn <- RSA2[RSA2@data$NAME_1 == "KwaZulu-Natal", ]
kzn_df <- fortify(kzn)

Western_Cape<- RSA2[RSA2@data$NAME_1 == "Western Cape", ]
Western_Cape_df <- fortify(Western_Cape)

Northern_Cape<- RSA2[RSA2@data$NAME_1 == "Northern Cape", ]
Northern_Cape_df <- fortify(Northern_Cape)

North_West<- RSA2[RSA2@data$NAME_1 == "North West", ]
North_West_df <- fortify(North_West)

Limpopo<- RSA2[RSA2@data$NAME_1 == "Limpopo", ]
Limpopo_df <- fortify(Limpopo)

Free_State<- RSA2[RSA2@data$NAME_1 == "Free State", ]
Free_State_df <- fortify(Free_State)

Mpumalanga<- RSA2[RSA2@data$NAME_1 == "Mpumalanga", ]
Mpumalanga_df <- fortify(Mpumalanga)

# set my theme

theme_opts<-list(theme(panel.grid.minor = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.background = element_blank(),
                       plot.background = element_blank(),
                       axis.line = element_blank(),
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks = element_blank(),
                       axis.title.x = element_blank(),
                       axis.title.y = element_blank(),
                       plot.title = element_text(hjust = 0.5),
                       text = element_text(family = "Helvetica"),
                       plot.caption = element_text(family = "Helvetica", size = 12, vjust = 0.5),
                       plot.caption.position = "plot"))

## GGPLOT

#pdf("map_RSA_final.pdf", width = 9, height = 7)
ggplot() + 
  geom_polygon(data = RSA2, aes(long,lat,group=group), 
               fill="whitesmoke", colour = "#1d1d1b")+
  geom_path(data = RSA2, aes(long,lat, group=group), 
            color="#1d1d1b", size=0.1) +
  geom_path(data = easterncape_df, aes(long, lat, group = group), 
            colour = "#495da7") +
  geom_polygon(data = easterncape_df, aes(long, lat, group = group), 
               fill = "#00965b")+
  geom_polygon(data = Gauteng_df, aes(long, lat, group = group), 
               fill = "#b8b335")+
  geom_polygon(data = kzn_df, aes(long, lat, group = group), 
               fill = "#781e52")+
  geom_polygon(data = Western_Cape_df, aes(long, lat, group = group), 
               fill = "#495da7")+
  geom_polygon(data = Northern_Cape_df, aes(long, lat, group = group), 
               fill = "#c35959")+
  geom_polygon(data = North_West_df, aes(long, lat, group = group), 
               fill = "#777141")+
  geom_polygon(data = Free_State_df, aes(long, lat, group = group), 
               fill = "burlywood4")+
  geom_polygon(data = Limpopo_df, aes(long, lat, group = group), 
               fill = "darkolivegreen3")+
  geom_polygon(data = Mpumalanga_df, aes(long, lat, group = group), 
               fill = "3a4971")+
  geom_point(data = points2, aes(x = long, y = lat), 
             size = 2, col = "red") +
  geom_point(data = SA_EC_Rural, aes(x = long, y = lat), 
             size = 0.5) +
  geom_label_repel(data = SA_EC_Rural, aes(long, lat, label = name), 
                   size = 4, label.size = 0.3, label.padding = 0.3, segment.size = 0.5, segment.color = "black", force = 2) +
  theme(aspect.ratio = 1)+
  labs(caption = "Source: www.gadm.org, June 2020
                  Graphic: Sixolile Ngqwala at Good Governance Africa",
       title = "Rural areas affected within the municipality boundaries of South Africa") +
  theme_opts
#dev.off()

ggsave("map_of_RSA_erica.png", dpi = 600, width = 9, height = 7)



Sys.getenv("PATH")
Sys.which("raster.dll")





