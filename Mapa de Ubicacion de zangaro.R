library(raster)
library(sf)
library(ggplot2)
library(dplyr)
library(forcats)
library(units)

Bolivia           <- getData('GADM', country='Bolivia', level=0) %>% st_as_sf()
Brazil             <- getData('GADM', country='Brazil', level=0) %>% st_as_sf()
Chile              <- getData('GADM', country='Chile', level=0) %>% st_as_sf()
Ecuador            <- getData('GADM', country='Ecuador', level=0) %>% st_as_sf()
Peru               <- getData('GADM', country='Peru', level=3) %>% st_as_sf()
Per               <- getData('GADM', country='Peru', level=1) %>% st_as_sf()
Per_pr            <- getData('GADM', country='Peru', level=2) %>% st_as_sf()
Puno             <- subset(Peru, NAME_1  == "Puno")
Puno_pr           <- subset(Per_pr , NAME_1  == "Puno")
Azangaro            <- subset(Puno, NAME_2  == "Azángaro")
Azangaro_xy <- cbind(Azangaro, st_coordinates(st_centroid(Azangaro$geometry)))
Puno_pr_xy <- cbind(Puno_pr, st_coordinates(st_centroid(Puno_pr$geometry)))
write_sf(Azangaro, "SHP/Azangaro.shp")

Azangaro$Area<- st_area(Azangaro) / 1000000

Azangaro_data <- Azangaro %>%
  mutate(NAME_3= fct_reorder(NAME_3, Area, .desc = TRUE))

Macro= ggplot() +
  geom_sf(data=Per, color="white", fill="gray80", size=0.5)+
  geom_sf(data=Bolivia, fill=NA, color="black", size=0.5)+
  geom_sf(data=Brazil, fill=NA, color="black", size=0.5)+
  geom_sf(data=Chile, fill=NA, color="black", size=0.5)+
  geom_sf(data=Ecuador, fill=NA, color="black", size=0.5)+
  geom_sf(data = Azangaro , fill="red", color="red") + 
  coord_sf(xlim = c(-81.3307, -67), ylim = c(-18.3518 ,-0.03747),expand = FALSE)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.4))+
  annotate(geom = "text", x = -80, y = -1, hjust = 0, vjust = 1, 
           label = "Ecuador",size = 3, family="serif", color = "black",  fontface="italic")+
  annotate(geom = "text", x = -68, y = -15, hjust = 0, vjust = 1, angle = 90,
           label = "Bolivia",size = 3, family="serif", color = "black",  fontface="italic")+
  annotate(geom = "text", x = -70, y = -8, hjust = 0, vjust = 1, 
           label = "Brasil",size = 3, family="serif", color = "black",  fontface="italic")+
  annotate(geom = "text", x = -74, y = -1, hjust = 0, vjust = 1, 
           label = "Colombia",size = 3, family="serif", color = "black",  fontface="italic")
Macro.grob  <- ggplotGrob(Macro)

Graf = ggplot(Azangaro_data,aes(x = Area, y =NAME_3)) +
  geom_col(fill="#55a630") +
  scale_y_discrete(limits = rev)+
  labs(x = "Area en km²",y = "Distritos de Azángaro")+
  theme_bw()+
  theme(axis.text.x  = element_text( color="black", size=7, family="serif"),
        axis.text.y  = element_text(color="black", size=7,family="serif"),
        axis.title.x = element_text(color="black", size=8,family="serif"),
        axis.title.y = element_text(color="black", size=8,family="serif"))

Graf.grob  <- ggplotGrob(Graf)

Mapa =ggplot() +
  geom_sf(data=Puno_pr, color="white", fill="gray90", size=0.5)+
  geom_sf(data=Azangaro, color="white", fill="gray80", size=0.5)+
  geom_sf_text(data = Azangaro_xy , aes(x= X, y=Y, label = NAME_3), size =2.5, color="black"
               ,fontfamily = "serif",  fontface="italic")+
  geom_sf_label(data = Puno_pr_xy , aes(x= X, y=Y, label = NAME_2), size = 3, color="black", alpha=0.4,
               fontfamily = "serif", fontface="italic")+
  coord_sf(xlim = c(-70.7, -69.3), ylim = c(-15.5 ,-14.2),expand = FALSE)+
  annotation_custom(Macro.grob, xmin = -69.6, xmax = -69.3, ymin =-14.6, ymax=-14.2)+
  annotation_custom(Graf.grob , xmin = -69.8, xmax = -69.3, ymin =-15.40264, ymax=-15)+
  theme_bw()+
  theme(panel.grid.major = element_line(color = gray(.5),
                                        linetype = "dashed", size = 0.5),
        axis.text.x  = element_text(face="bold", color="black", size=8),
        axis.text.y  = element_text(angle = 90,face="bold", color="black", size=8),
        legend.background = element_blank(),
        legend.text =element_text(size=11, family="serif"),
        legend.title = element_text(size=11, family="serif"),
        legend.key.size = unit(0.4, "cm"), 
        legend.key.width = unit(0.9,"cm"),
        plot.title = element_text(size = 16, hjust = 0.5, color = "#4e4d47", family="serif", face = "italic"),
        plot.subtitle = element_text(size = 11, hjust = 0.8, face = "italic", color = "#4e4d47", family="serif"),
        plot.caption = element_text(size = 10, hjust = 0.95, color = "#4e4d47", family="serif", face = "italic"))+
  scale_x_continuous(name=expression(paste("Longitude (",degree,")"))) +
  scale_y_continuous(name=expression(paste("Latitude (",degree,")"))) +
  ggtitle("")+
  labs(subtitle="", 
       caption="Fuente: Geographic Data Analysis and Modeling")
  


ggsave(plot = Mapa  ,"Mapa/Mapa_Azangaro.png", units = "cm", 
       width = 21,height = 25, dpi = 1200)
