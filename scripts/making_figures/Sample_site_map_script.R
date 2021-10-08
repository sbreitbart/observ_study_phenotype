
#=====================
# Make sample site map
#=====================

# load libraries
library(ggmap)
library(cowplot)
library(ggsn)

#---------------------
# Import data
#---------------------

sites_all <- read.csv(here("./raw_data/80_Sites.csv"), sep = ",", header = TRUE)
sites <- sites_all[,2:6]
sites <- sites[c(1:3, 5,10,14,17,21:93),]

# Import urb_index values for each of these rows
urb_scores <- read.csv(here::here("./Figures_Tables/UrbanizationScore/Urbanization_Scores_Table.csv"),
                       header=T, na.strings=c("","NA")) %>%
  dplyr::select(2,8)

sites <- dplyr::inner_join(urb_scores, sites, by = "Patch_ID") 

sites$Transect <- factor(sites$Transect , levels = c("North", "South", "Rural"))



#---------------------
# Make Stamen map
#---------------------
mylocation <- c(-80.19, 43.2, -79.2, 43.81)
base_map <- ggmap(get_map(location = mylocation, maptype = 'terrain', source = 'stamen'))
cols1 <- c("North" = "#FDE725FF", "South" = "#21908CFF", "Rural" = "#660099")

map1 <- base_map +
  geom_point(data = sites, mapping = aes(x = lon, y = lat, fill=Transect, shape=Transect),
             color = "black", size=3) +
  labs(x = 'Longitude', y = 'Latitude') +
  scalebar(x.min = -80.19, x.max = -79.25,
           y.min = 43.25, y.max = 43.8,
           dist = 10, dist_unit = "km",
           st.bottom = FALSE, st.color = "black",
           transform = TRUE, model = 'WGS84',
           st.dist=.03) +
  theme(legend.position = c(0.75, 0.35),
        legend.background = element_rect(fill = "white",colour = "black"),
        legend.box.margin = margin(6, 6, 6, 6),
        legend.text = element_text(size=12),
        legend.title = element_text(size = 14),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14)) +
  scale_fill_manual(values = cols1,
                    breaks = c("North", "South", "Rural"),
                    labels = c("Urban: Non-Corridor", "Urban: Corridor", "Rural"),
                    name = "Sample Site Subtransect") +
  scale_shape_manual(values = c(23,24,21),
                     breaks = c("North", "South", "Rural"),
                     labels = c("Urban: Non-Corridor", "Urban: Corridor", "Rural"),
                     name = "Sample Site Subtransect") 


north2(map1, x = .59, y = .2, scale = 0.11, symbol = 1)
dev.copy2pdf(file="~/R_Projects/chapter_one/Figures_Tables/SampleSiteMaps/SampleSiteMap_terrain_color.pdf",
             width = 7, height = 5)


# add climate graph insets
# precip_bar <- system.file("extdata", "logo.png", package = "cowplot")
# 
# map1_legendmoved <- base_map +
#   geom_point(data = sites, mapping = aes(x = lon, y = lat, fill=Transect, shape=Transect), color = "black", size=3) +
#   labs(x = 'Longitude', y = 'Latitude') +
#   scalebar(x.min = -80.19, x.max = -79.25,
#            y.min = 43.25, y.max = 43.8,
#            dist = 10, dist_unit = "km",
#            st.bottom = FALSE, st.color = "black",
#            transform = TRUE, model = 'WGS84',
#            st.dist=.03) +
#   theme(legend.position = c(0.8, 0.3),
#         legend.background = element_rect(fill = "white",colour = "black"),
#         legend.box.margin = margin(6, 6, 6, 6),
#         legend.text = element_text(size=12),
#         legend.title = element_text(size = 14),
#         axis.text=element_text(size=12),
#         axis.title=element_text(size=14)) +
#   scale_fill_manual(values = cols1,
#                     breaks = c("North", "South", "Rural"),
#                     labels = c("Urban: Non-Corridor", "Urban: Corridor", "Rural"),
#                     name = "Sample Site Subtransect") +
#   scale_shape_manual(values = c(23,24,21),
#                      breaks = c("North", "South", "Rural"),
#                      labels = c("Urban: Non-Corridor", "Urban: Corridor", "Rural"),
#                      name = "Sample Site Subtransect") 
# 
# ggdraw(map1) +
#   draw_image(here::here("./Figures_Tables/Meteorology/Monthly_precip_bargraph.png"),
#              x = .5, y = 1,
#              hjust = 1, vjust = 1,
#              width = 0.25, height = 0.25)
# north2(map1, x = .59, y = .2, scale = 0.11, symbol = 1)


map1_urbscore <- base_map +
  geom_point(data = sites,
             mapping = aes(x = lon, y = lat, shape=Transect,
                           fill=Urb_score),
             color = "black",
             size = 3) +
  labs(x = 'Longitude', y = 'Latitude') +
  scalebar(x.min = -80.19, x.max = -79.25,
           y.min = 43.25, y.max = 43.8,
           dist = 10, dist_unit = "km",
           st.bottom = FALSE, st.color = "black",
           transform = TRUE, model = 'WGS84',
           st.dist=.03) +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill = "white"),
        legend.box.margin = margin(6, 6, 6, 6),
        legend.text = element_text(size=12),
        legend.title = element_text(size = 14),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.title.align=0.5,
        legend.box = "vertical")+
  scale_shape_manual(values = c(23,24,21),
                     breaks = c("North", "South", "Rural"),
                     labels = c("Urban: Non-Corridor", "Urban: Corridor", "Rural"),
                     name = "Sample Site Subtransect") +
  scale_fill_gradient(low = "white", high = "dark red",
                      breaks=c(-3.5, 0, 3.5),
                      labels=c("Rural",0, "Urban"),
                      limits=c(-3.6, 3.6),
                      name = "Urbanization\nScore"
                      ) 


north2(map1_urbscore, x = .6, y = .37, scale = 0.11, symbol = 1)

dev.copy2pdf(file="~/R_Projects/chapter_one/Figures_Tables/SampleSiteMaps/SampleSiteMap_terrain_color_urbscore.pdf",
             width = 8, height = 7)






#---------------------
# Google satellite map
#---------------------
base_map2 <- ggmap(get_map(location = mylocation, maptype = 'satellite', source = 'google'))
cols2 <- c("North" = "#E69F00", "South" = "#F0E442", "Rural" = "#D55E00")

map1_satellite <- get_googlemap(center = c(lon = -79.74256, lat = 43.5421), zoom = 10, maptype = 'satellite') %>% ggmap() +
  geom_point(data = sites, mapping = aes(x = lon, y = lat, fill=Transect, shape=Transect), color = "black", size=3) +
  labs(x = 'Longitude', y = 'Latitude') +
  scalebar(x.min = -80.19, x.max = -79.35,
           y.min = 43.42, y.max = 43.75,
           dist = 10, dist_unit = "km",
           st.bottom = FALSE, st.color = "black",
           transform = TRUE, model = 'WGS84',
           st.dist=.03) +
  theme(legend.position = c(0.8, 0.15),
        legend.background = element_rect(fill = "white",colour = "black"),
        legend.box.margin = margin(6, 6, 6, 6),
        legend.text = element_text(size=12),
        legend.title = element_text(size = 14),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14)) +
  scale_fill_manual(values = cols2,
                    breaks = c("North", "South", "Rural"),
                    labels = c("Urban: Non-Corridor", "Urban: Corridor", "Rural"),
                    name = "Sample Site Subtransect") +
  scale_shape_manual(values = c(23,24,21),
                     breaks = c("North", "South", "Rural"),
                     labels = c("Urban: Non-Corridor", "Urban: Corridor", "Rural"),
                     name = "Sample Site Subtransect") 

north2(map1_satellite, x = .7, y = .5, scale = 0.11, symbol = 1)
dev.copy2pdf(file="~/R_Projects/Chapter1/Figures_Tables/SampleSiteMaps/SampleSiteMap_satellite_color.pdf", width = 10, height = 7)

##### moving the scale bar & new colors #####
# map1_satellite <- get_googlemap(center = c(lon = -79.74256, lat = 43.5421), zoom = 10, maptype = 'satellite') %>% ggmap() +
#   geom_point(data = sites, mapping = aes(x = lon, y = lat, fill=Transect, shape=Transect), color = "black", size=3) +
#   labs(x = 'Longitude', y = 'Latitude') +
#    scalebar(x.min = -80.19, x.max = -79.4,
#             y.min = 43.25, y.max = 43.8,
#             dist = 10, dist_unit = "km",
#             st.bottom = FALSE, st.color = "black",
#             transform = TRUE, model = 'WGS84',
#             st.dist=.03) +
#    theme(legend.position = c(0.8, 0.3),
#          legend.background = element_rect(fill = "white",colour = "black"),
#          legend.box.margin = margin(6, 6, 6, 6),
#          legend.text = element_text(size=12),
#          legend.title = element_text(size = 14),
#          axis.text=element_text(size=12),
#          axis.title=element_text(size=14)) +
#    scale_fill_manual(values = cols1,
#                      breaks = c("North", "South", "Rural"),
#                        labels = c("Urban: Non-Corridor", "Urban: Corridor", "Rural"),
#                        name = "Sample Site Subtransect") +
#    scale_shape_manual(values = c(23,24,21),
#                       breaks = c("North", "South", "Rural"),
#                        labels = c("Urban: Non-Corridor", "Urban: Corridor", "Rural"),
#                        name = "Sample Site Subtransect") 


#---------------------
# Black and white map
#---------------------
cols <- c("North" = "gray72", "South" = "gray33", "Rural" = "white")

map2 <- base_map +
  scale_fill_manual(values = cols) +
  scale_shape_manual(values = c(23,24,21)) +
  geom_point(data = sites, mapping = aes(x = lon, y = lat, shape=Transect, fill = Transect), color = "black", size=3) +
  labs(x = 'Longitude', y = 'Latitude') +
  scalebar(x.min = -80.19, x.max = -79.25,
           y.min = 43.25, y.max = 43.8,
           dist = 10, dist_unit = "km",
           st.bottom = FALSE, st.color = "black",
           transform = TRUE, model = 'WGS84',
           st.dist=.03) +
  theme(legend.position = c(0.9, 0.3),
        legend.background = element_rect(fill = "white",colour = "black"),
        legend.box.margin = margin(6, 6, 6, 6)) 


north2(map2, x = .7, y = .3, scale = 0.11, symbol = 1)



#---------------------
# Inset of Ontario
#---------------------

# logo_file <- system.file("extdata", "logo.png", package = "cowplot")
# 
# ggdraw(p) + 
#   draw_image(logo_file, x = 1, y = 1, hjust = 1, vjust = 1, width = 0.13, height = 0.2)



#---------------------
# GTA Land use/land cover
#---------------------
# Tried obtaining Toronto's LULC dataset directly from the website but am having issues, so I downloaded it. (see code below) ###########

# install.packages("opendatatoronto")
# library(opendatatoronto)
# library(dplyr)
# 
# # get package
# package <- show_package("61642048-56bb-4050-b7c3-f569fcf94527")
# package
# 
# lulc_gta <- list_package_resources(package$id)
# lulc_gta_shp <- get_resource(lulc_gta[3,])

# 
# library(rgdal)
# my_spdf <- readOGR( 
#   dsn= paste0(getwd(),"~/R_Projects/TorontoOpenData_ForestandLandCover2018_20200916/cotlandcover/") , 
#   layer="cotLandcover",
#   verbose=FALSE
# )
# 
# library(raster)
# s <- shapefile("~/R_Projects/TorontoOpenData_ForestandLandCover2018_20200916/cotlandcover/cotLandcover.shp")
# 
# 
# # Basic plot of shapefile:
# par(mar=c(0,0,0,0))
# plot(s, col="#f2f2f2", bg="skyblue", lwd=0.25, border=0 )
# 
# install.packages("rmapshaper")
# library(rmapshaper)
# shape_simp <- rmapshaper::ms_simplify((raster::shapefile("~/R_Projects/TorontoOpenData_ForestandLandCover2018_20200916/cotlandcover/cotLandcover.shp")), keep = 0.005)