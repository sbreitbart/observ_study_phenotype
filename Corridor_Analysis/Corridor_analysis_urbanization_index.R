# Load packages
library(dplyr)
library(ggplot2)
library("factoextra")
library(MASS)
library(patchwork)
library(ggpubr)
library(here)


# Read in data
pca_res <- read.csv(here::here("./Urbanization_Score_files/UrbIndex_2020/pca_result_clean.csv"),
                    header = T, na.strings=c("","NA"), fileEncoding="UTF-8-BOM")
distances <- read.csv(here::here("./raw_data/80_Sites.csv"),
                      header = T, na.strings=c("","NA"),fileEncoding="UTF-8-BOM")

# Clean up data
pca_res <- pca_res[,-1] %>% dplyr::rename(c(
  buildings_cells = Cells_where_building_label.2,
  vegetation_cells = Cells_where_vegetation_label.2,
  roads_cells = Cells_where_road_label.1,
  buildings_labels = Avg_building_labels,
  veg_labels = Avg_vegetation_labels,
  Urb_score = PCA_value,
  Pop_ID = Location_name)) 
  
## Join with distances df
scores <- full_join(pca_res, distances, by = "Pop_ID") %>% dplyr::select(.,-c(lat,lon)) %>%

## Consolidate 93 sites into 80 by taking means of pops w/more than one site associated with it.
# Ex. MW004 associated with >1 AS site.
group_by(Patch_ID) %>%
  dplyr::summarise(
    buildings_cells = mean(buildings_cells),
    vegetation_cells = mean(vegetation_cells),
    roads_cells = mean(roads_cells),
    buildings_labels = mean(buildings_labels),
    veg_labels = mean(veg_labels),
    Urb_score = mean(Urb_score),
    Pop_ID = first(Pop_ID),
    Patch_ID = first(Patch_ID),
    City_dist = mean(City_dist),
    Transect = first(Transect)
  )


# Export
write.csv(scores, here::here("./Urbanization_Score_files/Urbanization_Scores_Table.csv"))



# Analyze land use/land cover along 3 subtransects
## Gradient approach
scores_gradient <- ggplot(scores, aes(x = City_dist, y = Urb_score)) + 
  labs(x = "Distance to urban center (km)", y = "Urbanization score") +
  geom_point(aes(color=Transect, shape=Transect),  size=2) +
  scale_shape(solid = F)+
  geom_smooth(color = 'black', fill = 'black', size=0.75, method = lm, alpha=.15, se = T) +
  labs(title = paste(" Adj R2 = ",signif(summary(lm(Urb_score ~ City_dist, data = scores))$adj.r.squared, 5),
                     "\n",
                     "Intercept =",signif(lm(Urb_score ~ City_dist, data = scores)$coef[[1]],5 ),
                     "\n",
                     "Slope =",signif(lm(Urb_score ~ City_dist, data = scores)$coef[[2]], 5),
                     "\n",
                     "P =",signif(summary(lm(Urb_score ~ City_dist, data = scores))$coef[2,4], 5)))


## Split into subtransects
scores_subtransects <- ggplot(scores, aes(x = City_dist, y = Urb_score, color = Transect, shape=Transect)) + 
  labs(x = "Distance to urban center (km)", y = "Urbanization score") +
  geom_point(aes(color=Transect, shape=Transect),  size=2) +
  scale_shape(solid = F)+
  geom_smooth(aes(color=Transect, shape=Transect, fill=Transect),
              size=0.75, method = lm, alpha=.15, se = T) 

## Export 
both_scores <- scores_gradient + scores_subtransects
both_scores
dev.copy2pdf(file = here::here("./Figures_Tables/UrbanizationScores_Regressions.pdf"),
             width = 10, height = 5)

# Plot buildings vs vegetation
ggplot(scores, aes(x = veg_labels, y = buildings_labels)) + 
  labs(x = "Buildings", y = "Vegetation") +
  geom_point(aes(color=Transect, shape=Transect),  size=2) +
  scale_shape(solid = F)+
  geom_smooth(color = 'black', fill = 'black', size=0.75, method = lm, alpha=.15, se = T) 


# PCA
scores.active <- scores[,c(2:4)]
urbindex.pca <- prcomp(scores.active, scale = TRUE)
fviz_pca_ind(urbindex.pca,
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Subtransects",
             col.ind = scores$Transect)




# Check for normality
## Just city_dist
par(mfrow=c(2,2))
plot(lm(Urb_score ~ City_dist, data = scores))
summary(lm(Urb_score ~ City_dist, data = scores))
gg_reshist(lm(Urb_score ~ City_dist, data = scores), bins=20)
ggqqplot(scores$Urb_score)
## seems fine. Might be some heteroskedasticity (?) but other graphs look good...


## city_dist * transect
par(mfrow=c(2,2))
plot(lm(Urb_score ~ City_dist * Transect, data = (scores %>% filter(scores$Transect != 'Rural'))))
summary(lm(Urb_score ~ City_dist * Transect, data = (scores %>% filter(scores$Transect != 'Rural'))))
gg_reshist(lm(Urb_score ~ City_dist * Transect, data = (scores %>% filter(scores$Transect != 'Rural'))), bins=20)
ggqqplot(scores$Urb_score)
## seems fine.




# ANOVA
summary(lm(Urb_score ~ City_dist, data = scores))
scores_anova_q1 <- car::Anova(lm(Urb_score ~ City_dist, data = scores))
# Highly significant. Urbanization score DOES CHANGE along the gradient.
## Get R-sq
round((summary(lm(Urb_score ~ City_dist, data = scores)))$r.squared,3)



summary(lm(Urb_score ~ City_dist * Transect,
           data = (scores %>% filter(scores$Transect != 'Rural'))))
scores_anova_q3 <- car::Anova(lm(Urb_score ~ City_dist * Transect,
           data = (scores %>% filter(scores$Transect != 'Rural'))))
# Interaction and city_dist sig. Urbanization score does not differ much by subtransect
# (aka the means are similar), but it does still change along the subtransects, and
# there is an interaction, meaning that urbanization score depends both on subtransect
# and distance from city center.

## Get R-sq
round(summary(lm(Urb_score ~ City_dist * Transect,
                 data = (scores %>% filter(scores$Transect != 'Rural'))))$r.squared,3)

# Export to csv
sink('UrbScores_ANOVA.csv')

## table 1
cat('Urbanization Scores- Q1')
cat('\n')
cat('Urb_score ~ City_dist')
cat('\n')
cat('Main Effect')
write.csv(scores_anova_q1)
cat('\n')

## table 2
cat('Urbanization Scores- Q3')
cat('\n')
cat('Urb_score ~ City_dist * Transect')
cat('\n')
cat('Main Effect')
write.csv(scores_anova_q3)
cat('\n')
cat('\n')

##### Close the sink ####
sink()






### How many plants are within transects?

# For the southern urban (green corridor) subtransect, 
# these sites are within the green corridor: MW032, MW014,
#  MW013, MW003, MW004, MW069, MW050
# **MW013 isn't technically within the green corridor itself 
# but it's perpendicular and connected through a different
#  green corridor, so I'm counting it. It's ~100m from the
#   corridor.

# 29 southern urban sites total, 7 within the corridor itself
#  (<25%)


### Histograms of green corridor site proximities
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(dplyr) # A Grammar of Data Manipulation # A Grammar of Data Manipulation

corridor_data <- read.csv(here("./Corridor_Analysis/SouthernUrbanSites_ProximitytoCorridor.csv"),  header=T, na.strings=c("","NA"))


corridor_data %>%
  ggplot( aes(x=Distance, fill = Corr_or_greenspace)) +
  geom_histogram( alpha=0.5, position = 'identity', binwidth = 50) +
  facet_wrap(~Corr_or_greenspace) +
  scale_x_continuous(name="Site Distance to...",
                     breaks = seq(0,2800,500),
                     labels = seq(0,2800,500)) +
  theme(legend.position = 'none')

corr_dist <- (corridor_data$Distance)
bins <- seq(0,2700,by=50)
corr_table <- cut(corr_dist,bins)
# transform(table(corr_table))

corr_table1<- with(corridor_data, table(Corr_or_greenspace, cut(Distance, bins, include.lowest = TRUE)))
# transform(corr_table1)
library(data.table) # Extension of `data.frame`
corr_df <- setDT(corridor_data)[, .N, by = .(Corr_or_greenspace, Bin = cut(Distance, bins, include.lowest = TRUE))] 

corr_df_spread <- tidyr::spread(corr_df, Corr_or_greenspace, N)

corr_df_spread$Percent_Corridor <- as.numeric(corr_df_spread$Corridor) / .34
corr_df_spread$Percent_Greenspace <- as.numeric(corr_df_spread$Greenspace) / .34
corr_df_spread[is.na(corr_df_spread)] <- 0
corr_df_spread$Perc_Corr_Cumulative <- cumsum(corr_df_spread[, 4])
corr_df_spread$Perc_Greensp_Cumulative <- cumsum(corr_df_spread[, 5])

write.csv(corr_df_spread, here::here("./Corridor_Analysis/corr_df_spread.csv"), row.names = FALSE)
