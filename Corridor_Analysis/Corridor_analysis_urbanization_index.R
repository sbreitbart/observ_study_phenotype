# Load packages
library(dplyr)
library(ggplot2)
library("factoextra")
library(MASS)
library(patchwork)
library(ggpubr)
library(here)

# Set style
theme_set(theme_bw())

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
