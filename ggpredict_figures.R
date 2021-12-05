# Q1 / Gradient---------------------
## Distance from City Center----------

### Peduncles
peds_glmer_gradient_01
peds_gradient_01_pred <- ggeffects::ggpredict(peds_glmer_gradient_01,
                                   terms = c("City_dist", "Year"),
                                   type = "zero_inflated")

### Pollinia
poll_glmer_gradient_01.v2
poll_gradient_01_pred <- ggpredict(poll_glmer_gradient_01.v2,
                                   terms = c("City_dist", "Year"),
                                   type = "fe")

### Pods
pods_glmer_gradient_01
pods_gradient_01_pred <- ggpredict(pods_glmer_gradient_01,
                                   terms = c("City_dist", "Year"),
                                   type = "zero_inflated")

### Pods/Peduncle
podsperped_lmer_gradient_02
podsperped_gradient_01_pred <- ggpredict(podsperped_lmer_gradient_02,
                                         terms = c("City_dist", "Year"),
                                         type = "zero_inflated")




### Peduncles
ggpred_Q1.citydist_peds <- (ggplot(peds_gradient_01_pred) + 
    geom_smooth(aes(x = x, y = predicted, color = group),
                se = F) +
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error, fill = group),  alpha = 0.3) + 
    geom_point(data = fertile_pops_all, 
               aes(x = City_dist, y = Peduncles, colour = Year, shape = Year), size = 2)
)+
  scale_shape(solid = F)+
  labs( color="Year", shape="Year", fill = "Year") +
  ylim(0, 15) +
  theme(
    legend.position = c(.99, .99),
    legend.justification = c("right", "top"),
    legend.box = 'horizontal',
    legend.box.just = "right",
    legend.margin = margin(5,5,5,5),
    text = element_text(size=14),
    axis.title.x = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.key = element_rect(fill = NA) ) + 
  labs(x = "Distance to city center (km)", 
       y = "Inflorescences") 




### Pollen Removal
ggpred_Q1.citydist_poll <- (ggplot(poll_gradient_01_pred) + 
  geom_smooth(aes(x = x, y = predicted, color = group),
              se = F) +
  geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error, fill = group),  alpha = 0.3) + 
  geom_point(data = AvgVars_notNA_Poll_18_19, 
             aes(x = City_dist, y = Average_Pollinia, colour = Year, shape = Year), size = 2)
)+
  scale_shape(solid = F)+
  scale_y_continuous(breaks=c(0,1,2,3,4,5), limits=c(0,5)) +
  labs( color="Year", shape="Year", fill = "Year") +
  theme(
    legend.position = c(.99, .99),
    legend.justification = c("right", "top"),
    legend.box = 'horizontal',
    legend.box.just = "right",
    legend.margin = margin(5,5,5,5),
    text = element_text(size=14),
    axis.title.x = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.key = element_rect(fill = NA) ) + 
  labs(x = "Distance to city center (km)", 
       y = "Pollinaria Removed")




# Pods
ggpred_Q1.citydist_pods <- (ggplot(pods_gradient_01_pred) + 
    geom_smooth(aes(x = x, y = predicted, color = group),
                se = F) +
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error, fill = group),  alpha = 0.3) + 
    geom_point(data = fertile_pops_all, 
               aes(x = City_dist, y = Viable_Pods^2, colour = Year, shape = Year), size = 2)
)+
  labs( color="Year", shape="Year", fill = "Year") +
  ylim(0, 350) +
  scale_shape(solid = F)+
  theme(
    legend.position = c(.99, .99),
    legend.justification = c("right", "top"),
    legend.box = 'horizontal',
    legend.box.just = "right",
    legend.margin = margin(5,5,5,5),
    text = element_text(size=14),
    axis.title.x = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.key = element_rect(fill = NA) ) + 
  labs(x = "Distance to city center (km)", 
       y = expression(Follicles^{2}))




# Pods per Peduncle
ggpred_Q1.citydist_podsperped <- (ggplot(podsperped_gradient_01_pred) + 
    geom_smooth(aes(x = x, y = predicted, color = group),
                se = F) +
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error, fill = group),  alpha = 0.3) + 
    geom_point(data = fertile_pops_all, 
               aes(x = City_dist, y = pods_per_ped, colour = Year, shape = Year), size = 2)
)+
  scale_shape(solid = F)+
  labs( color="Year", shape="Year", fill = "Year") +
  ylim(0, 3) +
  theme(
    legend.position = c(.99, .99),
    legend.justification = c("right", "top"),
    legend.box = 'horizontal',
    legend.box.just = "right",
    legend.margin = margin(5,5,5,5),
    text = element_text(size=14),
    axis.title.x = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.key = element_rect(fill = NA) ) + 
  labs(x = "Distance to city center (km)", 
       y = "Follicles per Inflorescence") 


# COMBINE
Q1_regressions_citydist_ggpred <- ggarrange(ggpred_Q1.citydist_poll,
          ggpred_Q1.citydist_pods,
          ggpred_Q1.citydist_podsperped,
          ggpred_Q1.citydist_peds+
            font("x.text"),
          ncol = 4,
          nrow = 1,
          align = "hv",
          labels = list("A", "B", "C", "D"),
          font.label = (size =16),
          common.legend = T,
          legend = "top") %T>%
  plot


## Urbanization Score----------

### Peduncles
u_peds_glmer_gradient_01
peds_gradient_01_pred.u <- ggpredict(u_peds_glmer_gradient_01,
                                     terms = c("Urb_score", "Year"),
                                     type = "zero_inflated")

### Pollinia
u_poll_glmer_gradient_1
poll_gradient_01_pred.u <- ggpredict(u_poll_glmer_gradient_1,
                                     terms = c("Urb_score", "Year"),
                                     type = "fe")

### Pods
u_pods_glmer_gradient_1
pods_gradient_01_pred.u <- ggpredict(u_pods_glmer_gradient_1,
                                     terms = c("Urb_score", "Year"),
                                     type = "zero_inflated")

### Pods/Peduncle
u_podsperped_lmer_gradient_02
podsperped_gradient_01_pred.u <- ggpredict(u_podsperped_lmer_gradient_02,
                                           terms = c("Urb_score", "Year"),
                                           type = "fe")




### Peduncles
ggpred_Q1.urbscore_peds <- (ggplot(peds_gradient_01_pred.u) + 
                              geom_smooth(aes(x = x, y = predicted, color = group),
                                          se = F) +
                              geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error, fill = group),  alpha = 0.3) + 
                              geom_point(data = fertile_pops_all, 
                                         aes(x = Urb_score, y = Peduncles, colour = Year, shape = Year), size = 2)
)+
  scale_shape(solid = F)+
  xlim(-4, 4) +
  scale_x_reverse() +
  labs( color="Year", shape="Year", fill = "Year") +
  ylim(0, 15) +
  theme(
    legend.position = c(.99, .99),
    legend.justification = c("right", "top"),
    legend.box = 'horizontal',
    legend.box.just = "right",
    legend.margin = margin(5,5,5,5),
    text = element_text(size=14),
    # axis.title.x = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.key = element_rect(fill = NA) ) + 
  labs(x = "Urbanization Score", 
       y = "Inflorescences") 




### Pollen Removal
ggpred_Q1.urbscore_poll <- (ggplot(poll_gradient_01_pred.u) + 
                              geom_smooth(aes(x = x, y = predicted, color = group),
                                          se = F) +
                              geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error, fill = group),  alpha = 0.3) + 
                              geom_point(data = AvgVars_notNA_Poll_18_19, 
                                         aes(x = Urb_score, y = Average_Pollinia, colour = Year, shape = Year), size = 2)
)+
  scale_shape(solid = F)+
  xlim(-4, 4) +
  scale_x_reverse() +
  scale_y_continuous(breaks=c(0,1,2,3,4,5), limits=c(0,5)) +
  labs( color="Year", shape="Year", fill = "Year") +
  theme(
    legend.position = c(.99, .99),
    legend.justification = c("right", "top"),
    legend.box = 'horizontal',
    legend.box.just = "right",
    legend.margin = margin(5,5,5,5),
    text = element_text(size=14),
    # axis.title.x = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.key = element_rect(fill = NA) ) + 
  labs(x = "Urbanization Score", 
       y = "Pollinaria Removed")




# Pods
ggpred_Q1.urbscore_pods <- (ggplot(pods_gradient_01_pred.u) + 
                              geom_smooth(aes(x = x, y = predicted, color = group),
                                          se = F) +
                              geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error, fill = group),  alpha = 0.3) + 
                              geom_point(data = fertile_pops_all, 
                                         aes(x = Urb_score, y = Viable_Pods^3, colour = Year, shape = Year), size = 2)
)+
  labs( color="Year", shape="Year", fill = "Year") +
  # ylim(0, 350) +
  xlim(-4, 4) +
  scale_x_reverse() +
  scale_shape(solid = F)+
  theme(
    legend.position = c(.99, .99),
    legend.justification = c("right", "top"),
    legend.box = 'horizontal',
    legend.box.just = "right",
    legend.margin = margin(5,5,5,5),
    text = element_text(size=14),
    # axis.title.x = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.key = element_rect(fill = NA) ) + 
  labs(x = "Urbanization Score", 
       y = expression(Follicles^{3}))




# Pods per Peduncle
ggpred_Q1.urbscore_podsperped <- (ggplot(podsperped_gradient_01_pred.u) + 
                                    geom_smooth(aes(x = x, y = predicted, color = group),
                                                se = F) +
                                    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error, fill = group),  alpha = 0.3) + 
                                    geom_point(data = fertile_pops_all, 
                                               aes(x = Urb_score, y = pods_per_ped, colour = Year, shape = Year), size = 2)
)+
  scale_shape(solid = F)+
  xlim(-4, 4) +
  scale_x_reverse() +
  labs( color="Year", shape="Year", fill = "Year") +
  ylim(0, 3) +
  theme(
    legend.position = c(.99, .99),
    legend.justification = c("right", "top"),
    legend.box = 'horizontal',
    legend.box.just = "right",
    legend.margin = margin(5,5,5,5),
    text = element_text(size=14),
    # axis.title.x = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.key = element_rect(fill = NA) ) + 
  labs(x = "Urbanization Score", 
       y = "Follicles per Inflorescence") 



# HEIGHT- DISTANCE
height_lmer_gradient_02
height_gradient_01_pred <- ggpredict(height_lmer_gradient_02,
                                     terms = c("City_dist", "Year"),
                                     type = "fe")

ggpred_Q1.citydist_height <- (ggplot(height_gradient_01_pred) + 
                                geom_smooth(aes(x = x, y = predicted, color = group),
                                            se = F) +
                                geom_ribbon(aes(x = x, ymin = predicted - std.error,
                                                ymax = predicted + std.error, fill = group),
                                            alpha = 0.3) + 
                                geom_point(data = fertile_pops_all, 
                                           aes(x = City_dist, y = Height_Sept,
                                               colour = Year, shape = Year), size = 2)
)+
  labs(x = "Distance to Urban Center (km)", 
       y = "Height (cm)") +
  scale_shape(solid = F)+
  labs(color="Year", size="Year", fill = "Year") +
  scale_y_continuous(limits=c(0,200)) +
  labs(linetype="Year", color="Year", shape = "Year") +
  theme(legend.position = c(.99, .99),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.box = 'horizontal',
        legend.margin = margin(5,5,5,5),
        text = element_text(size=14),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = NA) )
ggpred_Q1.citydist_height


# HEIGHT- URB SCORE
u_height_lmer_gradient_01
height_gradient_01_pred.u <- ggpredict(u_height_lmer_gradient_01,
                                       terms = c("Urb_score", "Year"),
                                       type = "fe")

ggpred_Q1.citydist_height.u <- (ggplot(height_gradient_01_pred.u) + 
                                  geom_smooth(aes(x = x, y = predicted, color = group),
                                              se = F) +
                                  geom_ribbon(aes(x = x, ymin = predicted - std.error,
                                                  ymax = predicted + std.error, fill = group),
                                              alpha = 0.3) + 
                                  geom_point(data = fertile_pops_all, 
                                             aes(x = Urb_score, y = Height_Sept,
                                                 colour = Year, shape = Year), size = 2)
)+
  labs(x = "Urbanization Score", 
       y = "Height (cm)") +
  scale_shape(solid = F)+
  labs(color="Year", size="Year", fill = "Year") +
  scale_x_reverse() +
  xlim(3.5, -4) +
  scale_y_continuous(limits=c(0,200)) +
  labs(linetype="Year", color="Year", shape = "Year") +
  theme(legend.position = c(.99, .99),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.box = 'horizontal',
        legend.margin = margin(5,5,5,5),
        text = element_text(size=14),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = NA) )
ggpred_Q1.citydist_height.u



# PUT THIS PLOT INTO MAIN TEXT 
city_plots.ggpred <- annotate_figure(Q1_regressions_citydist_ggpred,
                                     bottom = text_grob("Distance to Urban Center (km)",
                                                        size=14))

dev.copy2pdf(file="~/R_Projects/chapter_one/Figures_Tables/Q1_Gradient/DISTGradient_regressions.pdf",
             width = 12, height = 3.75)





# this is now going into supplement. Going to add height
Q1_regressions_urbscore_ggpred <- ggarrange(ggpred_Q1.citydist_height, # city_dist
                                            ggpred_Q1.urbscore_poll, # urb_score
                                            ggpred_Q1.urbscore_pods, # urb_score
                                            ggpred_Q1.urbscore_podsperped, # urb_score
                                            ggpred_Q1.urbscore_peds , # urb_score
                                            ggpred_Q1.citydist_height.u + # urb_score
                                              font("x.text"),
                                            ncol = 3,
                                            nrow = 2,
                                            align = "hv",
                                            labels = list("A", "B", "C",
                                                          "D", "E", "F"),
                                            font.label = (size =16),
                                            common.legend = T,
                                            legend = "right") %T>%
  plot

dev.copy2pdf(file="~/R_Projects/chapter_one/Figures_Tables/Q1_Gradient/URBSCOREGradient_regressions.pdf",
             width = 12, height = 7)


## Find estimated marginal means at terminii-----
### Distance-----
Perc_change_gradient_ggpredict <- function(ggpredict_object){
  
  urban_estimate_2018 <- ggpredict_object %>%
    tibble %>%
    filter(x == 0 & group == "2018") %>%
    dplyr::select("predicted") %>%
    as.numeric() %>%
    round(3)
  
  urban_estimate_2019 <- ggpredict_object %>%
    tibble %>%
    filter(x == 0 & group == "2019") %>%
    dplyr::select("predicted") %>%
    as.numeric() %>%
    round(3)
  
  
  rural_estimate_2018 <- ggpredict_object %>%
    tibble %>%
    filter(x == 70 & group == "2018") %>%
    dplyr::select("predicted") %>%
    as.numeric() %>%
    round(3)
  
  rural_estimate_2019 <- ggpredict_object %>%
    tibble %>%
    filter(x == 70 & group == "2019") %>%
    dplyr::select("predicted") %>%
    as.numeric() %>%
    round(3)
    
    
  print(paste(noquote(c("There are an average of", urban_estimate_2018,
                        "[variable] per plant at the urban terminus in 2018")),
              collapse = ' '))
  
  print(paste(noquote(c("There are an average of", urban_estimate_2019,
                        "[variable] per plant at the urban terminus in 2019")),
              collapse = ' '))
  
  print(paste(noquote(c("There are an average of", rural_estimate_2018,
                        "[variable] per plant at the rural terminus in 2018")),
              collapse = ' '))
  
  print(paste(noquote(c("There are an average of", rural_estimate_2019,
                        "[variable] per plant at the rural terminus in 2019")),
              collapse = ' '))
  
  
  
  # AVG (var) FROM URBAN TO RURAL TERMINUS:
  avg_diff_2018 <- abs( urban_estimate_2018 - rural_estimate_2018 )  /
    (0.5* (    urban_estimate_2018 + rural_estimate_2018  )  ) # %T>%
  # print()
  
  # % CHANGE
  percent_change_2018 <- (((rural_estimate_2018 - urban_estimate_2018) /
                        abs(urban_estimate_2018)) * 100) # %T>%
  # print()
  
  # % CHANGE
  percent_change_2018.2 <- (((urban_estimate_2018 - rural_estimate_2018) /
                             abs(rural_estimate_2018)) * 100) # %T>%
  # print()
  
  
  
  
  avg_diff_2019 <- abs( urban_estimate_2019 - rural_estimate_2019 )  /
    (0.5* (    urban_estimate_2019 + rural_estimate_2019  )  ) # %T>%
  # print()
  
  # % CHANGE
  percent_change_2019 <- (((rural_estimate_2019 - urban_estimate_2019) /
                             abs(urban_estimate_2019)) * 100) # %T>%
  # print()
  
  # % CHANGE
  percent_change_2019.2 <- (((urban_estimate_2019 - rural_estimate_2019) /
                               abs(rural_estimate_2019)) * 100) # %T>%
  # print()
  
  
  
  
  print(paste(noquote(c("On average, the most rural plants produced",
                        round(percent_change_2018, 1), "% more",
                        "[variable] per plant than the most urban plants in 2018")),
              collapse = ' '))
  
  print(paste(noquote(c("On average, the most urban plants produced",
                        round(percent_change_2018.2, 1), "% more",
                        "[variable] per plant than the most rural plants in 2018")),
              collapse = ' '))
  
  print(paste(noquote(c("On average, the most rural plants produced",
                        round(percent_change_2019, 1), "% more",
                        "[variable] per plant than the most urban plants in 2019")),
              collapse = ' '))
  
  print(paste(noquote(c("On average, the most urban plants produced",
                        round(percent_change_2019.2, 1), "% more",
                        "[variable] per plant than the most rural plants in 2019")),
              collapse = ' '))
  
}
Perc_change_gradient_ggpredict_byyear <- function(ggpredict_object){
  mean_2018.1 <- ggpredict_object %>%
    tibble %>%
    filter(group == "2018") %>%
    dplyr::select("predicted")
  
  mean_2018 <- mean(mean_2018.1$predicted)
  
  mean_2019.1 <- ggpredict_object %>%
    tibble %>%
    filter(group == "2019") %>%
    dplyr::select("predicted") 
  
  mean_2019 <- mean(mean_2019.1$predicted)
  
  print(paste(noquote(c("2018 mean:", mean_2018)),
              collapse = ' '))
  
  print(paste(noquote(c("2019 mean:", mean_2019)),
              collapse = ' '))
  
  print(paste(noquote(c("2018 had", ((mean_2018 - mean_2019)/mean_2019),
                      "more [variable] than 2019")),
              collapse = ' '))
  
  print(paste(noquote(c("2019 had", ((mean_2019 - mean_2018)/mean_2018),
                      "more [variable] than 2018")),
              collapse = ' '))
}



Perc_change_gradient_ggpredict(peds_gradient_01_pred)
Perc_change_gradient_ggpredict_byyear(peds_gradient_01_pred)

Perc_change_gradient_ggpredict(poll_gradient_01_pred)
# per flower: urban/2018: 0.983/5 = 0.200 (20%)
#             urban/2019: 0.562/5 = 0.112 (11%)
#             rural/2018: 1.958/5 = 0.392 (39%)
#             rural/2019: 1.896/5 = 0.379 (38%)
Perc_change_gradient_ggpredict_byyear(poll_gradient_01_pred)



Perc_change_gradient_ggpredict(pods_gradient_01_pred) # remember this is squared
Perc_change_gradient_ggpredict_byyear(pods_gradient_01_pred)
# per plant: urban/2018: sqrt(150.314) = 12.26
#            urban/2019: sqrt(139.956) = 11.83
#            rural/2018: sqrt(121.149) = 11.01
#            rural/2019: sqrt(151.400) = 12.3

Perc_change_gradient_ggpredict(podsperped_gradient_01_pred)


# Q2 / Subtransects---------------------
## Distance from City Center----------

### Peduncles
peds_glmer_subtr_01
peds_subtr_01_pred <- ggpredict(peds_glmer_subtr_01,
                                   terms = c("City_dist", "Year", "Transect_ID"),
                                   type = "zero_inflated")

### Pollinia
poll_glmer_subtr_01
poll_subtr_01_pred <- ggpredict(poll_glmer_subtr_01,
                                   terms = c("City_dist", "Year", "Transect_ID"),
                                   type = "fe")

### Pods
pods_glmer_subtr_03
pods_subtr_01_pred <- ggpredict(pods_glmer_subtr_03,
                                   terms = c("City_dist", "Year", "Transect_ID"),
                                   type = "zero_inflated")

### Pods/Peduncle
podsperped_lmer_subtr_08
podsperped_subtr_01_pred <- ggpredict(podsperped_lmer_subtr_08,
                                         terms = c("City_dist", "Year", "Transect_ID"),
                                         type = "fe") %T>% view()





ggpred_Q2.citydist_peds <- (ggplot(peds_subtr_01_pred) +
                              geom_smooth(aes(x = x,
                                              y = predicted,
                                              color = facet,
                                              linetype = group),
                                          method = "loess",
                                          se = F) +
                              geom_ribbon(aes(x = x,
                                              ymin = predicted - std.error,
                                              ymax = predicted + std.error,
                                              fill = facet,
                                              linetype = group),
                                          alpha = 0.2)) + 
  geom_point(data = fertile_pops_all %>%
               filter(Transect_ID != "Rural"),
             aes(x = City_dist, y = Peduncles,
                 colour = Transect_ID,
                 shape = interaction(Year, Transect_ID)),
             size = 3) +
  scale_y_continuous(limits=c(0,15)) +
  labs(x = "Distance to Urban Center (km)",
       y = "Inflorescences") +
  scale_shape_manual(values = c(16,2,17,1)) +
  theme(legend.position = c(.99, .99),
        legend.justification = c("right", "top"),
        legend.box = 'horizontal',
        legend.box.just = "right",
        legend.margin = margin(5,5,5,5),
        text = element_text(size=14),
        axis.title.x = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = NA),
        legend.key.size = unit(2, "line") )



## factors (transects) for this object are North, then South
## for some reason (peduncles and at least one more is South, North)
## so to keep legend consistent, have to reorder levels first
poll_subtr_01_pred$facet <- factor(poll_subtr_01_pred$facet,
                                   levels(poll_subtr_01_pred$facet)[c(2,1)])
levels(poll_subtr_01_pred$facet)

ggpred_Q2.citydist_poll <- (ggplot(poll_subtr_01_pred) +
                              geom_smooth(aes(x = x,
                                              y = predicted,
                                              color = facet,
                                              linetype = group),
                                          method = "loess",
                                          se = F) +
                              geom_ribbon(aes(x = x,
                                              ymin = predicted - std.error,
                                              ymax = predicted + std.error,
                                              fill = facet,
                                              linetype = group),
                                          alpha = 0.2)) + 
  geom_point(data = AvgVars_notNA_Poll_18_19 %>%
               filter(Transect_ID != "Rural"),
             aes(x = City_dist, y = Average_Pollinia,
                 colour = Transect_ID,
                 shape = interaction(Year, Transect_ID)),
             size = 3) +
  scale_y_continuous(breaks=c(0,1,2,3), limits=c(0,3)) +
  labs(x = "Distance to Urban Center (km)",
       y = "Pollinaria Removed") +
  scale_shape_manual(values = c(16,2,17,1)) +
  theme(legend.position = c(.99, .99),
        legend.justification = c("right", "top"),
        legend.box = 'horizontal',
        legend.box.just = "right",
        legend.margin = margin(5,5,5,5),
        text = element_text(size=14),
        axis.title.x = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = NA),
        legend.key.size = unit(2, "line") ) 




ggpred_Q2.citydist_pods <- (ggplot(pods_subtr_01_pred) +
                              geom_smooth(aes(x = x,
                                              y = predicted,
                                              color = facet,
                                              linetype = group),
                                          method = "loess",
                                          se = F) +
                              geom_ribbon(aes(x = x,
                                              ymin = predicted - std.error,
                                              ymax = predicted + std.error,
                                              fill = facet,
                                              linetype = group),
                                          alpha = 0.2)) + 
  geom_point(data = fertile_pops_all %>%
               filter(Transect_ID != "Rural"),
             aes(x = City_dist, y = Viable_Pods^2,
                 colour = Transect_ID,
                 shape = interaction(Year, Transect_ID)),
             size = 3) +
  scale_y_continuous(limits=c(0,350)) +
  labs(x = "Distance to Urban Center (km)",
       y = expression(Follicles^{2})) +
  scale_shape_manual(values = c(16,2,17,1)) +
  theme(legend.position = c(.99, .99),
        legend.justification = c("right", "top"),
        legend.box = 'horizontal',
        legend.box.just = "right",
        legend.margin = margin(5,5,5,5),
        text = element_text(size=14),
        axis.title.x = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = NA),
        legend.key.size = unit(2, "line") )


## factors (transects) for this object are North, then South
## so to keep legend consistent, have to reorder levels first
podsperped_subtr_01_pred$facet <- factor(podsperped_subtr_01_pred$facet,
                                         levels(podsperped_subtr_01_pred$facet)[c(2,1)])
levels(podsperped_subtr_01_pred$facet)

ggpred_Q2.citydist_podsperped <- (ggplot(podsperped_subtr_01_pred) +
                                    geom_smooth(aes(x = x,
                                                    y = predicted,
                                                    color = facet,
                                                    linetype = group),
                                                method = "loess",
                                                se = F) +
                                    geom_ribbon(aes(x = x,
                                                    ymin = predicted - std.error,
                                                    ymax = predicted + std.error,
                                                    fill = facet,
                                                    linetype = group),
                                                alpha = 0.2)) + 
  geom_point(data = fertile_pops_all %>%
               filter(Transect_ID != "Rural"),
             aes(x = City_dist, y = pods_per_ped,
                 colour = Transect_ID,
                 shape = interaction(Year, Transect_ID)),
             size = 3) +
  scale_y_continuous(limits=c(0,3)) +
  labs(x = "Distance to Urban Center (km)",
       y = "Follicles per Inflorescence") +
  scale_shape_manual(values = c(16,2,17,1)) +
  theme(legend.position = c(.99, .99),
        legend.justification = c("right", "top"),
        legend.box = 'horizontal',
        legend.box.just = "right",
        legend.margin = margin(5,5,5,5),
        text = element_text(size=14),
        axis.title.x = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = NA),
        legend.key.size = unit(2, "line") )



# reordering for main text
Q2_regressions_citydist_ggpred <- ggarrange( ggpred_Q2.citydist_poll,
                                             ggpred_Q2.citydist_pods,
                                             ggpred_Q2.citydist_podsperped,
                                             ggpred_Q2.citydist_peds +
                                              font("x.text"),
                                            ncol = 4,
                                            nrow = 1,
                                            align = "hv",
                                            labels = list("A", "B", "C", "D"),
                                            font.label = (size =16),
                                            common.legend = T,
                                            legend = "none") %T>%
  plot

library(patchwork)
Q2_legend <- readPNG(here::here("./Figures_Tables/Q2_UrbanSubtransects/Q2_legend.png"),
                    native = TRUE)


city_plots.ggpred2 <- annotate_figure(Q2_regressions_citydist_ggpred,
                bottom = text_grob("Distance to Urban Center (km)",
                                   size=14)) +                  # Combine plot & image
  Q2_legend +
  patchwork::plot_layout(design =
 "#######B
  AAAAAAAA
  AAAAAAAA
  AAAAAAAA
  AAAAAAAA")

# annotate_figure(Q2_regressions_citydist_ggpred,
#                                       bottom = text_grob("Distance to Urban Center (km)",
#                                                          size=14)) +                  # Combine plot & image
#   Q2_legend +
#   patchwork::plot_layout(design = "AAAAAAAB
#                          AAAAAAA#
#                          AAAAAAA#")

# export to PDF
dev.copy2pdf(file="~/R_Projects/chapter_one/Figures_Tables/Q2_UrbanSubtransects/DISTSubtransect_regressions_ggpredict.pdf",
             width = 12, height = 4.5)



## Urbanization Score----------

### Peduncles
u_peds_glmer_subtr_06
peds_subtr_01_pred.u <- ggpredict(u_peds_glmer_subtr_06,
                                terms = c("Urb_score", "Year", "Transect_ID"),
                                type = "zero_inflated")

### Pollinia
u_poll_glmer_subtr_01
poll_subtr_01_pred.u <- ggpredict(u_poll_glmer_subtr_01,
                                terms = c("Urb_score", "Year", "Transect_ID"),
                                type = "fe")

### Pods
u_pods_glmer_subtr_03
pods_subtr_01_pred.u <- ggpredict(u_pods_glmer_subtr_03,
                                terms = c("Urb_score", "Year", "Transect_ID"),
                                type = "zero_inflated")

### Pods/Peduncle
u_podsperped_lmer_subtr_09
podsperped_subtr_01_pred.u <- ggpredict(u_podsperped_lmer_subtr_09,
                                      terms = c("Urb_score", "Year", "Transect_ID"),
                                      type = "fe") 





ggpred_Q2.urbscore_peds <- (ggplot(peds_subtr_01_pred.u) +
                              geom_smooth(aes(x = x,
                                              y = predicted,
                                              color = facet,
                                              linetype = group),
                                          method = "loess",
                                          se = F) +
                              geom_ribbon(aes(x = x,
                                              ymin = predicted - std.error,
                                              ymax = predicted + std.error,
                                              fill = facet,
                                              linetype = group),
                                          alpha = 0.2)) + 
  geom_point(data = fertile_pops_all %>%
               filter(Transect_ID != "Rural"),
             aes(x = Urb_score, y = Peduncles,
                 colour = Transect_ID,
                 shape = interaction(Year, Transect_ID)),
             size = 3) +
  xlim(4, -4) +
  scale_x_reverse() +
  scale_y_continuous(limits=c(0,15)) +
  labs(x = "Urbanization Score",
       y = "Inflorescences") +
  scale_shape_manual(values = c(16,2,17,1)) +
  theme(legend.position = c(.99, .99),
        legend.justification = c("right", "top"),
        legend.box = 'horizontal',
        legend.box.just = "right",
        legend.margin = margin(5,5,5,5),
        text = element_text(size=14),
        # axis.title.x = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = NA),
        legend.key.size = unit(2, "line") )


## factors (transects) for this object are North, then South
## for some reason (peduncles and at least one more is South, North)
## so to keep legend consistent, have to reorder levels first
poll_subtr_01_pred.u$facet <- factor(poll_subtr_01_pred.u$facet,
                                     levels(poll_subtr_01_pred.u$facet)[c(2,1)])
levels(poll_subtr_01_pred.u$facet)

ggpred_Q2.urbscore_poll <- (ggplot(poll_subtr_01_pred.u) +
    geom_smooth(aes(x = x,
                    y = predicted,
                    color = facet,
                    linetype = group),
                method = "loess",
                se = F) +
    geom_ribbon(aes(x = x,
                    ymin = predicted - std.error,
                    ymax = predicted + std.error,
                    fill = facet,
                    linetype = group),
                alpha = 0.2)) + 
  geom_point(data = AvgVars_notNA_Poll_18_19 %>%
               filter(Transect_ID != "Rural"),
             aes(x = Urb_score, y = Average_Pollinia,
                 colour = Transect_ID,
                 shape = interaction(Year, Transect_ID)),
             size = 3) +
  xlim(4, -4) +
  scale_x_reverse() +
  scale_y_continuous(breaks=c(0,1,2,3), limits=c(0,3)) +
  labs(x = "Urbanization Score",
       y = "Pollinaria Removed") +
  scale_shape_manual(values = c(16,2,17,1)) +
  theme(legend.position = c(.99, .99),
        legend.justification = c("right", "top"),
        legend.box = 'horizontal',
        legend.box.just = "right",
        legend.margin = margin(5,5,5,5),
        text = element_text(size=14),
        # axis.title.x = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = NA),
        legend.key.size = unit(2, "line") )



ggpred_Q2.urbscore_pods <- (ggplot(pods_subtr_01_pred.u) +
                              geom_smooth(aes(x = x,
                                              y = predicted,
                                              color = facet,
                                              linetype = group),
                                          method = "loess",
                                          se = F) +
                              geom_ribbon(aes(x = x,
                                              ymin = predicted - std.error,
                                              ymax = predicted + std.error,
                                              fill = facet,
                                              linetype = group),
                                          alpha = 0.2)) + 
  geom_point(data = fertile_pops_all %>%
               filter(Transect_ID != "Rural"),
             aes(x = Urb_score, y = Viable_Pods^3,
                 colour = Transect_ID,
                 shape = interaction(Year, Transect_ID)),
             size = 3) +
  xlim(4, -4) +
  scale_x_reverse() +
  labs(x = "Urbanization Score",
       y = expression(Follicles^{3})) +
  scale_shape_manual(values = c(16,2,17,1)) +
  theme(legend.position = c(.99, .99),
        legend.justification = c("right", "top"),
        legend.box = 'horizontal',
        legend.box.just = "right",
        legend.margin = margin(5,5,5,5),
        text = element_text(size=14),
        # axis.title.x = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = NA),
        legend.key.size = unit(2, "line") )




ggpred_Q2.urbscore_podsperped <- (ggplot(podsperped_subtr_01_pred.u) +
                                    geom_smooth(aes(x = x,
                                                    y = predicted,
                                                    color = facet,
                                                    linetype = group),
                                                method = "loess",
                                                se = F) +
                                    geom_ribbon(aes(x = x,
                                                    ymin = predicted - std.error,
                                                    ymax = predicted + std.error,
                                                    fill = facet,
                                                    linetype = group),
                                                alpha = 0.2)) + 
  geom_point(data = fertile_pops_all %>%
               filter(Transect_ID != "Rural"),
             aes(x = Urb_score, y = pods_per_ped,
                 colour = Transect_ID,
                 shape = interaction(Year, Transect_ID)),
             size = 3) +
  xlim(4, -4) +
  scale_x_reverse() +
  scale_y_continuous(limits=c(0,3)) +
  labs(x = "Urbanization Score",
       y = "Follicles per Inflorescence") +
  scale_shape_manual(values = c(16,2,17,1)) +
  theme(legend.position = c(.99, .99),
        legend.justification = c("right", "top"),
        legend.box = 'horizontal',
        legend.box.just = "right",
        legend.margin = margin(5,5,5,5),
        text = element_text(size=14),
        # axis.title.x = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key = element_rect(fill = NA),
        legend.key.size = unit(2, "line") )



### Height- DISTANCE
height_lmer_subtr_01
height_subtr_01_pred <- ggpredict(height_lmer_subtr_01,
                                  terms = c("City_dist", "Year", "Transect_ID"),
                                  type = "fe")

## factors (transects) for this object are North, then South
## so to keep legend consistent, have to reorder levels first
height_subtr_01_pred$facet <- factor(height_subtr_01_pred$facet,
                                     levels(height_subtr_01_pred$facet)[c(2,1)])
levels(height_subtr_01_pred$facet)

ggpred_Q2.citydist_height <- (ggplot(height_subtr_01_pred) +
                                geom_smooth(aes(x = x,
                                                y = predicted,
                                                color = facet,
                                                linetype = group),
                                            method = "loess",
                                            se = F) +
                                geom_ribbon(aes(x = x,
                                                ymin = predicted - std.error,
                                                ymax = predicted + std.error,
                                                fill = facet,
                                                linetype = group),
                                            alpha = 0.2)) + 
  geom_point(data = fertile_pops_all %>%
               filter(Transect_ID != "Rural"),
             aes(x = City_dist, y = Height_Sept,
                 colour = Transect_ID,
                 shape = interaction(Year, Transect_ID)),
             size = 3) +
  scale_y_continuous(limits=c(0,200)) +
  labs(x = "Distance to Urban Center (km)",
       y = "Height (cm)") +
  scale_shape_manual(values = c(16,2,17,1)) +
  theme( #legend.position = c(.97, .99),
    legend.justification = c("center"),
    legend.box = 'horizontal',
    legend.box.just = "center",
    legend.margin = margin(5,5,5,5),
    text = element_text(size=14),
    # axis.title.x = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.key = element_rect(fill = NA),
    legend.key.size = unit(2, "line") )



### Height- URB SCORE
u_height_lmer_subtr_01
height_subtr_01_pred.u <- ggpredict(u_height_lmer_subtr_01,
                                    terms = c("Urb_score", "Year", "Transect_ID"),
                                    type = "fe")


## factors (transects) for this object are North, then South
## so to keep legend consistent, have to reorder levels first
height_subtr_01_pred.u$facet <- factor(height_subtr_01_pred.u$facet,
                                       levels(height_subtr_01_pred.u$facet)[c(2,1)])
levels(height_subtr_01_pred.u$facet)

ggpred_Q2.citydist_height.u <- (ggplot(height_subtr_01_pred.u) +
                                  geom_smooth(aes(x = x,
                                                  y = predicted,
                                                  color = facet,
                                                  linetype = group),
                                              method = "loess",
                                              se = F) +
                                  geom_ribbon(aes(x = x,
                                                  ymin = predicted - std.error,
                                                  ymax = predicted + std.error,
                                                  fill = facet,
                                                  linetype = group),
                                              alpha = 0.2)) + 
  geom_point(data = fertile_pops_all %>%
               filter(Transect_ID != "Rural"),
             aes(x = Urb_score, y = Height_Sept,
                 colour = Transect_ID,
                 shape = interaction(Year, Transect_ID)),
             size = 3) +
  scale_x_reverse() +
  xlim(3.5, -2) +
  scale_y_continuous(limits=c(0,200)) +
  labs(x = "Urbanization Score",
       y = "Height (cm)") +
  scale_shape_manual(values = c(16,2,17,1)) +
  theme(# legend.position = c(.97, .99),
    legend.justification = c("center"),
    legend.box = 'horizontal',
    legend.box.just = "center",
    legend.margin = margin(5,5,5,5),
    text = element_text(size=14),
    # axis.title.x = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.key = element_rect(fill = NA),
    legend.key.size = unit(2, "line") )



# reordering for main text
# Q2_regressions_urbscore_ggpred <- ggarrange(ggpred_Q2.urbscore_poll,
#                                             ggpred_Q2.urbscore_pods,
#                                             ggpred_Q2.urbscore_podsperped, 
#                                             ggpred_Q2.urbscore_peds  +
#                                               font("x.text"),
#                                             ncol = 4,
#                                             nrow = 1,
#                                             align = "hv",
#                                             labels = list("E", "F", "G", "H"),
#                                             font.label = (size =16),
#                                             common.legend = T,
#                                             legend = "none") %T>%
#   plot

# library(patchwork)
# Q2_legend <- readPNG(here::here("./Figures_Tables/Q2_UrbanSubtransects/Q2_legend.png"),
#                      native = TRUE)
# 
# 
# urbscore_plots.ggpred2 <- annotate_figure(Q2_regressions_urbscore_ggpred,
#                                           bottom = text_grob("Urbanization Score",
#                                                              size=14)) +                  # Combine plot & image
#   Q2_legend +
#   patchwork::plot_layout(design =
#                            "#######B
#                             AAAAAAAA
#                             AAAAAAAA
#                             AAAAAAAA
#                             AAAAAAAA")
# 
# 
# # export to PDF
# dev.copy2pdf(file="~/R_Projects/chapter_one/Figures_Tables/Q2_UrbanSubtransects/URBSCORESubtransect_regressions_ggpredict.pdf",
#              width = 12, height = 4.5)






# this is now going into supplement. Going to add height
Q2_regressions_urbscore_ggpred <- ggarrange(ggpred_Q2.citydist_height, # city_dist
                                            ggpred_Q2.urbscore_poll, # urb_score
                                            ggpred_Q2.urbscore_pods, # urb_score
                                            ggpred_Q2.urbscore_podsperped, # urb_score
                                            ggpred_Q2.urbscore_peds , # urb_score
                                            ggpred_Q2.citydist_height.u + # urb_score
                                              font("x.text"),
                                            ncol = 3,
                                            nrow = 2,
                                            align = "hv",
                                            labels = list("A", "B", "C",
                                                          "D", "E", "F"),
                                            font.label = (size =16),
                                            common.legend = T,
                                            legend = "none") %T>%
  plot


library(patchwork)
Q2_legend <- readPNG(here::here("./Figures_Tables/Q2_UrbanSubtransects/Q2_legend.png"),
                     native = TRUE)


urbscore_plots.ggpred2 <- Q2_regressions_urbscore_ggpred +    # Combine plot & image
  Q2_legend +
  patchwork::plot_layout(design =
                           "#######B
                            AAAAAAAA
                            AAAAAAAA
                            AAAAAAAA
                            AAAAAAAA")


# export to PDF
dev.copy2pdf(file="~/R_Projects/chapter_one/Figures_Tables/Q2_UrbanSubtransects/URBSCORESubtransect_regressions_ggpredict.pdf",
             width = 12, height = 8.5)



############################
## Find estimated marginal means at terminii-----
### Distance-----

# Perc_change_subtransects_ggpredict <- function(ggpredict_object){
#   
#   urban_estimate_2018.north <- ggpredict_object %>%
#     tibble %>%
#     filter(x == min(x) & group == "2018" & facet == "North") %>%
#     dplyr::select("predicted") %>%
#     as.numeric() %>%
#     round(3)
#   
#   urban_estimate_2018.south <- ggpredict_object %>%
#     tibble %>%
#     filter(x == min(x) & group == "2018" & facet == "South") %>%
#     dplyr::select("predicted") %>%
#     as.numeric() %>%
#     round(3)
#   
#   urban_estimate_2019.north <- ggpredict_object %>%
#     tibble %>%
#     filter(x == min(x) & group == "2019" & facet == "North") %>%
#     dplyr::select("predicted") %>%
#     as.numeric() %>%
#     round(3)
#   
#   urban_estimate_2019.south <- ggpredict_object %>%
#     tibble %>%
#     filter(x == min(x) & group == "2019" & facet == "South") %>%
#     dplyr::select("predicted") %>%
#     as.numeric() %>%
#     round(3)
#   
#   
#   rural_estimate_2018.north <- ggpredict_object %>%
#     tibble %>%
#     filter(x == max(x) & group == "2018" & facet == "North") %>%
#     dplyr::select("predicted") %>%
#     as.numeric() %>%
#     round(3)
#   
#   rural_estimate_2018.south <- ggpredict_object %>%
#     tibble %>%
#     filter(x == max(x) & group == "2018" & facet == "South") %>%
#     dplyr::select("predicted") %>%
#     as.numeric() %>%
#     round(3)
#   
#   rural_estimate_2019.north <- ggpredict_object %>%
#     tibble %>%
#     filter(x == max(x) & group == "2019" & facet == "North") %>%
#     dplyr::select("predicted") %>%
#     as.numeric() %>%
#     round(3)
#   
#   rural_estimate_2019.south <- ggpredict_object %>%
#     tibble %>%
#     filter(x == max(x) & group == "2019" & facet == "South") %>%
#     dplyr::select("predicted") %>%
#     as.numeric() %>%
#     round(3)
#   
#   
#   print(paste(noquote(c("There are an average of", urban_estimate_2018.north,
#                         "[variable] per plant at the urban terminus of
#                         the North subtransect in 2018")),
#               collapse = ' '))
#   
#   print(paste(noquote(c("There are an average of", urban_estimate_2018.south,
#                         "[variable] per plant at the urban terminus of
#                         the South subtransect in 2018")),
#               collapse = ' '))
#   
#   print(paste(noquote(c("There are an average of", urban_estimate_2019.north,
#                         "[variable] per plant at the urban terminus of
#                         the North subtransect in 2019")),
#               collapse = ' '))
#   
#   print(paste(noquote(c("There are an average of", urban_estimate_2019.south,
#                         "[variable] per plant at the urban terminus of
#                         the South subtransect in 2019")),
#               collapse = ' '))
#   
#   
#   
#   
#   print(paste(noquote(c("There are an average of", rural_estimate_2018.north,
#                         "[variable] per plant at the rural terminus of
#                         the North subtransect in 2018")),
#               collapse = ' '))
#   
#   print(paste(noquote(c("There are an average of", rural_estimate_2018.south,
#                         "[variable] per plant at the rural terminus of
#                         the South subtransect in 2018")),
#               collapse = ' '))
#   
#   print(paste(noquote(c("There are an average of", rural_estimate_2019.north,
#                         "[variable] per plant at the rural terminus of
#                         the North subtransect in 2019")),
#               collapse = ' '))
#   
#   print(paste(noquote(c("There are an average of", rural_estimate_2019.south,
#                         "[variable] per plant at the rural terminus of
#                         the South subtransect in 2019")),
#               collapse = ' '))
# }




# PEDUNCLES----------------

# MEAN PEDUNCLES BY YEAR AND TRANSECT
mean_peds <- peds_subtr_01_pred %>%
  dplyr::filter(., x == 2 | x == 34) %>%
  dplyr::group_by(group, facet) %>%
  dplyr::summarise(mean_ped = mean(predicted)) %T>%
  view()

## PERCENT CHANGE, non-corr to corr subtransect:
perc_chg_peds <- mean_peds %>%
  as.data.frame() %>%
  dplyr::mutate(perc_chg_year = NA)

### percent change for 2018
perc_chg_peds[1,4] <- (perc_chg_peds[1,3]-perc_chg_peds[2,3])/perc_chg_peds[2,3]

### percent change for 2019
perc_chg_peds[3,4] <- (perc_chg_peds[3,3]-perc_chg_peds[4,3])/perc_chg_peds[4,3]




## PERCENT CHANGE, rural to urban terminii:
mean_peds2 <- peds_subtr_01_pred %>%
  dplyr::filter(., x == 2 | x == 34) %>%
  dplyr::group_by(x, group) %>%
  dplyr::summarise(terminus_mean = mean(predicted)) %T>%
  view()

perc_chg_peds2 <- mean_peds2 %>%
  as.data.frame() %>%
  dplyr::mutate(perc_chg_urbrur = NA)

### percent change for 2018
perc_chg_peds2[1,4] <- (perc_chg_peds2[1,3]-perc_chg_peds2[3,3])/perc_chg_peds2[3,3]

### percent change for 2019
perc_chg_peds2[2,4] <- (perc_chg_peds2[2,3]-perc_chg_peds2[4,3])/perc_chg_peds2[4,3]





# POLLINIA ----------------

# MEAN POLLINIA REMOVED BY YEAR AND TRANSECT
mean_poll <- poll_subtr_01_pred %>%
  mutate(poll_per_flower = predicted / 5) %>%
  dplyr::filter(., x == 2 | x == 34) %>%
  dplyr::group_by(group, facet) %>%
  dplyr::summarise(mean_poll = mean(poll_per_flower)) %T>%
  view()

## PERCENT CHANGE, non-corr to corr subtransect:
perc_chg_poll <- mean_poll %>%
  as.data.frame() %>%
  dplyr::mutate(perc_chg_year = NA)

### percent change for 2018
perc_chg_poll[2,4] <- (perc_chg_poll[2,3]-perc_chg_poll[1,3])/perc_chg_poll[1,3]

### percent change for 2019
perc_chg_poll[4,4] <- (perc_chg_poll[4,3]-perc_chg_poll[3,3])/perc_chg_poll[3,3]




## PERCENT CHANGE, rural to urban terminii:
mean_poll2 <- poll_subtr_01_pred %>%
  mutate(poll_per_flower = predicted / 5) %>%
  dplyr::filter(., x == 2 | x == 34) %>%
  dplyr::group_by(x, group) %>%
  dplyr::summarise(terminus_mean = mean(poll_per_flower)) %T>%
  view()

perc_chg_poll2 <- mean_poll2 %>%
  as.data.frame() %>%
  dplyr::mutate(perc_chg_urbrur = NA)

### percent change for 2018
perc_chg_poll2[1,4] <- (perc_chg_poll2[1,3]-perc_chg_poll2[3,3])/perc_chg_poll2[3,3]

### percent change for 2019
perc_chg_poll2[2,4] <- (perc_chg_poll2[2,3]-perc_chg_poll2[4,3])/perc_chg_poll2[4,3]





# PODS ----------------

# MEAN PODS BY YEAR AND TRANSECT
mean_pods <- pods_subtr_01_pred %>%
  dplyr::filter(., x == 2 | x == 34) %>%
  dplyr::mutate(sqrt_pods = sqrt(predicted)) %>%
  dplyr::group_by(x, group) %>%
  dplyr::summarise(mean_sqrt_pods = mean(sqrt_pods)) %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(mean_sqrt_pods = mean(mean_sqrt_pods)) %>%
  dplyr::mutate(perc_chg_year = NA) %T>%
  view()

### percent change from 2018-2019
mean_pods[1,3] <- (mean_pods[2,2]-mean_pods[1,2])/mean_pods[1,2]





# PODS PER PEDUNCLE ----------------

## PERCENT CHANGE, rural to urban terminii:
mean_ppp2 <- podsperped_subtr_01_pred %>%
  dplyr::filter(., x == 2 | x == 34) %>%
  dplyr::group_by(x, group) %>%
  dplyr::summarise(terminus_mean = mean(predicted)) %T>%
  view()

perc_chg_ppp2 <- mean_ppp2 %>%
  as.data.frame() %>%
  dplyr::mutate(perc_chg_urbrur = NA)

### percent change for 2018
perc_chg_ppp2[1,4] <- (perc_chg_ppp2[1,3]-perc_chg_ppp2[3,3])/perc_chg_ppp2[3,3]

### percent change for 2019
perc_chg_ppp2[2,4] <- (perc_chg_ppp2[2,3]-perc_chg_ppp2[4,3])/perc_chg_ppp2[4,3]




