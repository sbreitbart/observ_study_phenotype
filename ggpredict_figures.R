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



Q1_regressions_citydist_ggpred <- ggarrange(ggpred_Q1.citydist_peds , ggpred_Q1.citydist_poll, ggpred_Q1.citydist_pods,  ggpred_Q1.citydist_podsperped +
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
    axis.title.x = element_blank(),
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
    axis.title.x = element_blank(),
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
    axis.title.x = element_blank(),
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
    axis.title.x = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.key = element_rect(fill = NA) ) + 
  labs(x = "Urbanization Score", 
       y = "Follicles per Inflorescence") 



Q1_regressions_urbscore_ggpred <- ggarrange(ggpred_Q1.urbscore_peds ,
                                            ggpred_Q1.urbscore_poll,
                                            ggpred_Q1.urbscore_pods,
                                            ggpred_Q1.urbscore_podsperped +
                                              font("x.text"),
                                            ncol = 4,
                                            nrow = 1,
                                            align = "hv",
                                            labels = list("A", "B", "C", "D"),
                                            font.label = (size =16),
                                            common.legend = T,
                                            legend = "none") %T>%
  plot


city_plots.ggpred <- annotate_figure(Q1_regressions_citydist_ggpred,
                                     bottom = text_grob("Distance to Urban Center (km)",
                                                        size=14))
urbscore_plots.ggpred <- annotate_figure(Q1_regressions_urbscore_ggpred,
                                         bottom = text_grob("Urbanization Score",
                                                            size=14))

city_plots.ggpred/urbscore_plots.ggpred

dev.copy2pdf(file="~/R_Projects/chapter_one/Figures_Tables/Q1_Gradient/Gradient_regressions_ggpredict.pdf",
             width = 12, height = 8)


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
                        abs(rural_estimate_2018)) * 100) # %T>%
  # print()
  
  # % CHANGE
  percent_change_2018.2 <- (((urban_estimate_2018 - rural_estimate_2018) /
                             abs(urban_estimate_2018)) * 100) # %T>%
  # print()
  
  
  
  
  avg_diff_2019 <- abs( urban_estimate_2019 - rural_estimate_2019 )  /
    (0.5* (    urban_estimate_2019 + rural_estimate_2019  )  ) # %T>%
  # print()
  
  # % CHANGE
  percent_change_2019 <- (((rural_estimate_2019 - urban_estimate_2019) /
                             abs(rural_estimate_2019)) * 100) # %T>%
  # print()
  
  # % CHANGE
  percent_change_2019.2 <- (((urban_estimate_2019 - rural_estimate_2019) /
                               abs(urban_estimate_2019)) * 100) # %T>%
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
  
  print(paste(noquote(c("2018 had", ((mean_2018 - mean_2019)/mean_2018),
                      "more [variable] than 2019")),
              collapse = ' '))
  
  print(paste(noquote(c("2019 had", ((mean_2019 - mean_2018)/mean_2019),
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
                                              color = interaction(group,facet),
                                              linetype = interaction(group,facet)),
                                          method = "loess",
                                          se = F) +
                              geom_ribbon(aes(x = x,
                                              ymin = predicted - std.error,
                                              ymax = predicted + std.error,
                                              fill = interaction(group,facet),
                                              linetype = interaction(group,facet)),
                                          alpha = 0.2)) + 
  geom_point(data = fertile_pops_all %>%
               filter(Transect_ID != "Rural"),
             aes(x = City_dist, y = Peduncles,
                 colour = interaction(Year, Transect_ID),
                 shape = interaction(Year, Transect_ID)),
             size = 3) +
  scale_y_continuous(limits=c(0,15)) +
  labs(x = "Distance to Urban Center (km)",
       y = "Inflorescences",
       color = "Year, Subtransect",
       shape = "Year, Subtransect",
       fill = "Year, Subtransect",
       linetype = "Year, Subtransect") +
  scale_shape_manual(values = c(16,2,17,1),
                     labels = c("2018- Urban: Non-Corridor",
                                "2019- Urban: Non-Corridor",
                                "2018- Urban: Corridor",
                                "2019- Urban: Corridor")) +
  scale_color_manual(values = c("#00BFC4", "#00BFC4", "#F8766D", "#F8766D"),
                     labels = c("2018- Urban: Non-Corridor",
                                "2019- Urban: Non-Corridor",
                                "2018- Urban: Corridor",
                                "2019- Urban: Corridor")) +
  scale_fill_manual(values = c("#00BFC4", "#00BFC4", "#F8766D", "#F8766D"),
                    labels = c("2018- Urban: Non-Corridor",
                               "2019- Urban: Non-Corridor",
                               "2018- Urban: Corridor",
                               "2019- Urban: Corridor")) +
  scale_linetype_manual(values = c('solid', 'dashed', 'solid', 'dashed'),
                        labels = c("2018- Urban: Non-Corridor",
                                   "2019- Urban: Non-Corridor",
                                   "2018- Urban: Corridor",
                                   "2019- Urban: Corridor")) +
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
ggpred_Q2.citydist_peds



ggpred_Q2.citydist_poll <- (ggplot(poll_subtr_01_pred) +
     geom_smooth(aes(x = x,
                     y = predicted,
                     color = interaction(group,facet),
                     linetype = interaction(group,facet)),
                 method = "loess",
                 se = F) +
     geom_ribbon(aes(x = x,
                     ymin = predicted - std.error,
                     ymax = predicted + std.error,
                     fill = interaction(group,facet),
                     linetype = interaction(group,facet)),
                 alpha = 0.2)) + 
  geom_point(data = AvgVars_notNA_Poll_18_19 %>%
               filter(Transect_ID != "Rural"),
             aes(x = City_dist, y = Average_Pollinia,
                 colour = interaction(Year, Transect_ID),
                 shape = interaction(Year, Transect_ID)),
             size = 3) +
  scale_y_continuous(breaks=c(0,1,2,3), limits=c(0,3)) +
  labs(x = "Distance to Urban Center (km)",
       y = "Pollinaria Removed",
       color = "Year, Subtransect",
       shape = "Year, Subtransect",
       fill = "Year, Subtransect",
       linetype = "Year, Subtransect") +
  scale_shape_manual(values = c(16,2,17,1),
                     labels = c("2018- Urban: Non-Corridor",
                                "2019- Urban: Non-Corridor",
                                "2018- Urban: Corridor",
                                "2019- Urban: Corridor")) +
  scale_color_manual(values = c("#00BFC4", "#00BFC4", "#F8766D", "#F8766D"),
                     labels = c("2018- Urban: Non-Corridor",
                                "2019- Urban: Non-Corridor",
                                "2018- Urban: Corridor",
                                "2019- Urban: Corridor")) +
  scale_fill_manual(values = c("#00BFC4", "#00BFC4", "#F8766D", "#F8766D"),
                     labels = c("2018- Urban: Non-Corridor",
                                "2019- Urban: Non-Corridor",
                                "2018- Urban: Corridor",
                                "2019- Urban: Corridor")) +
  scale_linetype_manual(values = c('solid', 'dashed', 'solid', 'dashed'),
                        labels = c("2018- Urban: Non-Corridor",
                                   "2019- Urban: Non-Corridor",
                                   "2018- Urban: Corridor",
                                   "2019- Urban: Corridor")) +
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
ggpred_Q2.citydist_poll



ggpred_Q2.citydist_pods <- (ggplot(pods_subtr_01_pred) +
                              geom_smooth(aes(x = x,
                                              y = predicted,
                                              color = interaction(group,facet),
                                              linetype = interaction(group,facet)),
                                          method = "loess",
                                          se = F) +
                              geom_ribbon(aes(x = x,
                                              ymin = predicted - std.error,
                                              ymax = predicted + std.error,
                                              fill = interaction(group,facet),
                                              linetype = interaction(group,facet)),
                                          alpha = 0.2)) + 
  geom_point(data = fertile_pops_all %>%
               filter(Transect_ID != "Rural"),
             aes(x = City_dist, y = Viable_Pods^2,
                 colour = interaction(Year, Transect_ID),
                 shape = interaction(Year, Transect_ID)),
             size = 3) +
  scale_y_continuous(limits=c(0,350)) +
  labs(x = "Distance to Urban Center (km)",
       y = expression(Follicles^{2}),
       color = "Year, Subtransect",
       shape = "Year, Subtransect",
       fill = "Year, Subtransect",
       linetype = "Year, Subtransect") +
  scale_shape_manual(values = c(16,2,17,1),
                     labels = c("2018- Urban: Non-Corridor",
                                "2019- Urban: Non-Corridor",
                                "2018- Urban: Corridor",
                                "2019- Urban: Corridor")) +
  scale_color_manual(values = c("#00BFC4", "#00BFC4", "#F8766D", "#F8766D"),
                     labels = c("2018- Urban: Non-Corridor",
                                "2019- Urban: Non-Corridor",
                                "2018- Urban: Corridor",
                                "2019- Urban: Corridor")) +
  scale_fill_manual(values = c("#00BFC4", "#00BFC4", "#F8766D", "#F8766D"),
                    labels = c("2018- Urban: Non-Corridor",
                               "2019- Urban: Non-Corridor",
                               "2018- Urban: Corridor",
                               "2019- Urban: Corridor")) +
  scale_linetype_manual(values = c('solid', 'dashed', 'solid', 'dashed'),
                        labels = c("2018- Urban: Non-Corridor",
                                   "2019- Urban: Non-Corridor",
                                   "2018- Urban: Corridor",
                                   "2019- Urban: Corridor")) +
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
ggpred_Q2.citydist_pods



ggpred_Q2.citydist_podsperped <- (ggplot(podsperped_subtr_01_pred) +
                              geom_smooth(aes(x = x,
                                              y = predicted,
                                              color = interaction(group,facet),
                                              linetype = interaction(group,facet)),
                                          method = "loess",
                                          se = F) +
                              geom_ribbon(aes(x = x,
                                              ymin = predicted - std.error,
                                              ymax = predicted + std.error,
                                              fill = interaction(group,facet),
                                              linetype = interaction(group,facet)),
                                          alpha = 0.2)) + 
  geom_point(data = fertile_pops_all %>%
               filter(Transect_ID != "Rural"),
             aes(x = City_dist, y = pods_per_ped,
                 colour = interaction(Year, Transect_ID),
                 shape = interaction(Year, Transect_ID)),
             size = 3) +
  scale_y_continuous(limits=c(0,3)) +
  labs(x = "Distance to Urban Center (km)",
       y = "Follicles per Inflorescence",
       color = "Year, Subtransect",
       shape = "Year, Subtransect",
       fill = "Year, Subtransect",
       linetype = "Year, Subtransect") +
  scale_shape_manual(values = c(16,2,17,1),
                     labels = c("2018- Urban: Non-Corridor",
                                "2019- Urban: Non-Corridor",
                                "2018- Urban: Corridor",
                                "2019- Urban: Corridor")) +
  scale_color_manual(values = c("#00BFC4", "#00BFC4", "#F8766D", "#F8766D"),
                     labels = c("2018- Urban: Non-Corridor",
                                "2019- Urban: Non-Corridor",
                                "2018- Urban: Corridor",
                                "2019- Urban: Corridor")) +
  scale_fill_manual(values = c("#00BFC4", "#00BFC4", "#F8766D", "#F8766D"),
                    labels = c("2018- Urban: Non-Corridor",
                               "2019- Urban: Non-Corridor",
                               "2018- Urban: Corridor",
                               "2019- Urban: Corridor")) +
  scale_linetype_manual(values = c('solid', 'dashed', 'solid', 'dashed'),
                        labels = c("2018- Urban: Non-Corridor",
                                   "2019- Urban: Non-Corridor",
                                   "2018- Urban: Corridor",
                                   "2019- Urban: Corridor")) +
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
ggpred_Q2.citydist_podsperped



Q2_regressions_citydist_ggpred <- ggarrange(ggpred_Q2.citydist_peds ,
                                            ggpred_Q2.citydist_poll,
                                            ggpred_Q2.citydist_pods,
                                            ggpred_Q2.citydist_podsperped +
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
                                              color = interaction(group,facet),
                                              linetype = interaction(group,facet)),
                                          method = "loess",
                                          se = F) +
                              geom_ribbon(aes(x = x,
                                              ymin = predicted - std.error,
                                              ymax = predicted + std.error,
                                              fill = interaction(group,facet),
                                              linetype = interaction(group,facet)),
                                          alpha = 0.2)) + 
  geom_point(data = fertile_pops_all %>%
               filter(Transect_ID != "Rural"),
             aes(x = Urb_score, y = Peduncles,
                 colour = interaction(Year, Transect_ID),
                 shape = interaction(Year, Transect_ID)),
             size = 3) +
  xlim(4, -4) +
  scale_x_reverse() +
  scale_y_continuous(limits=c(0,15)) +
  labs(x = "Urbanization Score",
       y = "Inflorescences",
       color = "Year, Subtransect",
       shape = "Year, Subtransect",
       fill = "Year, Subtransect",
       linetype = "Year, Subtransect") +
  scale_shape_manual(values = c(16,2,17,1),
                     labels = c("2018- Urban: Non-Corridor",
                                "2019- Urban: Non-Corridor",
                                "2018- Urban: Corridor",
                                "2019- Urban: Corridor")) +
  scale_color_manual(values = c("#00BFC4", "#00BFC4", "#F8766D", "#F8766D"),
                     labels = c("2018- Urban: Non-Corridor",
                                "2019- Urban: Non-Corridor",
                                "2018- Urban: Corridor",
                                "2019- Urban: Corridor")) +
  scale_fill_manual(values = c("#00BFC4", "#00BFC4", "#F8766D", "#F8766D"),
                    labels = c("2018- Urban: Non-Corridor",
                               "2019- Urban: Non-Corridor",
                               "2018- Urban: Corridor",
                               "2019- Urban: Corridor")) +
  scale_linetype_manual(values = c('solid', 'dashed', 'solid', 'dashed'),
                        labels = c("2018- Urban: Non-Corridor",
                                   "2019- Urban: Non-Corridor",
                                   "2018- Urban: Corridor",
                                   "2019- Urban: Corridor")) +
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
ggpred_Q2.urbscore_peds



ggpred_Q2.urbscore_poll <- (ggplot(poll_subtr_01_pred.u) +
                              geom_smooth(aes(x = x,
                                              y = predicted,
                                              color = interaction(group,facet),
                                              linetype = interaction(group,facet)),
                                          method = "loess",
                                          se = F) +
                              geom_ribbon(aes(x = x,
                                              ymin = predicted - std.error,
                                              ymax = predicted + std.error,
                                              fill = interaction(group,facet),
                                              linetype = interaction(group,facet)),
                                          alpha = 0.2)) + 
  geom_point(data = AvgVars_notNA_Poll_18_19 %>%
               filter(Transect_ID != "Rural"),
             aes(x = Urb_score, y = Average_Pollinia,
                 colour = interaction(Year, Transect_ID),
                 shape = interaction(Year, Transect_ID)),
             size = 3) +
  xlim(4, -4) +
  scale_x_reverse() +
  scale_y_continuous(breaks=c(0,1,2,3), limits=c(0,3)) +
  labs(x = "Urbanization Score",
       y = "Pollinaria Removed",
       color = "Year, Subtransect",
       shape = "Year, Subtransect",
       fill = "Year, Subtransect",
       linetype = "Year, Subtransect") +
  scale_shape_manual(values = c(16,2,17,1),
                     labels = c("2018- Urban: Non-Corridor",
                                "2019- Urban: Non-Corridor",
                                "2018- Urban: Corridor",
                                "2019- Urban: Corridor")) +
  scale_color_manual(values = c("#00BFC4", "#00BFC4", "#F8766D", "#F8766D"),
                     labels = c("2018- Urban: Non-Corridor",
                                "2019- Urban: Non-Corridor",
                                "2018- Urban: Corridor",
                                "2019- Urban: Corridor")) +
  scale_fill_manual(values = c("#00BFC4", "#00BFC4", "#F8766D", "#F8766D"),
                    labels = c("2018- Urban: Non-Corridor",
                               "2019- Urban: Non-Corridor",
                               "2018- Urban: Corridor",
                               "2019- Urban: Corridor")) +
  scale_linetype_manual(values = c('solid', 'dashed', 'solid', 'dashed'),
                        labels = c("2018- Urban: Non-Corridor",
                                   "2019- Urban: Non-Corridor",
                                   "2018- Urban: Corridor",
                                   "2019- Urban: Corridor")) +
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
ggpred_Q2.urbscore_poll



ggpred_Q2.urbscore_pods <- (ggplot(pods_subtr_01_pred.u) +
                              geom_smooth(aes(x = x,
                                              y = predicted,
                                              color = interaction(group,facet),
                                              linetype = interaction(group,facet)),
                                          method = "loess",
                                          se = F) +
                              geom_ribbon(aes(x = x,
                                              ymin = predicted - std.error,
                                              ymax = predicted + std.error,
                                              fill = interaction(group,facet),
                                              linetype = interaction(group,facet)),
                                          alpha = 0.2)) + 
  geom_point(data = fertile_pops_all %>%
               filter(Transect_ID != "Rural"),
             aes(x = Urb_score, y = Viable_Pods^3,
                 colour = interaction(Year, Transect_ID),
                 shape = interaction(Year, Transect_ID)),
             size = 3) +
  xlim(4, -4) +
  scale_x_reverse() +
  # scale_y_continuous(limits=c(0,350)) +
  labs(x = "Urbanization Score",
       y = expression(Follicles^{3}),
       color = "Year, Subtransect",
       shape = "Year, Subtransect",
       fill = "Year, Subtransect",
       linetype = "Year, Subtransect") +
  scale_shape_manual(values = c(16,2,17,1),
                     labels = c("2018- Urban: Non-Corridor",
                                "2019- Urban: Non-Corridor",
                                "2018- Urban: Corridor",
                                "2019- Urban: Corridor")) +
  scale_color_manual(values = c("#00BFC4", "#00BFC4", "#F8766D", "#F8766D"),
                     labels = c("2018- Urban: Non-Corridor",
                                "2019- Urban: Non-Corridor",
                                "2018- Urban: Corridor",
                                "2019- Urban: Corridor")) +
  scale_fill_manual(values = c("#00BFC4", "#00BFC4", "#F8766D", "#F8766D"),
                    labels = c("2018- Urban: Non-Corridor",
                               "2019- Urban: Non-Corridor",
                               "2018- Urban: Corridor",
                               "2019- Urban: Corridor")) +
  scale_linetype_manual(values = c('solid', 'dashed', 'solid', 'dashed'),
                        labels = c("2018- Urban: Non-Corridor",
                                   "2019- Urban: Non-Corridor",
                                   "2018- Urban: Corridor",
                                   "2019- Urban: Corridor")) +
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
ggpred_Q2.urbscore_pods



ggpred_Q2.urbscore_podsperped <- (ggplot(podsperped_subtr_01_pred.u) +
                                    geom_smooth(aes(x = x,
                                                    y = predicted,
                                                    color = interaction(group,facet),
                                                    linetype = interaction(group,facet)),
                                                method = "loess",
                                                se = F) +
                                    geom_ribbon(aes(x = x,
                                                    ymin = predicted - std.error,
                                                    ymax = predicted + std.error,
                                                    fill = interaction(group,facet),
                                                    linetype = interaction(group,facet)),
                                                alpha = 0.2)) + 
  geom_point(data = fertile_pops_all %>%
               filter(Transect_ID != "Rural"),
             aes(x = Urb_score, y = pods_per_ped,
                 colour = interaction(Year, Transect_ID),
                 shape = interaction(Year, Transect_ID)),
             size = 3) +
  xlim(4, -4) +
  scale_x_reverse() +
  scale_y_continuous(limits=c(0,3)) +
  labs(x = "Urbanization Score",
       y = "Follicles per Inflorescence",
       color = "Year, Subtransect",
       shape = "Year, Subtransect",
       fill = "Year, Subtransect",
       linetype = "Year, Subtransect") +
  scale_shape_manual(values = c(16,2,17,1),
                     labels = c("2018- Urban: Non-Corridor",
                                "2019- Urban: Non-Corridor",
                                "2018- Urban: Corridor",
                                "2019- Urban: Corridor")) +
  scale_color_manual(values = c("#00BFC4", "#00BFC4", "#F8766D", "#F8766D"),
                     labels = c("2018- Urban: Non-Corridor",
                                "2019- Urban: Non-Corridor",
                                "2018- Urban: Corridor",
                                "2019- Urban: Corridor")) +
  scale_fill_manual(values = c("#00BFC4", "#00BFC4", "#F8766D", "#F8766D"),
                    labels = c("2018- Urban: Non-Corridor",
                               "2019- Urban: Non-Corridor",
                               "2018- Urban: Corridor",
                               "2019- Urban: Corridor")) +
  scale_linetype_manual(values = c('solid', 'dashed', 'solid', 'dashed'),
                        labels = c("2018- Urban: Non-Corridor",
                                   "2019- Urban: Non-Corridor",
                                   "2018- Urban: Corridor",
                                   "2019- Urban: Corridor")) +
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
ggpred_Q2.urbscore_podsperped




Q2_regressions_urbscore_ggpred <- ggarrange(ggpred_Q2.urbscore_peds ,
                                            ggpred_Q2.urbscore_poll,
                                            ggpred_Q2.urbscore_pods,
                                            ggpred_Q2.urbscore_podsperped +
                                              font("x.text"),
                                            ncol = 4,
                                            nrow = 1,
                                            align = "hv",
                                            labels = list("E", "F", "G", "H"),
                                            font.label = (size =16),
                                            common.legend = T,
                                            legend = "none") %T>%
  plot


city_plots.ggpred2 <- annotate_figure(Q2_regressions_citydist_ggpred,
                                     bottom = text_grob("Distance to Urban Center (km)",
                                                        size=14))
urbscore_plots.ggpred2 <- annotate_figure(Q2_regressions_urbscore_ggpred,
                                         bottom = text_grob("Urbanization Score",
                                                            size=14))

city_plots.ggpred2/urbscore_plots.ggpred2

dev.copy2pdf(file="~/R_Projects/chapter_one/Figures_Tables/Q2_UrbanSubtransects/Subtransect_regressions_ggpredict.pdf",
             width = 12, height = 8)

## Find estimated marginal means at terminii-----
### Distance-----
Perc_change_subtransects_ggpredict <- function(ggpredict_object){
  
  urban_estimate_2018.north <- ggpredict_object %>%
    tibble %>%
    filter(x == min(x) & group == "2018" & facet == "North") %>%
    dplyr::select("predicted") %>%
    as.numeric() %>%
    round(3)
  
  urban_estimate_2018.south <- ggpredict_object %>%
    tibble %>%
    filter(x == min(x) & group == "2018" & facet == "South") %>%
    dplyr::select("predicted") %>%
    as.numeric() %>%
    round(3)
  
  urban_estimate_2019.north <- ggpredict_object %>%
    tibble %>%
    filter(x == min(x) & group == "2019" & facet == "North") %>%
    dplyr::select("predicted") %>%
    as.numeric() %>%
    round(3)
  
  urban_estimate_2019.south <- ggpredict_object %>%
    tibble %>%
    filter(x == min(x) & group == "2019" & facet == "South") %>%
    dplyr::select("predicted") %>%
    as.numeric() %>%
    round(3)
  
  
  rural_estimate_2018.north <- ggpredict_object %>%
    tibble %>%
    filter(x == max(x) & group == "2018" & facet == "North") %>%
    dplyr::select("predicted") %>%
    as.numeric() %>%
    round(3)
  
  rural_estimate_2018.south <- ggpredict_object %>%
    tibble %>%
    filter(x == max(x) & group == "2018" & facet == "South") %>%
    dplyr::select("predicted") %>%
    as.numeric() %>%
    round(3)
  
  rural_estimate_2019.north <- ggpredict_object %>%
    tibble %>%
    filter(x == max(x) & group == "2019" & facet == "North") %>%
    dplyr::select("predicted") %>%
    as.numeric() %>%
    round(3)
  
  rural_estimate_2019.south <- ggpredict_object %>%
    tibble %>%
    filter(x == max(x) & group == "2019" & facet == "South") %>%
    dplyr::select("predicted") %>%
    as.numeric() %>%
    round(3)
  
  
  print(paste(noquote(c("There are an average of", urban_estimate_2018.north,
                        "[variable] per plant at the urban terminus of
                        the North subtransect in 2018")),
              collapse = ' '))
  
  print(paste(noquote(c("There are an average of", urban_estimate_2018.south,
                        "[variable] per plant at the urban terminus of
                        the South subtransect in 2018")),
              collapse = ' '))
  
  print(paste(noquote(c("There are an average of", urban_estimate_2019.north,
                        "[variable] per plant at the urban terminus of
                        the North subtransect in 2019")),
              collapse = ' '))
  
  print(paste(noquote(c("There are an average of", urban_estimate_2019.south,
                        "[variable] per plant at the urban terminus of
                        the South subtransect in 2019")),
              collapse = ' '))
  
  
  
  
  print(paste(noquote(c("There are an average of", rural_estimate_2018.north,
                        "[variable] per plant at the rural terminus of
                        the North subtransect in 2018")),
              collapse = ' '))
  
  print(paste(noquote(c("There are an average of", rural_estimate_2018.south,
                        "[variable] per plant at the rural terminus of
                        the South subtransect in 2018")),
              collapse = ' '))
  
  print(paste(noquote(c("There are an average of", rural_estimate_2019.north,
                        "[variable] per plant at the rural terminus of
                        the North subtransect in 2019")),
              collapse = ' '))
  
  print(paste(noquote(c("There are an average of", rural_estimate_2019.south,
                        "[variable] per plant at the rural terminus of
                        the South subtransect in 2019")),
              collapse = ' '))
  
  
  

  # % CHANGE
  percent_change_2018.2.north <- (((urban_estimate_2018.north - rural_estimate_2018.north) /
                               abs(rural_estimate_2018.north)) * 100) # %T>%
  # print()
  
  percent_change_2018.2.south <- (((urban_estimate_2018.south - rural_estimate_2018.south) /
                                     abs(rural_estimate_2018.south)) * 100) # %T>%
  # print()
  
  
  
  # % CHANGE
  percent_change_2019.2.north <- (((urban_estimate_2019.north - rural_estimate_2019.north) /
                               abs(rural_estimate_2019.north)) * 100) # %T>%
  # print()
  
  percent_change_2019.2.south <- (((urban_estimate_2019.south - rural_estimate_2019.south) /
                                     abs(rural_estimate_2019.south)) * 100) # %T>%
  # print()
  
  
  
  print(paste(noquote(c("On average, the most urban plants produced",
                        round(percent_change_2018.2.north, 1), "% more",
                        "[variable] per plant than the most rural plants in 2018
                        along the North subtransect")),
              collapse = ' '))
  
  print(paste(noquote(c("On average, the most urban plants produced",
                        round(percent_change_2018.2.south, 1), "% more",
                        "[variable] per plant than the most rural plants in 2018
                        along the South subtransect")),
              collapse = ' '))
  

  
  print(paste(noquote(c("On average, the most urban plants produced",
                        round(percent_change_2019.2.north, 1), "% more",
                        "[variable] per plant than the most rural plants in 2019
                        along the North subtransect")),
              collapse = ' '))
  
  print(paste(noquote(c("On average, the most urban plants produced",
                        round(percent_change_2019.2.south, 1), "% more",
                        "[variable] per plant than the most rural plants in 2019
                        along the South subtransect")),
              collapse = ' '))
  
}

Perc_change_subtransects_ggpredict(peds_subtr_01_pred)
# mean num. inflors at urban terminus in 2018: (2.711 + 7.451)/2 = 5.081
# mean num. inflors at rural terminus in 2018: (8.398 + 6.904)/2 = 7.651
# mean num. inflors at urban terminus in 2019: (3.733 + 4.236)/2 = 3.985
# mean num. inflors at rural terminus in 2019: (8.595 + 9.196)/2 = 8.896
# URBAN MEAN: (5.081 + 3.985) / 2 = 4.533
# RURAL MEAN: (7.651 + 8.896) / 2 = 8.274

# % diff: urban terminus had, on average, (4.533 - 8.274)/8.274 =
#         -0.452 = 45% fewer inflors than rural terminus

# % diff, urb:rural, 2018:
#                         urb mean = 5.081
#                         rur mean = 7.651
#                         there were (5.081 - 7.651)/7.651 = -0.3359 = 34% fewer
# inflors at urb terminus than rural terminus in 2018.

# % diff, urb:rural, 2019:
#                         urb mean = 3.985
#                         rur mean = 8.896
#                         there were (3.985 - 8.896)/8.896 = -0.552 = 55% fewer
# inflors at urb terminus than rural terminus in 2019.


Perc_change_subtransects_ggpredict(poll_subtr_01_pred)
# per flower: NORTH SUBTRANSECT
#             urban/2018: 0.35/5 = 0.07 (7%)
#             urban/2019: 0.956/5 = 0.191 (19%)
#             rural/2018: 2.02/5 = 0.404 (40%)
#             rural/2019: 0.541/5 = 0.108 (11%)

# per flower: SOUTH SUBTRANSECT
#             urban/2018: 1.122/5 = 0.224 (22%)
#             urban/2019: 0.872/5 = 0.174 (17%)
#             rural/2018: 1.7/5 = 0.34 (34%)
#             rural/2019: 0.68/5 = 0.136 (14%)



Perc_change_subtransects_ggpredict(pods_subtr_01_pred) # remember this is squared
# per plant: urban/2018: sqrt(150.314) = 12.26
#            urban/2019: sqrt(139.956) = 11.83
#            rural/2018: sqrt(121.149) = 11.01
#            rural/2019: sqrt(151.400) = 12.3
# 2018 MEAN: (12.26 + 11.01)/2 = 11.635
# 2019 MEAN: (11.83 + 12.3)/2 = 12.065
# 2019 had (12.065 - 11.635)/11.635 = 0.0359 = 4% more follicles than 2018


Perc_change_subtransects_ggpredict(podsperped_subtr_01_pred)
# mean num. inflors at urban terminus in 2018: (1.888 + 1.89)/2 = 1.889
# mean num. inflors at rural terminus in 2018: (1.265 + 1.267)/2 = 1.266
# mean num. inflors at urban terminus in 2019: (1.187 + 1.189)/2 = 1.188
# mean num. inflors at rural terminus in 2019: (1.968 + 1.969)/2 = 1.969
# URBAN MEAN: (1.889 + 1.888) / 2 = 1.889
# RURAL MEAN: (1.266 + 1.969) / 2 = 1.618

# % diff, urb:rural, 2018:
#                         there were (1.889 - 1.266)/1.266 = 49% more
# follicles per inflor at urb terminus than rural terminus in 2018.

# % diff, urb:rural, 2019:
#                         there were (1.188 - 1.969)/1.969 = 40% fewer
# follicles per inflor at urb terminus than rural terminus in 2019.




# Q1 / Gradient: Supplement-----
## Distance from City Center-----

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


## Urbanization Score-----
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

library(ggpubr)
Q1_regressions_supp.ggpredict <- ggarrange(ggpred_Q1.citydist_height,
                                           ggpred_Q1.citydist_height.u +
                                   font("x.text"),
                                 ncol = 2, nrow = 1, align = "h",
                                 labels = list("A", "B"),
                                 font.label = (size =16),
                                 common.legend = TRUE, 
                                 legend = "right")
Q1_regressions_supp.ggpredict

dev.copy2pdf(file="~/R_Projects/chapter_one/Figures_Tables/Q1_Gradient/Supplement/Gradient_regression_height_ggpredict.pdf", width = 8, height = 4)

## Find estimated marginal means at terminii-----
### Distance-----
Perc_change_gradient_ggpredict_byyear(height_gradient_01_pred)



# Q2 / Subtransects---------------------
## Distance from City Center----------

### Height
height_lmer_subtr_01
height_subtr_01_pred <- ggpredict(height_lmer_subtr_01,
                                terms = c("City_dist", "Year", "Transect_ID"),
                                type = "fe")



ggpred_Q2.citydist_height <- (ggplot(height_subtr_01_pred) +
                              geom_smooth(aes(x = x,
                                              y = predicted,
                                              color = interaction(group,facet),
                                              linetype = interaction(group,facet)),
                                          method = "loess",
                                          se = F) +
                              geom_ribbon(aes(x = x,
                                              ymin = predicted - std.error,
                                              ymax = predicted + std.error,
                                              fill = interaction(group,facet),
                                              linetype = interaction(group,facet)),
                                          alpha = 0.2)) + 
  geom_point(data = fertile_pops_all %>%
               filter(Transect_ID != "Rural"),
             aes(x = City_dist, y = Height_Sept,
                 colour = interaction(Year, Transect_ID),
                 shape = interaction(Year, Transect_ID)),
             size = 3) +
  scale_y_continuous(limits=c(0,200)) +
  labs(x = "Distance to Urban Center (km)",
       y = "Height (cm)",
       color = "Year, Subtransect",
       shape = "Year, Subtransect",
       fill = "Year, Subtransect",
       linetype = "Year, Subtransect") +
  scale_shape_manual(values = c(16,2,17,1),
                     labels = c("2018- Urban: Non-Corridor",
                                "2019- Urban: Non-Corridor",
                                "2018- Urban: Corridor",
                                "2019- Urban: Corridor")) +
  scale_color_manual(values = c("#00BFC4", "#00BFC4", "#F8766D", "#F8766D"),
                     labels = c("2018- Urban: Non-Corridor",
                                "2019- Urban: Non-Corridor",
                                "2018- Urban: Corridor",
                                "2019- Urban: Corridor")) +
  scale_fill_manual(values = c("#00BFC4", "#00BFC4", "#F8766D", "#F8766D"),
                    labels = c("2018- Urban: Non-Corridor",
                               "2019- Urban: Non-Corridor",
                               "2018- Urban: Corridor",
                               "2019- Urban: Corridor")) +
  scale_linetype_manual(values = c('solid', 'dashed', 'solid', 'dashed'),
                        labels = c("2018- Urban: Non-Corridor",
                                   "2019- Urban: Non-Corridor",
                                   "2018- Urban: Corridor",
                                   "2019- Urban: Corridor")) +
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
ggpred_Q2.citydist_height


## Urbanization Score----------

### Height
u_height_lmer_subtr_01
height_subtr_01_pred.u <- ggpredict(u_height_lmer_subtr_01,
                                  terms = c("Urb_score", "Year", "Transect_ID"),
                                  type = "fe")



ggpred_Q2.citydist_height.u <- (ggplot(height_subtr_01_pred.u) +
                                geom_smooth(aes(x = x,
                                                y = predicted,
                                                color = interaction(group,facet),
                                                linetype = interaction(group,facet)),
                                            method = "loess",
                                            se = F) +
                                geom_ribbon(aes(x = x,
                                                ymin = predicted - std.error,
                                                ymax = predicted + std.error,
                                                fill = interaction(group,facet),
                                                linetype = interaction(group,facet)),
                                            alpha = 0.2)) + 
  geom_point(data = fertile_pops_all %>%
               filter(Transect_ID != "Rural"),
             aes(x = Urb_score, y = Height_Sept,
                 colour = interaction(Year, Transect_ID),
                 shape = interaction(Year, Transect_ID)),
             size = 3) +
  scale_x_reverse() +
  xlim(3.5, -2) +
  scale_y_continuous(limits=c(0,200)) +
  labs(x = "Urbanization Score",
       y = "Height (cm)",
       color = "Year, Subtransect",
       shape = "Year, Subtransect",
       fill = "Year, Subtransect",
       linetype = "Year, Subtransect") +
  scale_shape_manual(values = c(16,2,17,1),
                     labels = c("2018- Urban: Non-Corridor",
                                "2019- Urban: Non-Corridor",
                                "2018- Urban: Corridor",
                                "2019- Urban: Corridor")) +
  scale_color_manual(values = c("#00BFC4", "#00BFC4", "#F8766D", "#F8766D"),
                     labels = c("2018- Urban: Non-Corridor",
                                "2019- Urban: Non-Corridor",
                                "2018- Urban: Corridor",
                                "2019- Urban: Corridor")) +
  scale_fill_manual(values = c("#00BFC4", "#00BFC4", "#F8766D", "#F8766D"),
                    labels = c("2018- Urban: Non-Corridor",
                               "2019- Urban: Non-Corridor",
                               "2018- Urban: Corridor",
                               "2019- Urban: Corridor")) +
  scale_linetype_manual(values = c('solid', 'dashed', 'solid', 'dashed'),
                        labels = c("2018- Urban: Non-Corridor",
                                   "2019- Urban: Non-Corridor",
                                   "2018- Urban: Corridor",
                                   "2019- Urban: Corridor")) +
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
ggpred_Q2.citydist_height.u




library(ggpubr)
Q2_regressions_supp.ggpredict <- ggarrange(ggpred_Q2.citydist_height,
                                           ggpred_Q2.citydist_height.u +
                                             font("x.text"),
                                           ncol = 2, nrow = 1, align = "h",
                                           labels = list("A", "B"),
                                           font.label = (size =16),
                                           common.legend = T, 
                                           legend = "bottom")
Q2_regressions_supp.ggpredict

dev.copy2pdf(file="~/R_Projects/chapter_one/Figures_Tables/Q2_UrbanSubtransects/Supplement/Subtransect_regression_height_ggpredict.pdf",
             width = 11, height = 5)
