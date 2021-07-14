# Q1 / Gradient---------------------
## Distance from City Center----------

### Peduncles
peds_glmer_gradient_01
peds_gradient_01_pred <- ggpredict(peds_glmer_gradient_01,
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
    geom_smooth(aes(x = x, y = predicted, color = group)) +
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
  geom_smooth(aes(x = x, y = predicted, color = group)) +
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
    geom_smooth(aes(x = x, y = predicted, color = group)) +
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
    geom_smooth(aes(x = x, y = predicted, color = group)) +
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
                              geom_smooth(aes(x = x, y = predicted, color = group)) +
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
                              geom_smooth(aes(x = x, y = predicted, color = group)) +
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
                              geom_smooth(aes(x = x, y = predicted, color = group)) +
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
                                    geom_smooth(aes(x = x, y = predicted, color = group)) +
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


## Find mean values at terminii
### Distance
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
  
  
  avg_diff_2019 <- abs( urban_estimate_2019 - rural_estimate_2019 )  /
    (0.5* (    urban_estimate_2019 + rural_estimate_2019  )  ) # %T>%
  # print()
  
  # % CHANGE
  percent_change_2019 <- (((rural_estimate_2019 - urban_estimate_2019) /
                             abs(rural_estimate_2019)) * 100) # %T>%
  # print()
  
  
  
  
  print(paste(noquote(c("On average, the most rural plants produced",
                        round(percent_change_2018, 1), "% more",
                        "[variable] per plant than the most urban plants in 2018")),
              collapse = ' '))
  
  print(paste(noquote(c("On average, the most rural plants produced",
                        round(percent_change_2019, 1), "% more",
                        "[variable] per plant than the most urban plants in 2019")),
              collapse = ' '))
  
}
Perc_change_gradient_ggpredict(peds_gradient_01_pred)
Perc_change_gradient_ggpredict(poll_gradient_01_pred)
Perc_change_gradient_ggpredict(pods_gradient_01_pred)
Perc_change_gradient_ggpredict(podsperped_gradient_01_pred)

### Urbanization Score