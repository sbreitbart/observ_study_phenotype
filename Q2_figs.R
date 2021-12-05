# *****distance-----
# PEDUNCLES-----
(ggplot(peds_subtr_01_pred) +
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


# POLLINIA-----
## factors (transects) for this object are North, then South
## for some reason (peduncles and at least one more is South, North)
## so to keep legend consistent, have to reorder levels first
poll_subtr_01_pred$facet <- factor(poll_subtr_01_pred$facet,
                                        levels(poll_subtr_01_pred$facet)[c(2,1)])
levels(poll_subtr_01_pred$facet)

(ggplot(poll_subtr_01_pred) +
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

# PODS-----
(ggplot(pods_subtr_01_pred) +
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

# PODS per PED-----
## factors (transects) for this object are North, then South
## so to keep legend consistent, have to reorder levels first
podsperped_subtr_01_pred$facet <- factor(podsperped_subtr_01_pred$facet,
                                   levels(podsperped_subtr_01_pred$facet)[c(2,1)])
levels(podsperped_subtr_01_pred$facet)

(ggplot(podsperped_subtr_01_pred) +
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




# HEIGHT-----
## factors (transects) for this object are North, then South
## so to keep legend consistent, have to reorder levels first
height_subtr_01_pred$facet <- factor(height_subtr_01_pred$facet,
                                         levels(height_subtr_01_pred$facet)[c(2,1)])
levels(height_subtr_01_pred$facet)

(ggplot(height_subtr_01_pred) +
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

# *****urbanization score-----
# PEDUNCLES-----
(ggplot(peds_subtr_01_pred.u) +
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

# POLLINIA-----
## factors (transects) for this object are North, then South
## for some reason (peduncles and at least one more is South, North)
## so to keep legend consistent, have to reorder levels first
poll_subtr_01_pred.u$facet <- factor(poll_subtr_01_pred.u$facet,
                                   levels(poll_subtr_01_pred.u$facet)[c(2,1)])
levels(poll_subtr_01_pred.u$facet)

(ggplot(poll_subtr_01_pred.u) +
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

# PODS-----
(ggplot(pods_subtr_01_pred.u) +
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

# PODS PER PED-----
## factors (transects) for this object are North, then South
## so to keep legend consistent, have to reorder levels first
podsperped_subtr_01_pred.u$facet <- factor(podsperped_subtr_01_pred.u$facet,
                                         levels(podsperped_subtr_01_pred.u$facet)[c(2,1)])
levels(podsperped_subtr_01_pred.u$facet)

(ggplot(podsperped_subtr_01_pred.u) +
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

# HEIGHT
## factors (transects) for this object are North, then South
## so to keep legend consistent, have to reorder levels first
height_subtr_01_pred.u$facet <- factor(height_subtr_01_pred.u$facet,
                                     levels(height_subtr_01_pred.u$facet)[c(2,1)])
levels(height_subtr_01_pred.u$facet)

(ggplot(height_subtr_01_pred.u) +
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

