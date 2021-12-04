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
  # labs(x = "Distance to Urban Center (km)",
  #      y = "Inflorescences",
  #      color = "Year, Subtransect",
  #      shape = "Year, Subtransect",
  #      fill = "Year, Subtransect",
  #      linetype = "Year, Subtransect") +
  scale_shape_manual(values = c(16,2,17,1) #,
                     # labels = c("2018- Urban: Non-Corridor",
                     #            "2019- Urban: Non-Corridor",
                     #            "2018- Urban: Corridor",
                     #            "2019- Urban: Corridor")
                     ) +
  # scale_color_manual(values = c("#00BFC4", "#00BFC4", "#F8766D", "#F8766D"),
  #                    labels = c("2018- Urban: Non-Corridor",
  #                               "2019- Urban: Non-Corridor",
  #                               "2018- Urban: Corridor",
  #                               "2019- Urban: Corridor")) +
  # scale_fill_manual(values = c("#00BFC4", "#00BFC4", "#F8766D", "#F8766D"),
  #                   labels = c("2018- Urban: Non-Corridor",
  #                              "2019- Urban: Non-Corridor",
  #                              "2018- Urban: Corridor",
  #                              "2019- Urban: Corridor")) +
  # scale_linetype_manual(values = c('solid', 'dashed', 'solid', 'dashed'),
  #                       labels = c("2018- Urban: Non-Corridor",
  #                                  "2019- Urban: Non-Corridor",
  #                                  "2018- Urban: Corridor",
  #                                  "2019- Urban: Corridor")) +
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
