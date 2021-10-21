####GRAPHS OF CLEANING INCIDENCE####
ggplot(bhvr,
       aes(y = i_cleans,
           x = variety,
           fill = sex)) +
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(fill = NULL) +
  ggtitle("Cleaning Frequency of Potato Psyllids on Four Potato Genotypes") +
  ylab("Number of Cleanings") +
  xlab("Genotype") +
  scale_fill_viridis_d(begin = 0.95,
                       end = 0.55,
                       option = "E") +
  coord_cartesian(ylim = c(0, 10))

#saving the plot as figure 7
ggsave(
  "fig_7.jpg",
  plot = last_plot(),
  device = "jpg",
  width = 6,
  height = 4.5,
  units = "in",
  scale = 1,
  dpi = 300,
  limitsize = TRUE,
  path = "figures"
)



####GRAPHS OF CLEANING DURATION####
ggplot(bhvr,
       aes(y = d_cleans,
           x = variety,
           fill = sex)) +
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(fill = NULL) +
  ggtitle("Cleaning Duration (s) of Potato Psyllids on Four Potato Genotypes") +
  ylab("Cleaning Duration (s)") +
  xlab("Genotype") +
  scale_fill_viridis_d(begin = 0.95,
                       end = 0.55,
                       option = "E") +
  coord_cartesian(ylim = c(0, 300))

#saving the plot as figure 8
ggsave(
  "fig_8.jpg",
  plot = last_plot(),
  device = "jpg",
  width = 6,
  height = 4.5,
  units = "in",
  scale = 1,
  dpi = 300,
  limitsize = TRUE,
  path = "figures"
)
