####GRAPHS OF WALKING INCIDENCE####
ggplot(bhvr,
       aes(y = i_walks,
           x = variety,
           fill = sex)) +
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(fill = NULL) +
  ggtitle("Walking Frequency of Potato Psyllids on Four Potato Genotypes") +
  ylab("Number of Walks") +
  xlab("Genotype") +
  scale_fill_viridis_d(begin = 0.95,
                       end = 0.55,
                       option = "E") +
  coord_cartesian(ylim = c(0, 10))

#saving the plot as figure 5
ggsave(
  "fig_5.jpg",
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



####GRAPHS OF WALKING DURATION####
ggplot(bhvr,
       aes(y = d_walks,
           x = variety,
           fill = sex)) +
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(fill = NULL) +
  ggtitle("Walking Duration (s) of Potato Psyllids on Four Potato Genotypes") +
  ylab("Walking Duration (s)") +
  xlab("Genotype") +
  scale_fill_viridis_d(begin = 0.95,
                       end = 0.55,
                       option = "E") +
  coord_cartesian(ylim = c(0, 300))

#saving the plot as figure 6
ggsave(
  "fig_6.jpg",
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
