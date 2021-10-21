####GRAPHS OF PROBING INCIDENCE####
#getting data from model for the graphs
bhvr$sex <- gsub('m', 'Male', bhvr$sex)
bhvr$sex <- gsub('f', 'Female', bhvr$sex)

#fixing genotype names
bhvr$variety <- gsub('10LB', '10LB', bhvr$variety)
bhvr$variety <- gsub('3LB', '3LB', bhvr$variety)
bhvr$variety <- gsub('4LB', '4LB', bhvr$variety)
bhvr$variety <- gsub('RB', 'Russet Burbank', bhvr$variety)


#graph of probing incidence
ggplot(bhvr,
       aes(y = i_probes,
           x = variety,
           fill = sex)) +
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(fill = NULL) +
  ggtitle("Probing Frequency of Potato Psyllids on Four Potato Genotypes") +
  ylab("Number of Probes") +
  xlab("Genotype") +
  scale_fill_viridis_d(begin = 0.95,
                       end = 0.55,
                       option = "E") +
  coord_cartesian(ylim = c(0, 10))

#saving the plot as figure 3
ggsave(
  "fig_3.jpg",
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



####GRAPHS OF PROBING DURATION####
ggplot(bhvr,
       aes(y = d_probes,
           x = variety,
           fill = sex)) +
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(fill = NULL) +
  ggtitle("Probing Duration (s) of Potato Psyllids on Four Potato Genotypes") +
  ylab("Probing Duration (s)") +
  xlab("Genotype") +
  scale_fill_viridis_d(begin = 0.95,
                       end = 0.55,
                       option = "E") +
  coord_cartesian(ylim = c(0, 300))

#saving the plot as figure 4
ggsave(
  "fig_4.jpg",
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
