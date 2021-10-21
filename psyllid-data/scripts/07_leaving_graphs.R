####GRAPHS OF LEAVINGING INCIDENCE####
ggplot(bhvr,
       aes(y = i_off,
           x = variety,
           fill = sex)) +
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(fill = NULL) +
  ggtitle("Off-Leaf Frequency of Potato Psyllids on Four Potato Genotypes") +
  ylab("Times off-leaf") +
  xlab("Genotype") +
  scale_fill_viridis_d(begin = 0.95,
                       end = 0.55,
                       option = "E") +
  coord_cartesian(ylim = c(0, 10))

#saving the plot as figure 9
ggsave(
  "fig_9.jpg",
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



####GRAPHS OF LEAVINGING DURATION####
ggplot(bhvr,
       aes(y = d_off,
           x = variety,
           fill = sex)) +
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(fill = NULL) +
  ggtitle("Off-Leaf Duration (s) of Potato Psyllids on Four Potato Genotypes") +
  ylab("Off-Leaf Duration (s)") +
  xlab("Genotype") +
  scale_fill_viridis_d(begin = 0.95,
                       end = 0.55,
                       option = "E") +
  coord_cartesian(ylim = c(0, 300))

#saving the plot as figure 10
ggsave(
  "fig_10.jpg",
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


#cleaning up the table for the final save
bhvr <- rename(
  bhvr,
  `Observation Filename` = file,
  Psyllid = psyllid,
  Sex = sex,
  `Sex on Variety Interaction` = sex_var,
  Genotype = variety,
  `Probe Incidence` = i_probes,
  `Probe Duration (s)` = d_probes,
  `Walking Incidence` = i_walks,
  `Walking Duration (s)` = d_walks,
  `Cleaning Incidence` = i_cleans,
  `Cleaning Duration (s)` = d_cleans,
  `Leaf-leaving Incidence` = i_off,
  `Leaf-Leaving Duration (s)` = d_off,
  `Plant Number` = plant_number,
  `Plant size (cm)` = plant_size,
  `Time from Initial Collection (mins)` = mins_away
)

#reordering the variables
bhvr <-
  select(
    bhvr,
    `Observation Filename`,
    Psyllid,
    Sex,
    `Sex on Variety Interaction`,
    Genotype,
    `Probe Incidence`,
    `Probe Duration (s)`,
    `Walking Incidence`,
    `Walking Duration (s)`,
    `Cleaning Incidence`,
    `Cleaning Duration (s)`,
    `Leaf-leaving Incidence`,
    `Leaf-Leaving Duration (s)`,
    `Plant Number`,
    `Plant size (cm)`,
    `Time from Initial Collection (mins)`
  )
write_xlsx(bhvr, 'results/Psyllid Behaviors on Four Potato Genotypes.xlsx')

#cleaning up the workspace
rm(list = ls())

#removing the bhvr files, they are redundant
file.remove(
  "data/bhvr_10LB.xlsx",
  "data/bhvr_3LB.xlsx",
  "data/bhvr_4LB.xlsx",
  "data/bhvr_RB.xlsx"
)
