####FECUNDITY GRAPHS####
#making table names nicer for graphs
fcnd <-
  rename(
    fcnd,
    Psyllid = psyllid,
    Genotype = germ,
    Period = period,
    Eggs = eggs,
    Nymphs = nymphs,
    `Hatch %` = hatch,
    Block = block,
    Letter = letter
  )
#adding 'Period' to the fcnd data for better labelling
fcnd$Period <- paste0('Period', sep = " ", fcnd$Period)

#making the ratio into percentages
fcnd$`Hatch %` <- fcnd$`Hatch %` * 100

#fixing genotype names
fcnd$Genotype <- gsub("10LB", "10 LB", fcnd$Genotype)
fcnd$Genotype <- gsub("3LB", "3 LB", fcnd$Genotype)
fcnd$Genotype <- gsub("4LB", "4 LB", fcnd$Genotype)
fcnd$Genotype <- gsub("RB", "Russet Burbank", fcnd$Genotype)


####GRAPH OF EGG PRODUCTION####
ggplot(fcnd,
       aes(x = Period, y = Eggs, fill = Genotype)) +
  geom_boxplot() +
  facet_wrap( ~ Genotype) +
  theme_minimal() +
  theme(legend.position = 'none', axis.title.x = element_blank()) +
  labs(fill = NULL) +  ggtitle("Psyllid Egg Production Per Period") +
  xlab("Number of Eggs") +
  scale_fill_viridis_d(option = "E",
                       direction = -1)

ggsave(
  "fig_11.jpg",
  plot = last_plot(),
  device = "jpg",
  width = 6,
  height = 5,
  units = "in",
  scale = 1,
  dpi = 300,
  limitsize = TRUE,
  path = "figures"
)



####GRAPH OF NYMPH DATA####
ggplot(fcnd,
       aes(x = Period, y = Nymphs, fill = Genotype)) +
  geom_boxplot() +
  facet_wrap( ~ Genotype) +
  theme_minimal() +
  theme(legend.position = 'none', axis.title.x = element_blank()) +
  labs(fill = NULL) +  ggtitle("Psyllid Nymph Production Per Period") +
  xlab("Number of Nymphs") +
  scale_fill_viridis_d(option = "E",
                       direction = -1)

ggsave(
  "fig_12.jpg",
  plot = last_plot(),
  device = "jpg",
  width = 6,
  height = 5,
  units = "in",
  scale = 1,
  dpi = 300,
  limitsize = TRUE,
  path = "figures"
)


####GRAPH OF HATCH DATA####
ggplot(fcnd,
       aes(x = Period, y = `Hatch %`, fill = Genotype)) +
  geom_boxplot() +
  facet_wrap( ~ Genotype) +
  theme_minimal() +
  theme(legend.position = 'none', axis.title.x = element_blank()) +
  labs(fill = NULL) +  ggtitle("Psyllid Egg Fertility Per Period") +
  xlab("Percent Nymphs Hatched") +
  scale_fill_viridis_d(option = "E",
                       direction = -1)
ggsave(
  "fig_13.jpg",
  plot = last_plot(),
  device = "jpg",
  width = 6,
  height = 5,
  units = "in",
  scale = 1,
  dpi = 300,
  limitsize = TRUE,
  path = "figures"
)

writexl::write_xlsx(fcnd, ('results/Psyllid Fecundity on Four Potato Genotypes.xlsx'))
