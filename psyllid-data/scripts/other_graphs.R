####OTHER GRAPHS####
packages_list <-
  c("devtools",
    "lme4",
    "pwr",
    "writexl",
    "pastecs",
    "optimx",
    "car",
    "fitdistrplus",
    "emmeans",
    "multcompView",
    "viridis",
    "tidyverse",
    "ggridges",
    "egg"
  )

#uncomment and run to install required  packages before running the first time
# install.packages("devtools")
# library(devtools)
# install.packages("lme4")
# install.packages("pwr")
# install.packages("writexl")
# install.packages("pastecs")
# install.packages("optimx")
# install.packages("car")
# install.packages("fitdistrplus")
# install.packages("emmeans")
# install.packages("multcompView")
# install.packages("viridis")
# install.packages("tidyverse")
# install.packages("ggridges")
# install_github("baptiste/egg")

#Loads the required packages for this analysis
lapply(packages_list, library, character.only = TRUE)

#removes the files in parentheses
rm(packages_list)


#reading in datasets
bhvr<-read_csv('results/bhvr.csv')
fcnd<-read_csv('results/fcnd.csv')
fcnd_hatch <- readxl::read_xlsx('results/fcnd_hatch.xlsx')


#graph of probing incidence
i_probes <-
  ggplot(bhvr,
         aes(
           x = i_probes,
           y = sex,
           fill = 0.5 - abs(0.5 - ..ecdf..)
         )) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = T,
    rel_min_height = 0.001,
    scale = 0.9,
    quantiles = c(0.025, 0.975)
  ) +
  facet_wrap(~ variety, nrow = 2) +
  theme_minimal() +
  theme(axis.text.y = element_text(vjust = 0),
        legend.position = "bottom") +
  labs(fill = NULL) +
  ggtitle("Probing Frequency") +
  xlab("Number of Probes") +
  ylab("Germplasm") +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_fill_viridis(name = "Tail Probability",
                     option = "E",
                     direction = -1) +
  coord_cartesian(xlim = c(0, 12))
i_probes <-
  egg::tag_facet(
    i_probes,
    x = 4,
    open = "",
    close = "",
    tag_pool = c("10LB a  ", "3LB a  ", "4LB ab ", " RB a  ")
  )
i_probes <-
  egg::tag_facet(
    i_probes,
    x = 4,
    y = 2,
    open = "",
    close = "",
    tag_pool = c("10LB a  ", "3LB a  ", "4LB a  ", " RB b  ")
  )

#saving the plot as figure 3
i_probes
ggsave(
  "i_probes_ridge.jpg",
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
d_probes <-
  ggplot(bhvr,
         aes(
           x = d_probes,
           y = sex,
           fill = 0.5 - abs(0.5 - ..ecdf..)
         )) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = T,
    rel_min_height = 0.001,
    scale = 0.9,
    quantiles = c(0.025, 0.975)
  ) +
  facet_wrap(~ variety, nrow = 2) +
  theme_minimal() +
  theme(axis.text.y = element_text(vjust = 0),
        legend.position = "bottom") +
  labs(fill = NULL) +
  ggtitle("Probing Duration") +
  xlab("Duration (s)") +
  ylab("Germplasm") +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_fill_viridis(name = "Tail Probability",
                     option = "E",
                     direction = -1) +
  coord_cartesian(xlim = c(0, 350))
d_probes <-
  egg::tag_facet(
    d_probes,
    x = 125,
    open = "",
    close = "",
    tag_pool = c("10LB a  ", "3LB a  ", "4LB a  ", " RB a  ")
  )
d_probes <-
  egg::tag_facet(
    d_probes,
    x = 125,
    y = 2,
    open = "",
    close = "",
    tag_pool = c("10LB a  ", "3LB a  ", "4LB a  ", " RB a  ")
  )

#saving the plot as figure 4
d_probes
ggsave(
  "d_probes_ridge.jpg",
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


####GRAPHS OF WALKING INCIDENCE####
i_walks <-
  ggplot(bhvr,
         aes(
           x = i_walks,
           y = sex,
           fill = 0.5 - abs(0.5 - ..ecdf..)
         )) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = T,
    rel_min_height = 0.001,
    scale = 0.9,
    quantiles = c(0.025, 0.975)
  ) +
  facet_wrap(~ variety, ncol = 2) +
  theme_minimal() +
  theme(axis.text.y = element_text(vjust = 0),
        legend.position = "bottom") +
  labs(fill = NULL) +
  ggtitle("Walking Frequency") +
  xlab("Number of Walks") +
  ylab("Germplasm") +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_fill_viridis(name = "Tail Probability",
                     option = "E",
                     direction = -1) +
  coord_cartesian(xlim = c(0, 12))
i_walks <-
  egg::tag_facet(
    i_walks,
    x = 4,
    open = "",
    close = "",
    tag_pool = c("10LB a  ", "3LB ab ", "4LB ab ", " RB ab ")
  )
i_walks <-
  egg::tag_facet(
    i_walks,
    x = 4,
    y = 2,
    open = "",
    close = "",
    tag_pool = c("10LB a  ", "3LB a  ", "4LB ab ", " RB b  ")
  )

#saving the plot as figure 5
i_walks
ggsave(
  "i_walks_ridge.jpg",
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
d_walks <-
  ggplot(bhvr,
         aes(
           x = d_walks,
           y = sex,
           fill = 0.5 - abs(0.5 - ..ecdf..)
         )) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = T,
    rel_min_height = 0.001,
    scale = 0.9,
    quantiles = c(0.025, 0.975)
  ) +
  facet_wrap(~ variety, nrow = 2) +
  theme_minimal() +
  theme(axis.text.y = element_text(vjust = 0),
        legend.position = "bottom") +
  labs(fill = NULL) +
  ggtitle("Walking Duration") +
  xlab("Duration (s)") +
  ylab("Germplasm") +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_fill_viridis(name = "Tail Probability",
                     option = "E",
                     direction = -1) +
  coord_cartesian(xlim = c(0, 350))

d_walks <-
  egg::tag_facet(
    d_walks,
    x = 125,
    open = "",
    close = "",
    tag_pool = c("10LB a  ", "3LB a  ", "4LB a  ", " RB a  ")
  )
d_walks <-
  egg::tag_facet(
    d_walks,
    x = 125,
    y = 2,
    open = "",
    close = "",
    tag_pool = c("10LB ab ", "3LB a  ", "4LB ab ", " RB b  ")
  )


#saving the plot as figure 6
d_walks
ggsave(
  "d_walks_ridge.jpg",
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
####GRAPHS OF CLEANING INCIDENCE####
i_cleans <-
  ggplot(bhvr,
         aes(
           x = i_cleans,
           y = sex,
           fill = 0.5 - abs(0.5 - ..ecdf..)
         )) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = T,
    rel_min_height = 0.001,
    scale = 0.9,
    quantiles = c(0.025, 0.975)
  ) +
  facet_wrap(~ variety, nrow = 2) +
  theme_minimal() +
  theme(axis.text.y = element_text(vjust = 0),
        legend.position = "bottom") +
  labs(fill = NULL) +
  ggtitle("Cleaning Frequency") +
  xlab("Number of Cleanings") +
  ylab("Germplasm") +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_fill_viridis(name = "Tail Probability",
                     option = "E",
                     direction = -1) +
  coord_cartesian(xlim = c(0, 12))
i_cleans <-
  egg::tag_facet(
    i_cleans,
    x = 4,
    open = "",
    close = "",
    tag_pool = c("10LB a  ", "3LB a  ", "4LB a  ", " RB a  ")
  )
i_cleans <-
  egg::tag_facet(
    i_cleans,
    x = 4,
    y = 2,
    open = "",
    close = "",
    tag_pool = c("10LB a  ", "3LB a  ", "4LB a  ", " RB a  ")
  )

#saving the plot as figure 7
i_cleans
ggsave(
  "i_cleans_ridge.jpg",
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
d_cleans <-
  ggplot(bhvr,
         aes(
           x = d_cleans,
           y = sex,
           fill = 0.5 - abs(0.5 - ..ecdf..)
         )) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = T,
    rel_min_height = 0.001,
    scale = 0.9,
    quantiles = c(0.025, 0.975)
  ) +
  facet_wrap(~ variety, nrow = 2) +
  theme_minimal() +
  theme(axis.text.y = element_text(vjust = 0),
        legend.position = "bottom") +
  labs(fill = NULL) +
  ggtitle("Cleaning Duration") +
  xlab("Duration (s)") +
  ylab("Germplasm") +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_fill_viridis(name = "Tail Probability",
                     option = "E",
                     direction = -1) +
  coord_cartesian(xlim = c(0, 350))
d_cleans <-
  egg::tag_facet(
    d_cleans,
    x = 125,
    open = "",
    close = "",
    tag_pool = c("10LB a  ", "3LB a  ", "4LB a  ", " RB a  ")
  )
d_cleans <-
  egg::tag_facet(
    d_cleans,
    x = 125,
    y = 2,
    open = "",
    close = "",
    tag_pool = c("10LB a  ", "3LB a  ", "4LB a  ", " RB a  ")
  )

#saving the plot as figure 8
d_cleans
ggsave(
  "d_cleans_ridge.jpg",
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
####GRAPHS OF LEAF-LEAVING INCIDENCE####
#female leaf-leafing incidence graph
i_off <-
  ggplot(bhvr,
         aes(
           x = i_off,
           y = sex,
           fill = 0.5 - abs(0.5 - ..ecdf..)
         )) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = T,
    rel_min_height = 0.001,
    scale = 0.9,
    quantiles = c(0.025, 0.975)
  ) +
  facet_wrap( ~ variety, nrow = 2) +
  theme_minimal() +
  theme(axis.text.y = element_text(vjust = 0),
        legend.position = "bottom") +
  labs(fill = NULL) +
  ggtitle("Leaf-leaving Frequency") +
  xlab("Number of Leaf-leavings") +
  ylab("Germplasm") +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_fill_viridis(name = "Tail Probability",
                     option = "E",
                     direction = -1) +
  coord_cartesian(xlim = c(0, 12))
i_off <-
  egg::tag_facet(
    i_off,
    x = 4,
    open = "",
    close = "",
    tag_pool = c("10LB a  ", "3LB a  ", "4LB a  ", " RB a  ")
  )
i_off <-
  egg::tag_facet(
    i_off,
    x = 4,
    y = 2,
    open = "",
    close = "",
    tag_pool = c("10LB a  ", "3LB a  ", "4LB a  ", " RB a  ")
  )

#saving the plot as figure 9
i_off
ggsave(
  "i_off_ridge.jpg",
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

####GRAPHS OF LEAF-LEAVING DURATION####
d_off <-
  ggplot(bhvr,
         aes(
           x = d_off,
           y = sex,
           fill = 0.5 - abs(0.5 - ..ecdf..)
         )) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = T,
    rel_min_height = 0.001,
    scale = 0.9,
    quantiles = c(0.025, 0.975)
  ) +
  facet_wrap(~ variety, nrow = 2) +
  theme_minimal() +
  theme(axis.text.y = element_text(vjust = 0),
        legend.position = "bottom") +
  labs(fill = NULL) +
  ggtitle("Leaf-leaving Duration") +
  xlab("Duration (s)") +
  ylab("Germplasm") +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_fill_viridis(name = "Tail Probability",
                     option = "E",
                     direction = -1) +
  coord_cartesian(xlim = c(0, 350))
d_off <-
  egg::tag_facet(
    d_off,
    x = 125,
    open = "",
    close = "",
    tag_pool = c("10LB a  ", "3LB a  ", "4LB a  ", " RB a  ")
  )
d_off <-
  egg::tag_facet(
    d_off,
    x = 125,
    y = 2,
    open = "",
    close = "",
    tag_pool = c("10LB a  ", "3LB a  ", "4LB a  ", " RB a  ")
  )

#saving the plot as figure 10
d_off
ggsave(
  "d_off_ridge.jpg",
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

#extra egg graph based on pre-summed values
egg_grph_1 <-
  ggplot(fcnd_hatch,
         aes(
           x = eggs,
           y = germ,
           fill = 0.5 - abs(0.5 - ..ecdf..)
         )) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = T,
    rel_min_height = 0.001,
    scale = 0.9,
    quantiles = c(0.025, 0.975)
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(vjust = 0),
        legend.position = "bottom") +
  labs(fill = NULL) +
  ggtitle("Total Psyllid Egg Production on Four Germplasms") +
  xlab("Number of Eggs") +
  ylab("Germplasm") +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_fill_viridis(name = "Tail Probability",
                     option = "E",
                     direction = -1)
egg_grph_1
ggsave(
  "x_egg_graph_1.jpg",
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


#extra nymph graph based on pre-summed values
nym_grph_1 <-
  ggplot(fcnd_hatch,
         aes(
           x = nymphs,
           y = germ,
           fill = 0.5 - abs(0.5 - ..ecdf..)
         )) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = T,
    rel_min_height = 0.001,
    scale = 0.9,
    quantiles = c(0.025, 0.975)
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(vjust = 0),
        legend.position = "bottom") +
  labs(fill = NULL) +
  ggtitle("Total Nymph Production on Four Germplasms") +
  xlab("Number of Nymphs") +
  ylab("Germplasm") +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_fill_viridis(name = "Tail Probability",
                     option = "E",
                     direction = -1)
nym_grph_1

ggsave(
  "x_nymph_graph_1.jpg",
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

#extra egg violin graph
egg_graph_2 <-
  ggplot(fcnd,
         aes(fill = germ,
             x = germ,
             y = eggs)) +
  geom_violin() +
  facet_wrap( ~ period, nrow = 2, ncol = 2) +
  theme_bw() +
  ggtitle("Number of Eggs Per Period on Four Germplasms") +
  ylab("Number of Eggs") +
  scale_fill_viridis(
    begin = 1,
    end = 0,
    option = "E",
    discrete = T
  ) +
  theme(legend.position = 'bottom')

egg_graph_2

ggsave(
  "x_egg_graph_2.jpg",
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

#extra nymph violin graph
nymph_graph_2 <-
  ggplot(fcnd,
         aes(fill = germ,
             x = germ,
             y = nymphs)) +
  geom_violin() +
  facet_wrap( ~ period, nrow = 2, ncol = 2) +
  theme_bw() +
  ggtitle("Number of Nymphs Per Period on Four Germplasms") +
  ylab("Number of Nymphs") +
  scale_fill_viridis(
    begin = 1,
    end = 0,
    option = "E",
    discrete = T
  ) +
  theme(legend.position = 'bottom')

nymph_graph_2

ggsave(
  "x_nymph_graph_2.jpg",
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

#imports table data
fcnd_grph <- readxl::read_excel("results/fcnd_table.xlsx")
#full germplasm names
fcnd_grph$Germplasm <-
  gsub('10LB', 'A07781-10LB', fcnd_grph$Germplasm)
fcnd_grph$Germplasm <-
  gsub('3LB', 'A07781-3LB', fcnd_grph$Germplasm)
fcnd_grph$Germplasm <-
  gsub('4LB', 'A07781-4LB', fcnd_grph$Germplasm)
fcnd_grph$Germplasm <-
  gsub('RB', 'Russet Burbank', fcnd_grph$Germplasm)

#making a graph
egg_grph <-
  ggplot(
    fcnd_grph,
    aes(
      color = Germplasm,
      group = Germplasm,
      x = Period,
      y = Egg_Mean,
      ymin = Egg_Mean - Egg_SEM,
      ymax = Egg_Mean + Egg_SEM
    )
  ) +
  geom_line(aes(group = Germplasm), size = 1.5) +
  geom_point(size = 4) +
  theme_bw() +
  xlab("Plant Transfer") +
  ylab("Mean Number of Eggs") +
  scale_color_viridis(
    begin = 1,
    end = 0,
    option = "E",
    discrete = T
  ) +
  scale_x_discrete(labels = c("0", "1", "2", "3")) +
  coord_cartesian(ylim = c(0, 25)) +
  geom_errorbar(
    aes(
      x = Period,
      ymin = Egg_Mean - Egg_SEM,
      ymax = Egg_Mean + Egg_SEM
    ),
    size = 0.5,
    position = position_dodge(0.2)
  ) +
  theme(legend.position = "none") +
  facet_wrap(~Germplasm)

egg_grph
ggsave(
  "x_egg_graph_3.jpg",
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
nym_grph <-
  ggplot(
    fcnd_grph,
    aes(
      color = Germplasm,
      group = Germplasm,
      x = Period,
      y = Nymph_Mean,
      ymin = Nymph_Mean - Nymph_SEM,
      ymax = Nymph_Mean + Nymph_SEM
    )
  ) +
  geom_line(aes(group = Germplasm), size = 1.5) +
  geom_point(size = 4) +
  theme_bw() +
  xlab("Plant Transfer") +
  ylab("Mean Number of Nymphs") +
  scale_color_viridis(
    begin = 1,
    end = 0,
    option = "E",
    discrete = T
  ) +
  scale_x_discrete(labels = c("0", "1", "2", "3")) +
  coord_cartesian(ylim = c(0, 25)) +
  geom_errorbar(
    aes(
      x = Period,
      ymin = Nymph_Mean - Nymph_SEM,
      ymax = Nymph_Mean + Nymph_SEM
    ),
    size = 0.5,
    position = position_dodge(0.2)
  ) +
  theme(legend.position = "none")+
  facet_wrap(~Germplasm)

nym_grph
ggsave(
  "x_nymph_graph_3.jpg",
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
hatch_grph <-
  ggplot(
    fcnd_grph,
    aes(
      color = Germplasm,
      group = Germplasm,
      x = Period,
      y = Hatch_Percent,
      ymin = Hatch_Percent - Hatch_SEM,
      ymax = Hatch_Percent + Hatch_SEM
    )
  ) +
  geom_line(aes(group = Germplasm), size = 1.5) +
  geom_point(size = 4) +
  theme_bw() +
  xlab("Plant Transfer") +
  ylab("Percentage Hatched") +
  scale_color_viridis(
    begin = 1,
    end = 0,
    option = "E",
    discrete = T
  ) +
  scale_x_discrete(labels = c("0", "1", "2", "3")) +
  coord_cartesian(ylim = c(0, 150)) + geom_errorbar(
    aes(
      x = Period,
      ymin = Hatch_Percent - Hatch_SEM,
      ymax = Hatch_Percent + Hatch_SEM
    ),
    size = 0.5,
    position = position_dodge(0.2)
  ) +
  theme(legend.position = "none")+
  facet_wrap(~Germplasm)

hatch_grph
ggsave(
  "x_hatch_graph_3.jpg",
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

#eggs graph
egg_graph <-
  ggplot(fcnd,
         aes(
           color = germ,
           group = psyllid,
           x = period,
           y = eggs
         )) +
  geom_point() +
  geom_line() +
  theme_bw() +
  xlab("Plant Transfer") +
  ylab("Number of Eggs") +
  scale_color_viridis(
    begin = 1,
    end = 0,
    option = "E",
    discrete = T
  ) +
  theme(legend.position = c(0.8, 0.84)) +
  coord_cartesian(ylim = c(0, 70))

egg_graph

ggsave(
  "x_egg_graph_4.jpg",
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

#nymphs
nym_graph <-
  ggplot(fcnd,
         aes(
           color = germ,
           group = psyllid,
           x = period,
           y = nymphs
         )) +
  geom_point() +
  geom_line() +
  theme_bw() +
  xlab("Plant Transfer") +
  ylab("Number of Nymphs") +
  scale_color_viridis(
    begin = 1,
    end = 0,
    option = "E",
    discrete = T
  ) +
  theme(legend.position = c(0.8, 0.84)) +
  coord_cartesian(ylim = c(0, 70))

nym_graph

ggsave(
  "x_nymph_graph_4.jpg",
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

#hatch percentage
hatch_graph <-
  ggplot(fcnd,
         aes(
           color = germ,
           group = psyllid,
           x = period,
           y = hatch
         )) +
  geom_point() +
  geom_line() +
  theme_bw() +
  xlab("Plant Transfer") +
  ylab("Hatch Percentage") +
  scale_color_viridis(
    begin = 1,
    end = 0,
    option = "E",
    discrete = T
  ) +
  theme(legend.position = 'right')

hatch_graph

ggsave(
  "x_hatch_graph_4.jpg",
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

#scatterplots
ggplot(fcnd, aes(y=eggs, x = period)) +
  geom_point() +
  facet_wrap(~germ) +
  ggtitle('Number of Eggs Produced on Four Germplasms')

ggsave(
  "x_eggs_graph_5.jpg",
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

ggplot(fcnd, aes(y=nymphs, x = period)) +
  geom_point() +
  facet_wrap(~germ) +
  ggtitle('Number of Nymphs Produced on Four Germplasms')

ggsave(
  "x_nymph_graph_5.jpg",
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

ggplot(fcnd, aes(y=hatch, x = period)) +
  geom_point() +
  facet_wrap(~germ) +
  ggtitle('Hatch % of Eggs Produced on Four Germplasms')

ggsave(
  "x_hatch_graph_5.jpg",
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

rm(list = ls())
