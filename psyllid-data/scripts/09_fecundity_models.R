####PREPARING FECUNDITY DATA FOR ANALYSIS####
fcnd <- read_csv("results/fcnd.csv")

#telling r which data are factors
fcnd <-
  dplyr::mutate(fcnd, psyllid = factor(psyllid, levels = unique(psyllid)))
fcnd <-
  dplyr::mutate(fcnd, germ = factor(germ, levels = unique(germ)))
fcnd <-
  dplyr::mutate(fcnd, block = factor(block, levels = unique(block)))
fcnd <-
  dplyr::mutate(fcnd, letter = factor(letter, levels = unique(letter)))
fcnd <-
  dplyr::mutate(fcnd, period = factor(period, levels = unique(period)))

#creating a table for overall hatch data per psyllid
eg_tots <- rowsum(fcnd$eggs, fcnd$psyllid)
ny_tots <- rowsum(fcnd$nymphs, fcnd$psyllid)
ht_tots <- (ny_tots / eg_tots)
fcnd_hatch <- cbind(eg_tots, ny_tots, ht_tots)
colnames(fcnd_hatch) <- c("eggs", "nymphs", "hatch")
psy <- fcnd_hatch[,0]
fcnd_hatch <- as_tibble(fcnd_hatch, rownames = "psyllid", .name_repair = "minimal")

#pulling in blocking data from master
fcd <- readxl::read_excel("data/fecundity_master.xlsx")
fcd <- dplyr::select(fcd, "psyllid", "germ", "block", 'letter')
fcd$psyllid <- as.character(fcd$psyllid)
fcnd_hatch <- dplyr::left_join(fcnd_hatch, fcd)

#rearranging like original dataset
fcnd_hatch <-
  dplyr::select(fcnd_hatch,
                'psyllid',
                'germ',
                'block',
                'letter',
                'eggs',
                'nymphs',
                'hatch')

#making sure psyllid is a factor
fcnd_hatch <-
  dplyr::mutate(fcnd_hatch, psyllid = factor(psyllid, levels = unique(psyllid)))
write_xlsx(fcnd_hatch, "results/fcnd_hatch.xlsx")

# ####CHECKING DATA DISTRIBUTIONS####
# #makes a Cullen and Frey graph of our data
# #to help us determine our data distribution
# descdist(fcnd$eggs, discrete = T)
# descdist(fcnd$nymphs, discrete = T)
# descdist(fcnd_hatch$hatch, discrete = F)
# qqnorm(fcnd_hatch$hatch)
# qqline(fcnd_hatch$hatch)
# #looks like poisson would be a good option for both eggs and nymphs


####EGG GLM####
eggs_glm <- glmer(
  eggs ~ germ * period + (1 | psyllid),
  data = fcnd,
  family = poisson,
  nAGQ = 25,
  control = glmerControl(
    optimizer = "optimx",
    optCtrl = list(method = "bobyqa",
                   dowarn = T)
  )
)


#summary
sink('results/eggs_summary.txt')
print(summary(eggs_glm))
sink()

#anova (period and period*germplasm interaction significant)
sink('results/eggs_anov.txt')
print(Anova(eggs_glm, type = "II"))
sink()

main_eggs <-
  emmeans(eggs_glm, list(pairwise ~ germ + period), adjust = "tukey", details = "true")

#gives a compact letter display of estimated marginal means (lsmeans) and saving results
sink('results/eggs_CLDs.txt', append = T)
print(CLD(main_eggs, Letters = letters, adjust = "tukey", details = "true"))
CLD(main_eggs, Letters = letters, by = "period", adjust = "tukey", details = "true")
CLD(main_eggs, Letters = letters, by = "germ", adjust = "tukey", details = "true")
sink()

#saving the stats
eggs_dat <-
  emmeans(eggs_glm, ~ germ + period, type = 'response', adjust = "tukey", details = "true")
sink("results/eggs_dat.txt")
print(eggs_dat)
sink()

#saving the stats for overall eggs laid
eggs_total_dat <-
  emmeans(eggs_glm, ~ period, type = 'response', adjust = "tukey", details = "true")
sink("results/eggs_total_dat.txt")
print(eggs_total_dat)
sink()


####NYMPH GLM####
nym_glm <- glmer(
  nymphs ~ germ * period + (1 | psyllid),
  data = fcnd,
  family = poisson,
  nAGQ = 25,
  control = glmerControl(
    optimizer = "optimx",
    optCtrl = list(method = "bobyqa",
                   dowarn = T)
  )
)


#summary
sink('results/nym_summary.txt')
summary(nym_glm)
sink()

#anova
sink('results/nym_anov.txt')
Anova(nym_glm, type = "II")
sink()

main_nym <-
  emmeans(nym_glm, list(pairwise ~ germ + period), adjust = "tukey", details = "true")


#gives a compact letter display of estimated marginal means (lsmeans)
sink('results/nym_CLDs.txt', append = T)
CLD(main_nym, Letters = letters, adjust = "tukey", details = "true")
CLD(main_nym, Letters = letters, by = "period", adjust = "tukey", details = "true")
CLD(main_nym, Letters = letters, by = "germ", adjust = "tukey", details = "true")
sink()

#saving the summary stats
sink("results/nym_dat.txt")
nym_dat <-
  emmeans(nym_glm, ~ germ + period, type = 'response', adjust = "tukey", details = "true")
nym_dat
sink()



####HATCH GLM (PRODUCTION PER DAY)####
hatch_glm_1 <- glmer(
  hatch ~ germ * period + (1 | psyllid),
  data = fcnd,
  weights = eggs,
  family = "binomial",
  nAGQ = 25,
  control = glmerControl(
    optimizer = "optimx",
    optCtrl = list(method = "bobyqa",
                   dowarn = T)
  )
)


#summary and anova
sink('results/hatch_1_summary.txt')
summary(hatch_glm_1)
sink()

#anova (period and period*germplasm interaction significant)
sink('results/hatch_1_anov.txt')
Anova(hatch_glm_1, type = "II")
sink()

main_hatch_1 <-
  emmeans(hatch_glm_1, list(pairwise ~ germ + period), adjust = "tukey", details = "true")

#gives a compact letter display of estimated marginal means (lsmeans)
sink('results/hatch_1_CLDs.txt', append = T)
CLD(main_hatch_1, Letters = letters, adjust = "tukey", details = "true")
CLD(main_hatch_1, Letters = letters, by = "period", adjust = "tukey", details = "true")
CLD(main_hatch_1, Letters = letters, by = "germ", adjust = "tukey", details = "true")
sink()

sink('results/hatch_1_dat.txt')
hatch_1_dat <-
  emmeans(hatch_glm_1, ~ germ + period, type = 'response', adjust = "tukey", details = "true")
hatch_1_dat
sink()

sink('results/hatch_totals_dat.txt')
hatch_totals_dat<-
  emmeans(hatch_glm_1, ~ period, type = 'response', adjust = "tukey", details = "true")
hatch_totals_dat
sink()

####HATCH GLM (PRODUCTION PER INDIVIDUAL)####
hatch_glm_2 <- glmer(
  hatch ~ germ + (1 | psyllid),
  data = fcnd_hatch,
  weights = eggs,
  family = "binomial",
  nAGQ = 25,
  control = glmerControl(
    optimizer = "optimx",
    optCtrl = list(method = "bobyqa",
                   dowarn = T)
  )
)

#summary and anova
sink('results/hatch_2_summary.txt')
summary(hatch_glm_2)
sink()

#anova (period and period*germplasm interaction significant)
sink('results/hatch_2_anov.txt')
Anova(hatch_glm_2, type = "II")
sink()

main_hatch_2 <-
  emmeans(hatch_glm_2, list(pairwise ~ germ), adjust = "tukey", details = "true")

#gives a compact letter display of estimated marginal means (lsmeans)
sink('results/hatch_2_CLDs.txt')
CLD(main_hatch_2, Letters = letters, adjust = "tukey", details = "true")
sink()

sink('results/hatch_2_dat.txt')
hatch_2_dat <-
  emmeans(hatch_glm_2, ~ germ, type = 'response', adjust = "tukey", details = "true")
hatch_2_dat
sink()
