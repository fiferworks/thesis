####PREPARING NO-CHOICE DATA FOR MODELING####
#preparing data for linear models
#telling R that variety is a factor
bhvr <-
  mutate(bhvr,
         variety = factor(variety,
                          levels = unique(variety)))

#telling R that sex is a factor
bhvr <-
  mutate(bhvr,
         sex = factor(sex,
                      levels = unique(sex)))

#telling R that psyllid is a factor
bhvr <-
  mutate(bhvr,
         psyllid = factor(psyllid,
                          levels = unique(psyllid)))

#telling R that mins_away is an integer
bhvr <-
  mutate(bhvr,
         mins_away = as.integer(mins_away))

#telling R that duration data are an integers
bhvr <- mutate(bhvr, d_probes = as.integer(d_probes))
bhvr <- mutate(bhvr, d_walks = as.integer(d_walks))
bhvr <- mutate(bhvr, d_cleans = as.integer(d_cleans))
bhvr <- mutate(bhvr, d_off = as.integer(d_off))




# ####CHECKING DATA DISTRIBUTIONS####
# #makes a Cullen and Frey graph of our data
# #to help us determine our data distribution
# descdist(bhvr$i_probes, discrete = T)
# descdist(bhvr$d_probes, discrete = T)
# descdist(bhvr$i_walks, discrete = T)
# descdist(bhvr$d_walks, discrete = T)
# descdist(bhvr$i_cleans, discrete = T)
# descdist(bhvr$d_cleans, discrete = T)
# descdist(bhvr$i_off, discrete = T)
# descdist(bhvr$d_off, discrete = T)
# more close to Poisson than negative binomial distribution




####PROBE INCIDENCE####
#a generalized linear mixed model describing probe incidence with psyllid number as the random variable
iprobes_glm <- glmer(
  i_probes ~ variety + sex + sex * variety + (1 | psyllid),
  data = bhvr,
  family = poisson,
  nAGQ = 25,
  control = glmerControl(
    optimizer = "optimx",
    optCtrl = list(method = "bobyqa",
                   dowarn = T)
  )
)

#summary of the model
sink('results/iprobes_model.txt')
summary(iprobes_glm)
sink()

#generates an anova table from our model (variety significant)
sink('results/iprobes_anov.txt')
Anova(iprobes_glm, type = "II")
sink()

###estimated marginal means (least-squares means) pairwise comparisons###
#for variety differences, see following iprobesv_table:
em_iprobes <-
  emmeans(
    iprobes_glm,
    list(pairwise ~ sex + variety + sex |
           variety),
    adjust = "tukey",
    details = T
  )

main_iprobes <-
  emmeans(iprobes_glm,
          list(pairwise ~ variety),
          adjust = "tukey",
          details = T)

#gives a compact letter display of estimated marginal means (lsmeans)
sink('results/iprobes_CLDS.txt')

CLD(main_iprobes,
    Letters = letters,
    adjust = "tukey",
    details = T)

CLD(em_iprobes,
    Letters = letters,
    adjust = "tukey",
    details = T)
CLD(
  em_iprobes,
  Letters = letters,
  by = "variety",
  adjust = "tukey",
  details = T
)
CLD(
  em_iprobes,
  Letters = letters,
  by = "sex",
  adjust = "tukey",
  details = T
)

#saving results
sink()


#saving the summary stats
sink("results/iprobes_dat.txt")
emmeans(
  iprobes_glm,
  ~ variety + sex ,
  type = 'response',
  adjust = "tukey",
  details = T
)
sink()




####PROBE DURATION####
#a generalized linear mixed model describing probe duration with psyllid number as the random variable
dprobes_glm <- glmer(
  d_probes ~ variety + sex + sex * variety + (1 | psyllid),
  data = bhvr,
  family = poisson,
  nAGQ = 25,
  control = glmerControl(
    optimizer = "optimx",
    optCtrl = list(method = "bobyqa",
                   dowarn = T)
  )
)

#summary of the model
sink('results/dprobes_model.txt')
summary(dprobes_glm)
sink()

#generates an anova table from our model (nothing)
sink('results/dprobes_anov.txt')
(Anova(dprobes_glm, type = "II"))
sink()

#for variety differences, see following iprobesv_table:
em_dprobes <-
  emmeans(
    dprobes_glm,
    list(pairwise ~ sex + variety + sex |
           variety),
    adjust = "tukey",
    details = T
  )

main_dprobes <-
  emmeans(dprobes_glm,
          list(pairwise ~ variety),
          adjust = "tukey",
          details = T)

#gives a compact letter display of estimated marginal means (lsmeans)
sink('results/dprobes_CLDS.txt')
CLD(main_dprobes,
    Letters = letters,
    adjust = "tukey",
    details = T)

CLD(em_dprobes,
    Letters = letters,
    adjust = "tukey",
    details = T)
CLD(
  em_dprobes,
  Letters = letters,
  by = "variety",
  adjust = "tukey",
  details = T
)

CLD(
  em_dprobes,
  Letters = letters,
  by = "sex",
  adjust = "tukey",
  details = T
)
sink()

#saving the summary stats
sink("results/dprobes_dat.txt")
emmeans(
  dprobes_glm,
  ~ variety + sex,
  type = 'response',
  adjust = "tukey",
  details = T
)
sink()


####WALKING INCIDENCE####
iwalks_glm <- glmer(
  i_walks ~ variety + sex + sex * variety + (1 | psyllid),
  data = bhvr,
  family = poisson,
  nAGQ = 25,
  control = glmerControl(
    optimizer = "optimx",
    optCtrl = list(method = "bobyqa",
                   dowarn = T)
  )
)

#summary of the model
sink('results/iwalks_model.txt')
summary(iwalks_glm)
sink()

#generates an anova table from our model
sink('results/iwalks_anov.txt')
(Anova(iwalks_glm, type = "II"))
sink()

#observing main effects
em_iwalks <-
  emmeans(
    iwalks_glm,
    list(pairwise ~ sex + variety + sex |
           variety),
    adjust = "tukey",
    details = T
  )

main_iwalks <-
  emmeans(iwalks_glm,
          list(pairwise ~ variety),
          adjust = "tukey",
          details = T)

sink('results/iwalks_CLDs.txt')
CLD(main_iwalks,
    Letters = letters,
    adjust = "tukey",
    details = T)

CLD(em_iwalks,
    Letters = letters,
    adjust = "tukey",
    details = T)

CLD(
  em_iwalks,
  Letters = letters,
  by = "variety",
  adjust = "tukey",
  details = T
)

CLD(
  em_iwalks,
  Letters = letters,
  by = "sex",
  adjust = "tukey",
  details = T
)

#saving results
sink()


#saving the summary stats
sink('results/iwalks_dat.txt')
emmeans(
  iwalks_glm,
  ~ variety + sex,
  type = 'response',
  adjust = "tukey",
  details = T
)
sink()

####WALKING DURATION####
dwalks_glm <- glmer(
  d_walks ~ variety + sex + sex * variety + (1 | psyllid),
  data = bhvr,
  family = poisson,
  nAGQ = 25,
  control = glmerControl(
    optimizer = "optimx",
    optCtrl = list(method = "bobyqa",
                   dowarn = T)
  )
)

#summary of the model
sink('results/dwalks_model.txt')
summary(dwalks_glm)
sink()


#generates an anova table from our model
sink('results/dwalks_anov.txt')
(Anova(dwalks_glm, type = "II"))
sink()

##variety preferences with sex interaction? Marginal significance?
em_dwalks <-
  emmeans(
    dwalks_glm,
    pairwise ~ sex + variety + sex |
      variety,
    adjust = "tukey",
    details = T
  )

main_dwalks <-
  emmeans(dwalks_glm,
          pairwise ~ variety,
          adjust = "tukey",
          details = T)

sink('results/dwalks_CLDs.txt')

CLD(
  main_dwalks,
  Letters = letters,
  adjust = "tukey",
  sort = T,
  details = T
)

CLD(
  em_dwalks,
  Letters = letters,
  adjust = "tukey",
  sort = T,
  details = T
)

CLD(
  em_dwalks,
  Letters = letters,
  by = "variety",
  adjust = "tukey",
  sort = T,
  details = T
)

CLD(
  em_dwalks,
  Letters = letters,
  by = "sex",
  adjust = "tukey",
  sort = T,
  details = T
)

#saving results
sink()


#saving the summary stats
sink('results/dwalks_dat.txt')
emmeans(
  dwalks_glm,
  ~ variety + sex,
  type = 'response',
  adjust = "tukey",
  details = T
)
sink()




####CLEANING INCIDENCE####
icleans_glm <- glmer(
  i_cleans ~ variety + sex * variety + (1 | psyllid),
  data = bhvr,
  family = poisson,
  nAGQ = 25,
  control = glmerControl(
    optimizer = "optimx",
    optCtrl = list(method = "bobyqa",
                   dowarn = T)
  )
)

#summary of the model
sink('results/icleans_model.txt')
summary(icleans_glm)
sink()

#generates an anova table from our model (nothing)
sink('results/icleans_anov.txt')
Anova(icleans_glm, type = 'II')
sink()

#for variety differences, see following iprobesv_table:
em_icleans <-
  emmeans(
    icleans_glm,
    list(pairwise ~ sex + variety + sex |
           variety),
    adjust = "tukey",
    details = T
  )

main_icleans <-
  emmeans(icleans_glm,
          list(pairwise ~ variety),
          adjust = "tukey",
          details = T)


sink('results/icleans_CLDS.txt')
CLD(main_icleans,
    Letters = letters,
    adjust = "tukey",
    details = T)


CLD(em_icleans,
    Letters = letters,
    adjust = "tukey",
    details = T)

CLD(
  em_icleans,
  Letters = letters,
  by = "variety",
  adjust = "tukey",
  details = T
)

CLD(
  em_icleans,
  Letters = letters,
  by = "sex",
  adjust = "tukey",
  details = T
)

#saving results
sink()

#saving the summary stats
sink('results/icleans_dat.txt')
emmeans(
  icleans_glm,
  ~ variety + sex ,
  type = 'response',
  adjust = "tukey",
  details = T
)
sink()

####CLEANING DURATION####
dcleans_glm <- glmer(
  d_cleans ~ variety + sex * variety +  (1 | psyllid),
  data = bhvr,
  family = poisson,
  nAGQ = 25,
  control = glmerControl(
    optimizer = "optimx",
    optCtrl = list(method = "bobyqa",
                   dowarn = T)
  )
)

#summary of the model
sink('results/dcleans_model.txt')
summary(dcleans_glm)
sink()


#generates an anova table from our model (nothing)
sink('results/dcleans_anov.txt')
(Anova(dcleans_glm, type = "II"))
sink()

em_dcleans <-
  emmeans(
    dcleans_glm,
    list(pairwise ~ sex + variety + sex |
           variety),
    adjust = "tukey",
    details = T
  )

main_dcleans <-
  emmeans(dcleans_glm,
          list(pairwise ~ variety),
          adjust = "tukey",
          details = T)

#gives a compact letter display of estimated marginal means (lsmeans)
#saving results
sink('results/dcleans_CLDS.txt')

CLD(main_dcleans,
    Letters = letters,
    adjust = "tukey",
    details = T)

CLD(em_dcleans,
    Letters = letters,
    adjust = "tukey",
    details = T)


CLD(
  em_dcleans,
  Letters = letters,
  by = "variety",
  adjust = "tukey",
  details = T
)
CLD(
  em_dcleans,
  Letters = letters,
  by = "sex",
  adjust = "tukey",
  details = T
)

sink()

#saving the summary stats
sink('results/dcleans_dat.txt')
emmeans(
  dcleans_glm,
  ~ variety + sex,
  type = 'response',
  adjust = "tukey",
  details = T
)
sink()


####OFF LEAF INCIDENCE####
ioff_glm <- glmer(
  i_off ~ variety + sex + (1 | psyllid),
  data = bhvr,
  family = poisson,
  nAGQ = 25,
  control = glmerControl(
    optimizer = "optimx",
    optCtrl = list(method = "bobyqa",
                   dowarn = T)
  )
)

#recording the summary
sink('results/ioff_model.txt')
summary(ioff_glm)
sink()

#recording the anova
sink('results/ioff_anov.txt')
Anova(ioff_glm, type = "II")
sink()

##no variety differences
main_ioff <-
  emmeans(
    ioff_glm,
    list(pairwise ~ sex + variety + sex |
           variety),
    adjust = "tukey",
    details = T
  )

em_ioff <-
  emmeans(ioff_glm,
          list(pairwise ~ variety),
          adjust = "tukey",
          details = T)

#gives a compact letter display of estimated marginal means (lsmeans)
sink('results/ioff_CLDS.txt')

CLD(main_ioff,
    Letters = letters,
    adjust = "tukey",
    details = T)

CLD(em_ioff,
    Letters = letters,
    adjust = "tukey",
    details = T)

CLD(
  em_ioff,
  Letters = letters,
  by = "variety",
  adjust = "tukey",
  details = T
)

CLD(
  em_ioff,
  Letters = letters,
  by = "sex",
  adjust = "tukey",
  details = T
)

#saving results
sink()

sink('results/ioff_dat.txt')
emmeans(
  ioff_glm,
  ~ variety + sex,
  type = 'response',
  adjust = "tukey",
  details = T
)
sink()



####OFF LEAF DURATION####
doff_glm <- glmer(
  d_off ~ variety + sex +  (1 | psyllid),
  data = bhvr,
  family = poisson,
  nAGQ = 25,
  control = glmerControl(
    optimizer = "optimx",
    optCtrl = list(method = "bobyqa",
                   dowarn = T)
  )
)

#recording the summary
sink('results/doff_model.txt')
summary(doff_glm)
sink()

#recording the anova
sink('results/doff_anov.txt')
Anova(doff_glm, type = "II")
sink()

##no variety differences
em_doff <-
  emmeans(
    doff_glm,
    list(pairwise ~ sex + variety + sex |
           variety),
    adjust = "tukey",
    details = T
  )

main_doff <-
  emmeans(doff_glm,
          list(pairwise ~ variety),
          adjust = "tukey",
          details = T)

#gives a compact letter display of estimated marginal means (lsmeans)
#saving results
sink('results/doff_CLDS.txt')

CLD(main_doff,
    Letters = letters,
    adjust = "tukey",
    details = T)

CLD(em_doff,
    Letters = letters,
    adjust = "tukey",
    details = T)
CLD(
  em_doff,
  Letters = letters,
  by = "variety",
  adjust = "tukey",
  details = T
)
CLD(
  em_doff,
  Letters = letters,
  by = "sex",
  adjust = "tukey",
  details = T
)

sink()

sink('results/doff_dat.txt')
emmeans(
  doff_glm,
  ~ variety + sex,
  type = 'response',
  adjust = "tukey",
  details = T
)
sink()
