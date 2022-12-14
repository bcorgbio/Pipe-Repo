library(tidyverse) # Remember to load your libraries!
library(ape)
library(nlme)
library(geiger)
library(caper)
library(phytools)
library(viridis)
library(MuMIn)

#1. Loading Data
anole <- read_csv("anole.dat.csv")
anole.eco <- read_csv("anole.eco.csv")

anole.log <- anole%>%
  left_join(anole.eco)%>%
  filter(!Ecomorph%in%c("U","CH"))%>%
  na.omit()%>%
  mutate_at(c("SVL", "HTotal","PH","ArbPD"),log)


#Q2: 2 linear models
anole.lm.PH <- lm(HTotal~SVL+PH,anole.log)

anole.lm.PD <- lm(HTotal~SVL+ArbPD,anole.log)

#Q3
anole.log <- anole.log %>%
  mutate(res.PH=residuals(anole.lm.PH),res.PD=residuals(anole.lm.PD))

anole.log%>%
  dplyr::select(Ecomorph2,res.PD,res.PH)%>%
  pivot_longer(cols=c("res.PD","res.PH"))%>%
  ggplot(aes(x=Ecomorph2,y=value))+geom_boxplot()+facet_grid(name~.,scales = "free_y")+ylab("residual")

#Q4
anole.tree <- read.tree("anole.tre")

#A PGLS model with the hindlimb-SVL relationship + perch height
pgls.BM1 <- gls(HTotal~SVL+PH, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")

#A PGLS model with the hindlimb-SVL relationship + perch diameter
pgls.BM2 <- gls(HTotal~SVL+ArbPD, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")

# A PGLS model with the hindlimb-SVL relationship + perch height + perch diameter
pgls.BM3 <- gls(HTotal~SVL+ArbPD+PH, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")

#Q5

anole.phylo.aic <- AICc(pgls.BM1,pgls.BM2,pgls.BM3)
aicw(anole.phylo.aic$AICc)
anova(pgls.BM3)
#Perch diameter is the better predictor of hind-limb length, producing a P-value (0.0005) 2 orders of magnitude smaller than perch height. This is indicative of a strong correlation with hind-limb length.


#Q6
anole.log <- anole.log%>%
  mutate(phylo.res=residuals(pgls.BM3))

anole.log%>%
  ggplot(aes(Ecomorph2,phylo.res))+geom_boxplot()

