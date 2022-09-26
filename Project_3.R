library(tidyverse) # Rember to load your libraries!
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

anole2 <- anole%>%
  left_join(anole.eco)%>%
  filter(!Ecomorph%in%c("U","CH"))%>%
  na.omit()%>%
  print()

anole.log <- anole2%>%
  mutate_at(c("SVL", "HTotal","PH","ArbPD"),log)


#Q2: 2 linear models
anole.lm.PH <- lm(HTotal~SVL+PH,anole.log)

anole.lm.PD <- lm(HTotal~SVL+ArbPD,anole.log)

anole.log <- anole.log %>%
  mutate(res.PH=residuals(anole.lm.PH),res.PD=residuals(anole.lm.PD))

#Q3
anole.log%>%
  ggplot(aes(Ecomorph2,res.PD))+geom_boxplot()

anole.log%>%
  ggplot(aes(Ecomorph2,res.PH))+geom_boxplot()


#Q4
anole.tree <- read.tree("anole.tre")
plot(anole.tree,cex=0.4)

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
#add conclusion

#Q6
anole.log <- anole.log%>%
  mutate(phylo.res=residuals(pgls.BM3))

p.phylo.point <- anole.log%>%
  ggplot(aes(x=ArbPD,y=phylo.res)) +geom_point() + geom_hline(aes(yintercept=mean(phylo.res)),color="blue")

print(p.phylo.point)
