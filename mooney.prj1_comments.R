#loading libraries and setting the working directory
library(ggplot2)
library(tidyverse)
setwd("~/Documents/RStudio/scales")

#CPK: No need set the working directory when working in an R project.


#loading scales dataset
dat <- read.csv("scales.csv")

#reporting the class of each column
sapply(dat,class)

#reporting dimensions of the dataset, reported as "rows, columns"
dim(dat)

#summary of number of scales punctured for each species
dat$species <- as.factor(dat$species)
dat %>%
  group_by(species) %>%
  summarise(n = n())

#summary of number of specimens sampled for each species
dat %>% 
  count(species,specimen) %>%
  print() %>%
  count(species,name = "n.specimens")

#PDF countaing boxplots of puncture force and quadrant
species <- levels(dat$species)
pdf("stephenmooney.species.quadrant.pdf")
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}
dev.off()

#CPK: Excellent work!! Well done, Stephen!