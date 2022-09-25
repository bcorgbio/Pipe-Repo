#Philip Chen
library(ggplot2)
library(tidyverse)

dat <- read.csv("scales.csv")
#dat variable assigned with the scales dataset

sapply(dat, class)
#class of each vector/column in data frame "dat"

dim(dat)

#CPK: don't need this `head` operation [-1]

head(dat)
#dimensions

dat$species <- as.factor(dat$species)
species <- levels(dat$species)
#convert species vector into a factor

species.n <- dat%>%
  group_by(species) %>%
  summarise(n = n())
species.n
#number of punctures per species

table(dat$species)
#another way I found
#CPK: Very nice!!


dat %>%
  count(species, specimen) %>%
  print() %>%
  count(species, name = "n.specimens")
#number of unique specimen sampled for each species and number of observations made with each specimen

pdf("Chen_species.quadrant.pdf")
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}
dev.off()
#print ggplots filtered by species to a new pdf file named "Chen_species.quadrant.pdf"

#CPK: Excellent work! Just be sure to only include what answers/addresses the prompts.

