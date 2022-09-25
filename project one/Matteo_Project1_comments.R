library(ggplot2)
library(tidyverse)

dat <- read.csv("scales.csv")

#CPK: dont't need the `view` and `head` operations [-1]

view(dat)

dim(dat)
head(dat)
sapply(dat,class)

dat$species <- as.factor(dat$species)
species <- levels(dat$species)

species.n <- dat%>%
  group_by(species) %>%
  summarise(n = n())
species.n

dat %>%
  count(species, specimen) %>%
  print() %>%
  count(species, name = "n.specimens")

pdf("Matteo.species.quadrant.pdf")
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}
dev.off()

#CPK: Excellent work, Matteo! Just be sure to include only what you need to answer/address the prompts.