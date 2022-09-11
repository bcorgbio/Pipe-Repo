library(ggplot2)
library(tidyverse)

dat <- read.csv("scales.csv")
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

pdf("Matteo_species.quadrant.pdf")
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}
dev.off()

