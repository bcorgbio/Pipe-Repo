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

table(dat$species)

dat %>%
  count(species, specimen) %>%
  print() %>%
  count(species, name = "n.specimens")