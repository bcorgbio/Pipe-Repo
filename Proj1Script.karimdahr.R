
library(tidyverse)
library(ggplot2)
dat <- read.csv("scales.csv")
#setup


head(dat)
sapply(dat, class)
dim(dat)
#data info


dat$species <- as.factor(dat$species)
species <- levels(dat$species)
#Seperates species


species.n<- dat %>%
  group_by(species) %>%
  summarise(n = n())
species.n
#Punctures for each species


dat %>% 
  count(species,specimen) %>%
  print() %>%
  count(species,name = "n.specimens")
# number of fish sampled/species


for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}
#Plot

pdf("Karim.project1quadrant.pdf")
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}
#Pdf
dev.off()
