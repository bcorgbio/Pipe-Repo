#loading libraries and data
library(tidyverse)
library(features)

pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")
pseed.met.rate <- read_csv("pseed.met.rate.csv")

#creating the pseed.wide data tibble
pseed2 <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))
pseed2 <- pseed2%>%
  left_join(pseed.bl,by="fish")
pseed2 <- pseed2%>%
  mutate(bl.s=cm.s/bl)
pseed2 <- pseed2 %>%
  group_by(date,frame) %>%
  mutate(amp.sum=sum(amp.bl))

pseed.wide <- pseed2 %>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)%>%
  print()

#computing mean maximum for all amp.sums for each swimming speed, and calculating SE
se <- function(x) sd(x) / sqrt(length(x))

pseed.sum.max <- pseed.wide %>%
  as_tibble()%>%
  group_by(fish,bl.s)%>%
  summarize(amp.sum.mean=mean(amp.sum), amp.sum.se=se(x = amp.sum))

#amp.sum vs swimming speed with SE error bars by specimen
pseed.sum.max %>%
  ggplot(aes(x=bl.s,y=amp.sum.mean,col=fish)) + geom_point() + geom_smooth(method="lm") + geom_errorbar(aes(ymin=amp.sum.mean-amp.sum.se, ymax=amp.sum.mean+amp.sum.se), width=.1)

#reading amd merging pseed.met.rate.cvs
pseed.sum.max <- pseed.sum.max%>%
  left_join(pseed.met.rate)

#plotting metabolic power output to each fish
pseed.sum.max %>%
  ggplot(aes(y=met.rate,x=amp.sum.mean,col=fish)) +  geom_point()  + geom_smooth(method="lm")

