library(tidyverse)
library(features)

pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")

pseed.met.rate <- read_csv("pseed.met.rate.csv")

#Question  1

find.peaks <- function(x,y,mult=100){ 
  f <- fget(features(x = x,y=y*mult))[2:3]%>% 
    as_tibble()%>% 
    filter(curvature<0)%>% 
    mutate(peaks=round(crit.pts,0))
  return(f$peaks) 
}

pseed2 <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))%>%
  print()

pseed2 <- pseed2%>%
  left_join(pseed.bl,by="fish")%>%
  print()

pseed2 <- pseed2%>%
  mutate(bl.s=cm.s/bl)%>%
  print()

pseed2 <- pseed2 %>%
  group_by(date,frame) %>%
  mutate(amp.sum=sum(amp.bl))

pseed.wide <- pseed2 %>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)%>%
  print() 

#Question 2/3

standard_error <- function(x) sd(x) / sqrt(length(x))

pseed.sum.max <- pseed.wide%>%
  group_by(fish, bl.s)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
  filter(peak==T)

pseed.sum.max <- pseed.sum.max%>%
  group_by(fish,bl.s)%>%
  summarize(amp.sum.mean=mean(amp.sum),
            amp.sum.se=standard_error(amp.sum))

pseed.sum.max %>%
  ggplot(aes(x=bl.s, y=amp.sum.mean, col = fish)) + geom_point() + geom_errorbar(aes(ymin=amp.sum.mean-amp.sum.se, ymax=amp.sum.mean+amp.sum.se), width = 0.03, size = 0.3)+geom_smooth(method="lm")

#Question 4

pseed.met.sum <- pseed.met.rate%>%
  group_by(fish,bl.s)%>%
  summarize(met.mean=mean(met.rate))

pseed.sum.max <- pseed.sum.max%>%
  inner_join(pseed.met.sum, by=c("fish", "bl.s"))%>%
  group_by(fish,bl.s)


#Question 6
pseed.sum.max%>%
  ggplot(aes(x=amp.sum.mean,y=met.mean,col=fish))+geom_point()+geom_smooth(method="lm")

