library(tidyverse)
library(features)
speeds <- read_csv("pseed.calibration.csv")
pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv( "pseed.lengths.csv")
#setup
pseed2 <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))%>%
  print()
#join speeds
pseed2 <- pseed2%>%
  left_join(pseed.bl,by="fish")%>%
  print()
#joinbl
pseed2 <- pseed2%>%
  mutate(bl.s=cm.s/bl)%>%
  print()
#adds bl.s column
pseed2 <- pseed2 %>%
  group_by(date,frame) %>%
  mutate(amp.sum=sum(amp.bl))
#adds amp.sum
pseed.wide <- pseed2 %>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)%>%
  print() 
#adds L + R/ Question 1 complete
pseed.amp.max <- pseed.wide %>% 
  group_by(fish, date, bl.s) %>% 
  summarize(amp.sum.max = max(amp.sum))
#tibble with max of amp.sums
pseed.mean <- pseed.amp.max %>% 
  group_by(fish, bl.s) %>% 
  summarize(amp.sum.mean = mean(amp.sum.max))
#made a tibble with the mean values of those maximums for each fish 
pseed.sum.max <- pseed.amp.max%>% 
  left_join(pseed.mean, by = c("fish", "bl.s"))
#combined the two tibbles as pseed.sum.max
SE <- function(x) {
  sd(x)/sqrt(length(x))
}
#standard error of the mean
pseed.sum.max <- pseed.sum.max %>% 
  group_by(fish, bl.s) %>% 
  mutate(amp.sum.se = SE(amp.sum.max))
#calculated standard errors and added them to a new column "amp.sum.se"
pseed.sum.max %>% 
  group_by(fish, bl.s) %>% 
  ggplot(aes(x=bl.s, y=amp.sum.mean, colour=fish)) + 
  geom_errorbar(aes(ymin=amp.sum.mean-amp.sum.se, ymax=amp.sum.mean+amp.sum.se), width=.1) +
  geom_point() + geom_smooth(method="lm")
#mean amp.sum vs specific swimming speed
pseed.met.rate <- read.csv("pseed.met.rate.csv")
pseed.sum.max <- pseed.sum.max %>% 
  left_join(pseed.met.rate)
as_tibble(pseed.sum.max)
#Met.rate data joined
pseed.sum.max %>% 
  group_by(fish, bl.s) %>% 
  ggplot(aes(x=amp.sum.mean , y=met.rate,col=fish))+ geom_point()+geom_smooth(method="lm")
#metabolic output vs mean max w/ linear regression