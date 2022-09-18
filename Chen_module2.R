library(tidyverse)
library(features)
#libraries

pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")
#loading data as tibbles

pseed2 <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))
pseed2 <- pseed2%>%
  left_join(pseed.bl,by="fish")
pseed2 <- pseed2%>%
  mutate(bl.s=cm.s/bl)
pseed.wide <- pseed2 %>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)
#combined code to make pseed.wide tibble

pseed.max.cycle <- pseed.wide %>% 
  group_by(fish, date, bl.s) %>% 
  summarize(amp.sum.max = max(amp.sum))
#made a tibble with maximum of amp.sums for each specific swimming speed for each cycle

pseed.mean <- pseed.max.cycle %>% 
  group_by(fish, bl.s) %>% 
  summarize(amp.sum.mean = mean(amp.sum.max))
#made a tibble with the mean values of those maximums for each fish 

pseed.sum.max <- pseed.max.cycle%>% 
  left_join(pseed.mean, by = c("fish", "bl.s"))
view(pseed.sum.max)
#combined the two tibbles as pseed.sum.max


SE <- function(x) {
  sd(x)/sqrt(length(x))
}
#made a function that calculates standard error using standard deviation and the length of each vector (number of observations)

pseed.sum.max <- pseed.sum.max %>% 
  group_by(fish, bl.s) %>% 
  mutate(amp.sum.se = SE(amp.sum.max))
view(pseed.sum.max)
#calculated standard errors and added them to a new column "amp.sum.se"

pseed.sum.max %>% 
  group_by(fish, bl.s) %>% 
  ggplot(aes(x=bl.s, y=amp.sum.mean, colour=fish)) + 
  geom_errorbar(aes(ymin=amp.sum.mean-amp.sum.se, ymax=amp.sum.mean+amp.sum.se), width=.1) +
  geom_point() + geom_smooth(method="lm")
#plotted mean amp.sum vs specific swimming speed and included error bars
#also included linear regression line to better show trend/correlation


pseed.met.rate <- read.csv("pseed.met.rate.csv")
pseed.sum.max <- pseed.sum.max %>% 
  left_join(pseed.met.rate)
view(pseed.sum.max)
#merged new tibble, "pseed.met.rate.csv," to pseed.sum.max via left_join()

pseed.sum.max %>% 
  group_by(fish, bl.s) %>% 
  ggplot(aes(x=amp.sum.mean , y=met.rate,col=fish))+ geom_point()+geom_smooth(method="lm")
#plotted metabolic output of each fish vs mean max amp.sum using ggplot point graph
#also included linear regression line to better show trend/correlation
