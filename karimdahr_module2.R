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


separate(date,)

f1 <-  features(x = exp1$frame,y=exp1$amp.bl)->f1

find.peaks <- function(date$11,mult=100){ #define the functions parameter/inputs:x,y, and how much we won't to multiple y by (remember the rounding issue)
  f <- fget(features(x = x,y=y*mult))[2:3]%>% #store results in `f` and compute the features for the x-y relationship, wrap in in fget to retrieve the important features, subset the results to take the 2nd and 3rd and  items, the critical points and curvature, then pass it to a tibble
    as_tibble()%>% # pass in through a filter that returns curvatures <0
    filter(curvature<0)%>% #add a column that rounds the critical point to an integer that represents the frame
    mutate(peaks=round(crit.pts,0))
  return(f$peaks) # return the peaks from tibble
