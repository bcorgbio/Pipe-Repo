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

pseed.wide <- pseed2 %>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)%>%
  print() 





exp1 <- pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")
f1 <-  features(x = exp1$frame,y=exp1$amp.bl)->f1

f2 <-  features(x = exp1$frame,y=exp1$amp.bl*100)
