#  Scottish hare data prep
################################################################################
#  Packages
#require(tidyr)
require(dplyr)
library(lubridate)
require(ggplot2)

setwd('/Users/marketzimova/Documents/WORK/DISSERTATION/2 Scotland/data')
rawd <-read.csv("Scotland molt phenology data_averagesNEW.csv", header=T,sep=",")

rawd$Date <- mdy(rawd$Date) 
rawd$Julian <- as.numeric(yday(rawd$Date))
seasons <- c("Fall", "Spring")

# Select best areas and observation with 1+ counts
stripped <- rawd %>%
  mutate(Color = (as.numeric(Color.code)), 
      Area = as.character(Area),
      Year = as.numeric(year(Date)),
      Month = as.numeric(month(Date)),
      Season = seasons[(Month %in% 1:7) + 1]) %>%   #1-7=spring
  filter(Area %in%  c("Coignafearn high","Coignafearn low","Lecht","Corndavon",
          "Gannochy","Glen Esk low","Glen Esk high","Glen Muick"), 
        Use!='no',
        Count>0) %>% 
  select(Area, Date, Count, Color, Season, Julian, Year) 
  
# Replicate by counts
replicated <- stripped[rep(row.names(stripped), stripped$Count), 1:7]
#View(arrange(replicated, Count,Date))

# Exploratory Plots
Lecht <-replicated  %>%
  filter(Area=="Lecht", Year==2015, Season=="Spring") %>%
  group_by(Date) %>%
  mutate(Avg.Color=mean(Color))
  ungroup(Lecht) 

ggplot(Lecht,aes(Date,Color)) +
  geom_point(aes(Date,Color, size=Count)) +
  geom_point(aes(Date,Avg.Color), color='red') 
 

