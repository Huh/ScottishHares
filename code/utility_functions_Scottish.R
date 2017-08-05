#  Scottish hare data prep
################################################################################

morph_data <- function(x){
  
seasons <- c("Fall", "Spring")

# Select best areas and observation with 1+ counts
stripped <- rawd %>%
  mutate(Color = (as.numeric(Color.code)), 
      #Area = as.character(Area),
      CameraNum = as.integer(as.factor(Area)),
      Date = mdy(Date), 
      Julian = as.numeric(yday(Date)),
      Year = as.numeric(year(Date)),
      Month = as.numeric(month(Date)),
      Season = seasons[(Month %in% 1:7) + 1]) %>%   #1-7=spring
  filter(Area %in%  c("CoigH","CoigL","Lecht","Corn",
          "Punch","EskL","EskH","Muick"), 
        Use!='no',
        Count>0) %>% 
  select(Area, Date, Count, Color, Season, Julian, Year,CameraNum)  
}

# Replicate by counts
#replicated <- stripped[rep(row.names(stripped), stripped$Count), 1:7]
#View(arrange(replicated, Count,Date))
# 
# 

