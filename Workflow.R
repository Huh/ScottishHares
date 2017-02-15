    #  Hare coat color change data exploration
    #  Josh Nowak/Marketa Zimova
    #  02/2017
################################################################################
    #  Packages
    require(readr)
    require(tidyr)
    require(dplyr)
################################################################################
    #  Data
    setwd("D:/Scotland")
    rawd <- read_csv("Scotland molt phenology data_averages.csv")

    #  Format data
    dat <- morph_data(rawd)
    
    