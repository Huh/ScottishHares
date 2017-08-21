    #  Hare coat color change data exploration
    #  Josh Nowak/Marketa Zimova
    #  02/2017
################################################################################
    #  Packages
    require(R2jags)
    require(readr)
    require(lubridate)
    require(tidyr)
    require(dplyr)
################################################################################
    #  Josh wd
    setwd("C:/Users/josh.nowak/Documents/Marketa/data")
    
    #  Source utility funs
    source("C:/Users/josh.nowak/Documents/GitHub/ScottishHares/code/utility_function_joshOLD.R")

    #  Data
    setwd("D:/Scotland")
    rawd <- read_csv("Scotland molt phenology data_averages.csv")

    #  Format and clean data
    dat <- morph_data(rawd)
    
    #  Calculate observed proportions
    obs_prop <- dat %>%
      mutate_at(vars(D:LL), funs(./Total)) %>%
      summarise_at(vars(D:LL), mean, na.rm = T)

    #  Perpare data for JAGS
    j_data <- list(
      nobs = nrow(dat),
      ncat = 5,
      y = select(dat, D:LL),
      n = dat$Total,
      alpha = as.numeric(obs_prop)
    )
    
    #  Initial values
    j_inits <- function(){
      list(
        b = runif(5, 1, 3)
      )
    }
    
    #  Parameters to monitor
    j_param <- c("rats")
    
    #  Call
    fit <- jags(
      data = j_data,
      inits = j_inits,
      parameters.to.save = j_param,
      model.file = "C:/Users/josh.nowak/Documents/GitHub/ScottishHares/models/multinom_norandoms.txt",
      n.chains = 3,
      n.iter = 1000,
      n.burnin = 500,
      n.thin = 1,
      DIC = T,
      jags.module = c("glm", "dic")    
    )
    

    