# compute_statistics.R

require(here)                          # Needed to determine project location.
require(tidyverse)                     # I live in the tidyverse.
require(forcats)                       # Used to handle categorical variables
require(lubridate)                     # Needed for date manipulation
require(flextable)                     # Provides table display functionality

source("D:/R-Projects/RStatistics/compute_mode.R")
source("D:/R-Projects/glucose/functions/compute_iqr.R")

compute_statistics <- 
  function( dftbl, days ){
    
    first_date <-                       # Compute first day
      now() - days * 24 * 60 * 60 

      Summary <-
        dftbl %>% 
          filter( date_time > first_date ) %>% 
            summarise( Days     = days,
                       Since    = as.Date( first_date ),
                       Readings = n(),
                       Mean     = round(   mean( glucose ), 1),
                       Std      = round(     sd( glucose ), 1),
                       CV       = round( 100 * Std/ Mean, 1 ),
                       Median   = round( median( glucose ), 1),
                       Mode     = compute_mode( glucose ),
                       IQR      = round( quantile( glucose, 0.75) -
                                         quantile( glucose, 0.25 )),
                       Minimum  = min( glucose ),
                       Maximum  = max( glucose ),
                       High = sum( if_else( glucose > 130, 1, 0)))
      Summary
    }
