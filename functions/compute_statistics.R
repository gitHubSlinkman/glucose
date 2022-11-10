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
          filter( Date_time > first_date ) %>% 
            summarise( Days     = days,
                       Since    = as.Date( first_date ),
                       Readings = n(),
                       Mean     = round(   mean( Glucose ), 1),
                       Std      = round(     sd( Glucose ), 1),
                       CV       = round( 100 * Std/ Mean, 1 ),
                       Median   = round( median( Glucose ), 1),
                       IQR      = round( quantile( Glucose, 0.75) -
                                         quantile( Glucose, 0.25 )),
                       Minimum  = min( Glucose ),
                       Maximum  = max( Glucose ),
                       High = sum( if_else( Glucose > 130, 1, 0)))
      Summary
    }
