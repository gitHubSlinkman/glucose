# test_compute_statistics.R

require(here)                          # Needed to determine project location.
require(tidyverse)                     # I live in the tidyverse.
require(forcats)                       # Used to handle categorical variables
require(lubridate)                     # Needed for date manipulation
require(flextable)                     # For display.

source("D:/R-Projects/Diabetes/functions/compute_statistics.R")


################################################################################
# Get data.
################################################################################

project <- here()                      # Determine location in flie system.

file <- 
  file.path( project,                  # Construct file path to data file.               
             "data",
             "diabetes.csv" )

Readings <-                           # Read file into tibble. 
  read_csv( file )

spec( Readings )                      # Show tibble column specifications.

################################################################################
# Change date_time from character data to a datae_time onject.
################################################################################

Readings <- 
  Readings %>%
  mutate( date_time = 
            parse_date_time( 
              date_time,
              "YmdHM",
              locale = Sys.getlocale("LC_TIME" )))

compute_statistics( Readings, 7 )

compute_statistics( Readings, 14 )

compute_statistics( Readings, 30 )

