# plot_time_series.R

require( tidyverse )                        # I live in the tidyverse.
require( lubridate )                        # For date computations.

plot_time_series <-
  function( Readings,
            days ){
    
    day_to_seconds = 24 * 60 * 60           #  Number of seconds in day.
    
    current_date_time <-                    # Get current date_time 
      now()
    
    begin_date_time   <-                    # Compute date 7 days ago.
      current_date_time - 
      days * day_to_seconds
    
    x_min_date <-                         # Compute minimum x-axis value.
      floor_date( 
        begin_date_time,
        unit = "day" )
    
    x_max_date <-                           # Compute the maximum date.
      ceiling_date( 
        current_date_time,
        unit = "day" )
    
    
    Readings <-                              # Get all readings since the begin
      Readings %>%                           #  date.
        filter( date_time > 
                  begin_date_time )
    
    n <- dim(Readings )[1]                   # Get number of observations.
    
    if(  n <=  14 ) {                        # Determine the number of date
      increment = 1                          #   labels to time-series plot of
    } else if ( n <= 28) {n                  #   glucose readings.
     increment = 2
    } else if ( n <= 60 ) {
      increment <- 7
    } else if ( n <= 180 ) {
      increment <- 14
    } else {
      increment <- 30
    }
    
    xbl <-                                  # Compute x-axis breaks and labels.
      seq( from = x_min_date,
           to   = x_max_date,
           by   = increment *  
             day_to_seconds )

    
  }