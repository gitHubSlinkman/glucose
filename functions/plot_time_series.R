# plot_time_series.R

require( tidyverse )                        # I live in the tidyverse.
require( lubridate )                        # For date computations.

plot_time_series <-
  function( Readings,
            days ){
    
    day_seconds = 24 * 60 * 60              #  Number of seconds in day.
    
    current_date <-                         # Get current date. 
      ceiling_date( 
        now(),
        unit = "day" )
    
    begin_date <-                           # Compute date 7 days ago.
      floor_date(
        current_date - 
          days *day_seconds,
          unit = "day" )
    
    Readings <-                              # Get all readings since the begin
      Readings %>%                           #  date.
        filter( date_time > 
                  begin_date )
    
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
      seq( from = begin_date,
           to   = current_date,
           by   = increment * day_seconds )
    
    ybl <- seq( from = 50,
                to   = 250,
                by = 25 )
    
    ybar <-
      mean( Readings %>% 
          pull( glucose ))
    
    ggplot( Readings,                       # Plot time series and mean.
            aes( x = date_time,
                 y = glucose )) +
      geom_line() +
      geom_hline( yintercept = ybar,
                  color    = "darkblue",
                  stat    = "unique" ) +
      scale_x_datetime( name = "Date", 
                        breaks = xbl,
                        labels = xbl  ) +
      scale_y_continuous(limits = c( 50, 250)) +
      annotate( x = min(xbl) - 1.0 * day_seconds,
                y    = ybar,
                geom = "text",
                label = "bar(y)",
                parse = TRUE,
                vjust = 0 ) +
      ylab( "Glucose(ng/dl)") +
      ggtitle( paste( "Glucose readings from", 
                      as.Date( begin_date ),
                      "to",
                      as.Date( current_date ))) +
      theme( axis.text.x = element_text( size = 10, angle = 90 ))
  }