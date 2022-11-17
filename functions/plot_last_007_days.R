# plot_last_007_days.R

require( tidyverse )                        # I live in the tidyverse.
require( lubridate)                         # Tidyverse date functionality.

plot_last_007_days <-
  function( Readings ){
   
    days <- 7                                   # Number of days to plot
    
    day_to_seconds = 24 * 60 * 60               # Number of seconds in one day
    
    current_date_time <-                        # Get current date_time 
      now()
    
    begin_date_time   <-                        # Compute date 7 days ago.
      current_date_time - 
      days * day_to_seconds
    
    Last_7_Days <-                              # Get last 7 days activities.
      Readings %>% 
      filter( date_time > begin_date_time )
    
    from_date <-
      floor_date( begin_date_time,              # Compute lower break_label.
                  unit = "day" ) -
      day_to_seconds
    to_date <-
      ceiling_date( current_date_time,          # Compute upper break_label.
                    unit = "day" ) +
      day_to_seconds
    
    break_labels <-                              # Compute all break_labels.
      seq( from = from_date,
           to   = to_date,
           by   = day_to_seconds )
    
    
    ggplot( Last_7_Days,
            aes( x = date_time,
                 y = glucose )) +
      geom_line() +
      scale_x_datetime( name = "Date", 
                        breaks = break_labels,
                        labels = break_labels ) +
      ylab( "Glucose(ng/dl)") +
      ggtitle( paste( "Glucose readings from", 
                      as.Date( begin_date_time ),
                      "to",
                      as.Date( current_date_time ))) +
      theme( axis.text.x = element_text( size = 12, angle = 90))
  }