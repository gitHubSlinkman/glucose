
require(here)
require(tidyverse)
require(lubridate)
require(flextable)
require(cowplot)

#
theme_set(theme_cowplot())
# 
project <- here()
file <- 
  file.path( project,
             "data",
             "diabetes.csv")
readings <- 
  read_csv( file )

spec(readings)

readings <- 
  readings %>%
    mutate( date_time = 
              parse_date_time( date_time,
                               "YmdHM",
                               locale = Sys.getlocale("LC_TIME" )))




flextable( readings ) %>% 
  autofit()

y_scale <- seq(from = 50,
                to  = 200,
                by = 10 )

ggplot( readings,
        aes( x   = date_time,
             y   = glucose )) +
  geom_line() +
  geom_point( aes( color = description)) +
  xlab( "Date-Time" ) +
  ylab( "Glucose(mg/dl)") +
  ggtitle("Daily blood gucose readings") +
  scale_y_continuous( breaks = y_scale, 
                      labels = y_scale,
                      limits = c( 60, 200 ))

ggplot( readings,
        aes( x= description,
             y = glucose )) +
        geom_boxplot( na.rm          = TRUE,
                      outlier.colour = "red", 
                      outlier.shape  = 16,
                      outlier.size   = 2, 
                      notch          =TRUE ) +
  scale_y_continuous( breaks = y_scale, 
                      labels = y_scale,
                      limits = c( 60, 200 ))
        
  
  
glucose <-
  readings %>% 
    pull(glucose)

glucose <- 
  glucose[!is.na(glucose)]

n <- length(glucose)
n

glucose_mean  <- mean (glucose,
                       na.rm =TRUE )
glucose_std   <- sd( glucose,
                     na.rm =TRUE )

glucose_median <- median( glucose,
                          na.rm = TRUE )
glucose_range  <- max( glucose ) - min( glucose,
                                        na.rm =TRUE )
glucose_iqr    <- quantile( glucose, 0.75,
                            na.rm =TRUE ) -
                  quantile( glucose, 0.25,
                            na.rm =TRUE )

source("D:/R-Projects/RStatistics/compute_mode.R")
glucose_mode <- compute_mode( glucose )

stats_location <- tibble( Mean = glucose_mean,
                          Median = glucose_median,
                          Mode   = glucose_mode )
stats_location 


  
q <- c( 0.000,
        0.010, 
        0.025,
        0.050,
        0.100,
        0.250,
        0.500,
        0.750,
        0.900,
        0.950,
        0.975,
        0.990,
        0.995,
        0.999,
        1.000 )

quantiles <- round( quantile( glucose, 
                              prob = q, 
                              type=8,
                              na.rm = TRUE ), 
                              1 )
quantiles

stats <- tibble( ybar, glucose_median, glucose_mode )

stats <- as_tibble( stats)

ggplot( readings,
  aes(  x =  description )) +
  geom_bar( color = "black",
            fill  = "green" ) 
 
  

source("D:/R-Projects/Rsource/plot_distribution.R")
plot_distribution( glucose, "Glucose(mg/dl", "Glucose Readings")

source("D:/R-Projects/Rsource/plot_qq_norm.R")
plot_qq_norm( glucose, R = 100,
              variable_name = "Glocose(mg/dl)",
              "QQ Plot of glucose") 


