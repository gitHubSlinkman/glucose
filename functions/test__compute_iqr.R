# test__compute_iqr.R

source("D:/R-Projects/Diabetes/functions/compute_iqr.R")

x <- rnorm( 1000, 
            mean = 75,
            sd = 25/4  )
compute_iqr(x)
