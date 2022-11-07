# compute_iqr.R

compute_iqr <-
  function( x ) {
    iqr <-quantile( x, 0.75 ) - quantile( x, 0.25 )
    names(iqr) <- "iqr"
    iqr
  }  