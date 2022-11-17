# compute_mode.R
#
compute_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  m <- u[tab == max(tab)]
  l <- length(m)
  ifelse( l == 1, m[1], m[l])
}