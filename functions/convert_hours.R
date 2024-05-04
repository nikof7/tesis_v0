HourToDecimal <- function(datetime) {
  h <- hour(datetime)
  m <- minute(datetime)
  s <- second(datetime)
  return(h + m / 60 + s / 3600)
}

HourToRadians <- function(datetime) {
  decimal_time <- HourToDecimal(datetime)
  radian_time <- decimal_time * ((2 * pi)/24)
  return(radian_time)
}
