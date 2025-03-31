# Calculate the length of a vector
VectLength <- function(vect){
  sqrt(vect[1]^2 + vect[2]^2 + vect[3]^2)
}

# Take the derivative
nderiv <- function(y, dt)
{
  (lead(y) - lag(y)) / (2*dt)
}

# Deal with end effects
EndEffect <- function(x,filt) {
  signal::filtfilt(filt,c(rev(x),x,rev(x)))[(length(x) + 1):(2 * length(x))]
}

# run butterworth filter
runXYZButter <- function(df,bf){
  
  df|>
    group_by(ID, bodypart) |>
    mutate(xsm.cm = EndEffect(xr.cm,bf),
           ysm.cm = EndEffect(yr.cm,bf),
           zsm.cm = EndEffect(zr.cm,bf))
  
}

unwrap <- function(y, modulus = 2*pi, jump = 0.5, na.rm = FALSE) {
  #' Unwraps angle data to remove discontinuities.
  #' 
  #' With angle data mod 2pi, the angles can jump discontinuously from
  #' pi to -pi or -pi to pi. This removes those discontinuities by adding
  #' or subtracting 2pi when there is a jump.
  #' 
  #' @param y Time series angle data.
  #' @param modulus Modulus of the data. For angle data in radians, use
  #'    2pi. Could also be 360 for angle data in degrees or 1 for data
  #'    mod 1.
  #' @param jump What fraction of the modulus constitutes a jump. Default
  #'    is 0.5, but you could set it up to 0.9 or more to be stricter.
  #' @param na.rm Remove NA values before calculating jumps (or not)
  
  if (na.rm) {
    good = !is.na(y)
  } else {
    good = rep_len(TRUE, length(y))
  }
  
  y1 <- y[good]
  
  dy <- y1 - lag(y1)
  dy[1] <- 0
  
  step <- rep_len(0, length(y))
  
  # look for the jumps
  step[good] = case_when(dy < -jump*modulus  ~  modulus,
                         dy > jump*modulus  ~  -modulus,
                         TRUE  ~  0)
  # steps are cumulative, so make sure to add them up
  step = cumsum(step)
  
  # then add the steps back on to the original data
  y + step
}

# Add back speed
AddBackSpeed <- function(df,speeddf) {
  dataJoin <- speeddf %>%
    select(ID, Speed.BLs, Speed.group)
  dataJoin <- distinct(dataJoin)
  
  df %>%
    left_join(dataJoin, by = "ID")
}
