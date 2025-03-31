# Import relevant libraries
importLibraries <- function() {
  library(tidyverse)
  source(here('Code/Functions/Format.R'))
  source(here('Code/Functions/Rotate.R'))
  source(here('Code/Functions/GeneralFunctions.R'))
  source(here('Code/Functions/FormattingPlots.R'))
}

# Apply a smoothing spline along the length of the fish
SplineSmooth <- function(df){
 df |>
    get_arc_length() |>
    SplineIt()
}

# Get the arc length between points along the fish
get_arc_length <- function(df) {
  # Calculates arc length along the body
  df |>
    group_by(ID, frame) |>
    mutate(dx = lead(xsm.cm) - xsm.cm,
           dy = lead(ysm.cm) - ysm.cm,
           s = cumsum(sqrt(dx^2 + dy^2)),
           s = replace_na(lag(s), 0)) |>
    ungroup()
}

# estimate s0 from original mass calculations.
# Should be the same for every frame for one individual
SplineIt <- function(df) {
  df |>
    group_by(ID, frame) |>
    mutate(xs.cm = spline(s,xsm.cm, xout=s0)$y,
           ys.cm = spline(s,ysm.cm, xout=s0)$y,
           xs.cm = if_else(bodypart == "Peduncle" & is.na(xs.cm), xsm.cm, xs.cm),
           ys.cm = if_else(bodypart == "Peduncle" & is.na(ys.cm), ysm.cm, ys.cm))
}