# Plots for identifying problematic individuals with the x point
ProblemIndPlotX <- function(df){
  df |> 
    ggplot(aes(x = frame, y = xsm.cm))+
    geom_point() +
    labs(title = first(df$ID))
}

# Plots for identifying problematic individuals with the y point
ProblemIndPlotY <- function(df){
  df |> 
    ggplot(aes(x = frame, y = ysm.cm))+
    geom_point() +
    labs(title = first(df$ID))
}

# Plots for identifying problematic individuals with the x point post splining
ProblemIndPlotXspl <- function(df){
  df |> 
    ggplot(aes(x = frame, y = xs.cm))+
    geom_point() +
    labs(title = first(df$ID))
}

# Plots for identifying problematic individuals with the y point post splining
ProblemIndPlotYspl <- function(df){
  df |> 
    ggplot(aes(x = frame, y = ys.cm))+
    geom_point() +
    labs(title = first(df$ID))
}

