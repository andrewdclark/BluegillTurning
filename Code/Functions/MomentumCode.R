calcAngularMomentum <- function(df, moi){
  df |> 
    left_join(moi, by = c('ID','frame')) |> 
    select(c(ID,Speed.group,frame,t.sec,snoutangle.radps,
             snoutangle.degps, MoI.gcm2)) |> 
    rowwise() |> 
    mutate(momentum.gradps = snoutangle.radps * MoI.gcm2,
           momentum.gdegps = snoutangle.degps * MoI.gcm2) |> 
    select(c(ID,Speed.group,frame,t.sec,momentum.gradps,momentum.gdegps))
}

prepLinearMomentum <- function(df){
  df |> 
    calcCentroid() |> 
  select(ID, frame, centroidx, centroidy) |> 
  group_by(ID, frame) |> 
  summarise(centroidx = first(centroidx),
            centroidy = first(centroidy))
}

calcLinearMomentum <- function(df, massdf){
  indmassdata <- massdf |> 
    group_by(ID, Individual, frame) |> 
    summarise(indmass.g = sum(mass.g)) |> 
    group_by(ID) |> 
    summarise(ID = first(ID),
              Individual = first(Individual),
              indmass.g = first(indmass.g))
  
  df |> 
    left_join(indmassdata, by = 'ID')|> 
    group_by(ID) |> 
    mutate(dx = lead(centroidx) - lag(centroidx),
           dy = lead(centroidy) - lag(centroidy),
           linearspeed.cmps = sqrt(dx^2 + dy^2)/(2*dt),
           linearmomentum.gcmps = linearspeed.cmps * indmass.g) |> 
    select(c(-dx, -dy))
}

normalizeMomentum <- function(df){
  df |> 
  mutate(momentum.gnormdegps = momentum.gdegps/(FullBL^4),
  linearmomentum.gnormcmps = linearmomentum.gcmps/(FullBL^4)) |> 
    select(ID,Individual,Speed.group,frame,t.sec,momentum.gnormdegps,linearmomentum.gnormcmps)
}

calcInitialMomentum <- function(df){
df |> 
    group_by(ID) |> 
    filter(frame < 0) |> 
    group_by(ID) |> 
    mutate(Time = 'Pre') |> 
    group_by(ID, Time) |> 
    summarise(Individual = first(Individual),
              Speed.group = first(Speed.group),
              meanmomentum.gnormdegps = mean(momentum.gnormdegps, na.rm = TRUE),
              meanlinearmomentum.gnormcmps = mean(linearmomentum.gnormcmps, na.rm = TRUE))
}