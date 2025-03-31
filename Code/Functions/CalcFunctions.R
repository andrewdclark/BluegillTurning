# Import relevant libraries
importLibraries <- function() {
  library(tidyverse)
  library(lme4)
  library(car)
  library(ggspatial)
  library(ggdist)
  library(beeswarm)
  library(emmeans)
  source(here('Code/Functions/GeneralFunctions.R'))
  source(here('Code/Functions/CalcPlots.R'))
  source(here('Code/Functions/PecProcessingCode.R'))
  source(here('Code/Functions/MomentumCode.R'))
}

# Calculate the centroid
calcCentroid <- function(df){
  df |>
    group_by(ID,frame) |>
    mutate(centroidx = (sum(xs.cm*mass.g)/sum(mass.g)),
           centroidy = (sum(ys.cm*mass.g)/sum(mass.g)))
}

# Calculate angle change of the snout over time
calcAngChange <- function(df){
  df |>
    filter(bodypart == 'Snout' | bodypart == 'PelvFinsCenter') |>
    group_by(ID,t.sec, frame) |>
    mutate(snoutangle.rad = atan2(ys.cm[1] - ys.cm[2], xs.cm[1] - xs.cm[2])) |>
    ungroup() |>
    group_by(ID) |>
    mutate(snoutangle.rad = unwrap(snoutangle.rad)) |>
    mutate(tempsnoutangle.deg = snoutangle.rad * 180/pi) |> 
    group_by(ID) |> 
    mutate(bleh = case_when(min(tempsnoutangle.deg, na.rm = TRUE) < -100 ~ 'BLEH',
                            min(tempsnoutangle.deg, na.rm = TRUE) > -100 ~ 'ARGH')) |> 
    rowwise() |> 
    mutate(snoutangle.deg = case_when(bleh == 'BLEH' ~ -tempsnoutangle.deg,
                                      bleh == 'ARGH' ~ tempsnoutangle.deg)) |> 
    mutate(snoutangle.rad = snoutangle.deg * pi/180) |> 
    filter(bodypart == 'Snout')
}

# Calculate angular velocity of the snout
calcAngVel <- function (df, dt){
  df |>
    group_by(ID, frame, t.sec) |>
    reframe(snoutangle.rad = snoutangle.rad[1],
            snoutangle.deg = snoutangle.deg[1],
            bleh = bleh) |>
    group_by(ID) |>
    mutate(dang = lead(snoutangle.rad)-lag(snoutangle.rad),
           snoutangle.radps = dang/(2*dt),
           snoutangle.degps = snoutangle.radps * 180/pi)
}

# Calculate angular change of the tail over time
calcAngChangeTail <- function(df){
  df |>
    filter(bodypart == 'Peduncle' | bodypart == 'MidAnalBasePeduncle') |>
    group_by(ID,t.sec, frame) |>
    mutate(tailangle.rad = atan2(ys.cm[1] - ys.cm[2], xs.cm[1] - xs.cm[2])) |>
    ungroup() |>
    group_by(ID) |>
    mutate(tailangle.rad = unwrap(tailangle.rad)) |>
    mutate(temptailangle.deg = tailangle.rad * 180/pi) |> 
    group_by(ID) |> 
    mutate(bleh = case_when(min(temptailangle.deg, na.rm = TRUE) < -100 ~ 'BLEH',
                            min(temptailangle.deg, na.rm = TRUE) > -100 ~ 'ARGH')) |> 
    rowwise() |> 
    mutate(tailangle.deg = case_when(bleh == 'BLEH' ~ -temptailangle.deg,
                                     bleh == 'ARGH' ~ temptailangle.deg)) |> 
    mutate(tailangle.rad = tailangle.deg * pi/180)
}

# Calculate angular velocity of the tail
calcAngVelTail <- function (df, dt){
  df |>
    group_by(ID, frame, t.sec) |>
    summarize(tailangle.rad = tailangle.rad[1],
              tailangle.deg = tailangle.deg[1]) |>
    ungroup() |>
    group_by(ID) |>
    mutate(dang = lead(tailangle.rad)-lag(tailangle.rad),
           tailangle.radps = dang/(2*dt),
           tailangle.degps = tailangle.radps * 180/pi)
}

# Calculate moment of inertia
calcMoI <- function(df){
  dataMOIcentroid <- df %>%
    group_by(ID,frame) %>%
    mutate(centroidx = (sum(xs.cm*mass.g)/sum(mass.g)),
           centroidy = (sum(ys.cm*mass.g)/sum(mass.g)))
  
  dataMoI <- dataMOIcentroid %>%
    group_by(ID,frame) %>%
    mutate(MoI.gcm2 = sum((sqrt((xs.cm - centroidx)^2 + (ys.cm - centroidy)^2)^2*mass.g)))
  
  dataMoI |> 
    select(c('Individual','ID','Speed.group','frame','t.sec','bodypart','MoI.gcm2')) |> 
    group_by(ID, frame) |> 
    summarize(Individual = first(Individual),
              ID = first(ID),
              Speed.group = first(Speed.group),
              frame = first(frame),
              t.sec = first(t.sec),
              MoI.gcm2 = first(MoI.gcm2))
}

# Calculate each individual's theoretical maximum moment of inertia
calcMaxMoI <- function(df){
  df <- df |> 
    rowwise() |> 
    mutate(ys.cm = 0,
           xs.cm = s0) |> 
    group_by(Individual) |> 
    filter(ID == first(ID)) |> 
    filter(frame == min(frame, na.rm = TRUE))
  
  dataMOIcentroid <- df %>%
    group_by(Individual) %>%
    mutate(centroidx = (sum(xs.cm*mass.g)/sum(mass.g)),
           centroidy = (sum(ys.cm*mass.g)/sum(mass.g)))
  
  dataMOIcentroid %>%
    group_by(Individual) %>%
    filter(frame == min(frame, na.rm = TRUE)) |> 
    mutate(MaxMoI.gcm2 = sum((sqrt((xs.cm - centroidx)^2 + (ys.cm - centroidy)^2)^2*mass.g))) |> 
    group_by(Individual) |> 
    summarise(Individual = first(Individual),
              MaxMoI.gcm2 = first(MaxMoI.gcm2))
}

# Smooth the change in the snout angle over time
runSnoutAngButter <- function(df,bf){
  df|>
    group_by(ID) |>
    mutate(snoutangle.deg = EndEffect(snoutangle.deg,bf),
           snoutangle.rad = EndEffect(snoutangle.rad,bf))
}

# Smooth the change in the tail angle over time
runTailAngButter <- function(df,bf){
  df|>
    group_by(ID) |>
    mutate(tailangle.rad = EndEffect(tailangle.rad, bf),
           tailangle.deg = EndEffect(tailangle.deg, bf))
}

# Read in the pectoral fin files and preserve the filename
read_csv_filename <- function(filename){
  df <- read.csv(filename)
  df$Filename <- filename
  df
}

# Calculate torque
calcTorque <- function(df){
  df |>
    group_by(ID, frame) |>
    arrange(ID, frame, bodypart) |> 
    group_by(ID, frame) |> 
    mutate(segang.rad = atan2(lead(ys.cm) - ys.cm, lead(xs.cm) - xs.cm)) |> 
    group_by(ID, bodypart) |> 
    mutate(segang.rad = unwrap(segang.rad, na.rm = TRUE)) |> 
    group_by(ID, bodypart) |> 
    mutate(segangvel.radps = nderiv(segang.rad, dt),
           segangaccel.radps2 = nderiv(segangvel.radps,dt)) |> 
    calcCentroid() |>
    ungroup() |>
    mutate(r.cm = sqrt((ys.cm-centroidy)^2 + (xs.cm - centroidx)^2),
           r2.cm2 = r.cm^2) |>
    group_by(ID, bodypart) |>
    mutate(rdot.cmps = nderiv(r.cm,dt)) |> 
    group_by(ID, frame) |> 
    summarize(torque.gcm2ps2 = sum(mass.g*(2*r.cm*segangvel.radps*rdot.cmps + r2.cm2*segangaccel.radps2), na.rm = TRUE),
              torque.kgm2ps2 = (torque.gcm2ps2/10000)/1000,
              torque.milliNm = torque.kgm2ps2 * 1000,
              torque.microNm = torque.kgm2ps2 * 1000000,
              t.sec = first(t.sec))
}

# Calculate mean snout velocity across trials within a set speed
calcMeanSnoutVelocity <- function(df,speed,t){
  df |> 
    group_by(ID) |> 
    mutate(framemax = frame[which.max(snoutangle.degps)],
           newframe = frame - framemax) |>
    ungroup() |> 
    group_by(Speed.group, newframe) |> 
    mutate(meansnoutvel.degps = mean(snoutangle.degps, na.rm = TRUE))|> 
    arrange(newframe) |> 
    group_by(Speed.group,newframe) |> 
    summarise(ID = first(ID),
              newframe = first(newframe),
              newT = newframe * dt,
              meansnoutvel.degps = first(meansnoutvel.degps)) |> 
    filter(Speed.group == speed & newT < t & newT > -t)
}

# Calculate mean moment of inertia across trials within a set speed
calcMeanMoI <- function(df, speed, t){
  df |> 
    group_by(ID) |> 
    mutate(percMoI = MoI.gcm2/MaxMoI.gcm2,
           framemax = frame[which.max(snoutangle.degps)],
           newframe = frame - framemax) |>
    ungroup() |> 
    group_by(Speed.group, newframe) |> 
    mutate(meanpercMoI = mean(percMoI, na.rm = TRUE))|> 
    arrange(newframe) |> 
    group_by(Speed.group,newframe) |> 
    summarise(ID = first(ID),
              newframe = first(newframe),
              newT = newframe * dt,
              meanpercMoI = first(meanpercMoI)) |> 
    filter(Speed.group == speed & newT < t & newT > -t)
}

# Calculate both mean snout angular velocity and mean moment of inertia for each trial
calcMeanMoIMeanSnoutVel <- function(df){
  df |> 
    group_by(ID) |> 
    summarise(meansnoutangle.degps = mean(snoutangle.degps, na.rm = TRUE)) |> 
    mutate(Individual = substr(ID,1,4)) |> 
    right_join(MoI, by = c('ID')) |> 
    group_by(ID) |> 
    mutate(percMoI = MoI.gcm2/(max(MoI.gcm2, na.rm = TRUE)),
           meanpercMoI = mean(percMoI)) |> 
    group_by(ID) |> 
    summarize(ID = first(ID),
              meansnoutangle.degps = first(meansnoutangle.degps),
              meanpercMoI = first(meanpercMoI)) |> 
    mutate(Individual = substr(ID,1,4))
}

# Trim the data to the relevant frames
trimFrames <- function(df){
  df |> 
    group_by(ID) |> 
    filter(frame >= 0 & frame <= (max(frame)-5))
}