# Initial data formatting
formatData <- function(df,mass){
  df |> 
    trimDF() |> 
    createIDvariables() |> 
    createTime() |> 
    convert2cm() |> 
    left_join(mass, by = c('Individual', 'bodypart'))
}

# Calculate mass of each body segment
calcPointMasses <- function(df){
  
  df |> 
    mutate(w1 = .5 * w1,
           w2 = .5 * w2,
           h1 = .5 * h1,
           h2 = .5 * h2,
           dl = l,
           dw = w2 - w1,
           dh = h2 - h1,
           mass.g = pi * dl * (w1 * h1 + 0.5*dw*h1 + 0.5*dh*w1 + 0.333333*dw*dh)) |> 
    mutate(TotalMass = sum(mass.g),
           TotalLength = sum(l)) |> 
    select(c(-w1,-w2,-h1,-h2,-l,-dl,-dw,-dh,-TotalLength, -TotalMass))
  
}

# Trim out unnecessary variables from the dataframe
trimDF <- function(df){
  df |> 
    select(c(video, frame, node, x_3D, y_3D, z_3D)) |> 
    rename(x.mm = x_3D,
           y.mm = y_3D,
           z.mm = z_3D,
           bodypart = node) 
}

# Create the ID variables for each trial
createIDvariables <- function(df){
  df |> 
    group_by(video) |> 
    mutate(trialStart = head(unlist(gregexpr('Tr', video)), n=1),
           turnStart = head(unlist(gregexpr('Tu', video)), n=1),
           FirstUnder = head(unlist(gregexpr('_', video)), n=1),
           SecondUnder = tail(unlist(gregexpr('_', video)), n=1),
           blsStart = head(unlist(gregexpr('BLS', video)), n=1),
           Individual = substr(video,1,FirstUnder - 1),
           Trial = as.character(substr(video,FirstUnder + 3,turnStart - 1)),
           Turn = as.character(substr(video,turnStart + 2, SecondUnder - 1)),
           Speed.BLs = as.character(substr(video,SecondUnder + 1, blsStart - 1)),
           ID = str_c(Individual, '_', Trial, '_', Turn)) |> 
    ungroup() |> 
    select(c(-trialStart, -turnStart, -FirstUnder, -SecondUnder, -blsStart, -video))
}

# Create time variable
createTime <- function(df) {
  df |>
    mutate(t = frame/60-1/60)
}

# Convert from mm to cm
convert2cm <- function(df){
  df |>
    mutate(x.cm = x.mm/10,
           y.cm = y.mm/10,
           z.cm = z.mm/10) |> 
  select(-x.mm, -y.mm, -z.mm)
}

# Trim the video based on the user identified turn frames
MatchTurnFrames <- function(df, turndf) {
  turndf <- turndf |> 
    select(-Individual, -Trial, -Turn)
  
  df <- df |> 
    left_join(turndf, by = 'ID') |>
    group_by(ID) |> 
    filter(frame >= StartFrame - 5 & frame <= EndFrame + 5) |> 
    select(-StartFrame, -EndFrame)
}
   