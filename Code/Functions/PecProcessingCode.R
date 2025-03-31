

pecfinformat <- function(df){
  df |> select(c(Filename, frame_idx, LeftPecBase.x, LeftPecBase.y, LeftPecTip.x, LeftPecTip.y, RightPecBase.x, RightPecBase.y, RightPecTip.x, RightPecTip.y))
  
  df |> 
    group_by(Filename) |> 
    mutate(under = head(unlist(gregexpr('_', Filename)), n=1),
           dash = head(unlist(gregexpr('-', Filename)), n=1)) |> 
    rowwise() |> 
    mutate(Filename = as.character(substr(Filename, under + 1, dash - 1))) |> 
    mutate(trialStart = head(unlist(gregexpr('Tr', Filename)), n=1),
           turnStart = head(unlist(gregexpr('Tu', Filename)), n=1),
           FirstUnder = head(unlist(gregexpr('_', Filename)), n=1),
           SecondUnder = tail(unlist(gregexpr('_', Filename)), n=1),
           blsStart = head(unlist(gregexpr('BLS', Filename)), n=1),
           Individual = substr(Filename,1,FirstUnder - 1),
           Trial = as.character(substr(Filename,FirstUnder + 3,turnStart - 1)),
           Turn = as.character(substr(Filename,turnStart + 2, SecondUnder - 1)),
           Speed.BLs = as.character(substr(Filename,SecondUnder + 1, blsStart - 1)),
           ID = str_c(Individual, '_', Trial, '_', Turn)) |> 
    ungroup() |> 
    select(c(ID, Individual, frame_idx, LeftPecBase.x, LeftPecBase.y, LeftPecTip.x, LeftPecTip.y, RightPecBase.x, RightPecBase.y, RightPecTip.x, RightPecTip.y)) |> 
    rename(frame = frame_idx,
           LeftPecBase_x = LeftPecBase.x,
           LeftPecBase_y = LeftPecBase.y,
           LeftPecTip_x = LeftPecTip.x,
           LeftPecTip_y = LeftPecTip.y,
           RightPecBase_x = RightPecBase.x,
           RightPecBase_y = RightPecBase.y,
           RightPecTip_x = RightPecTip.x,
           RightPecTip_y = RightPecTip.y,
           ID = ID) |> pivot_longer(cols = c(-ID,-frame),
                                    names_to = c('bodypart','.value'),
                                    names_sep = '_') |> 
    rename(x.pix = x,
           y.pix = y) |> MatchTurnFramesPecFins(pecfinturndetails)
}

MatchTurnFramesPecFins <- function(df, turndf) {
  df <- df |> 
    left_join(turndf, by = 'ID') |>
    group_by(ID) |> 
    filter(frame >= StartFrame & frame <= EndFrame) |> 
    select(-StartFrame, -EndFrame)
}

pecfinvectorcalc <- function(df){
  
  
  bothbodyvectorsleft <- df |> 
    group_by(ID, frame) |> 
    filter(fin == 'Both') |> 
    filter(bodypart == 'LeftPecBase' | bodypart == 'RightPecBase') |> 
    mutate(bodyvect.x = lead(x.pix) - x.pix,
           bodyvect.y = lead(y.pix) - y.pix,
           bv.length = sqrt(bodyvect.x^2 + bodyvect.y^2),
           unitbodyvect.x = bodyvect.x/bv.length,
           unitbodyvect.y = bodyvect.y/bv.length,
           fin = 'Left') |> 
    na.omit() |> 
    select(c(ID, frame, unitbodyvect.x, unitbodyvect.y,fin,OutsideFin))
  
  bothbodyvectorsright <- df |> 
    group_by(ID, frame) |> 
    filter(fin == 'Both') |> 
    filter(bodypart == 'LeftPecBase' | bodypart == 'RightPecBase') |> 
    mutate(bodyvect.x = x.pix - lead(x.pix),
           bodyvect.y = y.pix - lead(y.pix),
           bv.length = sqrt(bodyvect.x^2 + bodyvect.y^2),
           unitbodyvect.x = bodyvect.x/bv.length,
           unitbodyvect.y = bodyvect.y/bv.length,
           fin = 'Right') |> 
    na.omit() |> 
    select(c(ID, frame, unitbodyvect.x, unitbodyvect.y,fin,OutsideFin))
  
  bodyvectors <- df |> 
    group_by(ID, frame) |> 
    filter(bodypart == 'LeftPecBase' | bodypart == 'RightPecBase') |> 
    mutate(bodyvect.x = case_when(fin == 'Left' ~ lead(x.pix) - x.pix,
                                  fin == 'Right' ~ x.pix - lead(x.pix)),
           bodyvect.y = case_when(fin == 'Left' ~ lead(y.pix) - y.pix,
                                  fin == 'Right' ~ y.pix - lead(y.pix)),
           bv.length = sqrt(bodyvect.x^2 + bodyvect.y^2),
           unitbodyvect.x = bodyvect.x/bv.length,
           unitbodyvect.y = bodyvect.y/bv.length) |> 
    na.omit() |> 
    select(c(ID, frame, unitbodyvect.x, unitbodyvect.y,fin,OutsideFin)) |> 
    rbind(bothbodyvectorsleft) |> 
    rbind(bothbodyvectorsright)
  
  
  leftpecvectors <- df |> 
    filter(fin == 'Left' | fin == 'Both') |> 
    group_by(ID, frame) |> 
    filter(bodypart == 'LeftPecBase' | bodypart == 'LeftPecTip') |> 
    mutate(pecvect.x = lead(x.pix) - x.pix,
           pecvect.y = lead(y.pix) - y.pix,
           pv.length = sqrt(pecvect.x^2 + pecvect.y^2),
           unitpecvect.x = pecvect.x/pv.length,
           unitpecvect.y = pecvect.y/pv.length,
           fin = 'Left') |> 
    na.omit() |> 
    select(c(ID, frame, unitpecvect.x, unitpecvect.y,fin))
  
  rightpecvectors <- df |> 
    filter(fin == 'Right' | fin == 'Both') |> 
    group_by(ID, frame) |> 
    filter(bodypart == 'RightPecBase' | bodypart == 'RightPecTip') |> 
    mutate(pecvect.x = lead(x.pix) - x.pix,
           pecvect.y = lead(y.pix) - y.pix,
           pv.length = sqrt(pecvect.x^2 + pecvect.y^2),
           unitpecvect.x = pecvect.x/pv.length,
           unitpecvect.y = pecvect.y/pv.length,
           fin = 'Right') |> 
    na.omit() |> 
    select(c(ID, frame, unitpecvect.x, unitpecvect.y,fin))
  
  leftpecvectors |> rbind(rightpecvectors) |> right_join(bodyvectors, by = c('ID','frame','fin'))
  
}

pecanglescalc <- function(df){

df |> 
  rowwise() |> 
  mutate(crossproduct = unitbodyvect.x * unitpecvect.y - unitbodyvect.y * unitpecvect.x,
         dotproduct = unitbodyvect.x * unitpecvect.x + unitbodyvect.y * unitpecvect.y,
         angle.rad = acos(dotproduct),
         angle.deg = angle.rad * 180/pi,
         pecangle.rad = case_when(fin == 'Left' & crossproduct < 0 ~ angle.rad - pi/2,
                                  fin == 'Left' & crossproduct > 0 ~ (3*pi)/2 - angle.rad,
                                  fin == 'Left' & crossproduct == 0 ~ pi/2,
                                  fin == 'Right' & crossproduct > 0 ~ angle.rad - pi/2,
                                  fin == 'Right' & crossproduct < 0 ~ (3*pi)/2 - angle.rad,
                                  fin == 'Right' & crossproduct == 0 ~ pi/2),
         pecangle.deg = pecangle.rad *180/pi) |> 
  select(ID,frame,fin,OutsideFin,pecangle.rad,pecangle.deg)
  
}

calcPecStrokeNumber <- function(df, maindata){
  leftpecstroke <- df |> 
    filter(bodypart == 'LeftPecTip') |> 
    filter(!is.na(x.pix)) |> 
    group_by(ID) |> 
    summarise(ID = first(ID),
              Side = 'Left',
              NumOfStrokes = length(x.pix))
  
  rightpecstroke <- df |> 
    filter(bodypart == 'RightPecTip') |> 
    filter(!is.na(x.pix)) |> 
    group_by(ID) |> 
    summarise(ID = first(ID),
              Side = 'Right',
              NumOfStrokes = length(x.pix))
  
  PecStrokeCount <- leftpecstroke |>
    rbind(rightpecstroke) |> 
    select(c(ID, Side, NumOfStrokes)) |>  
    AddBackSpeed(maindata) |> 
    na.omit()
  
  PecStrokeCount <- PecStrokeCount |> 
    arrange(ID) |> 
    group_by(ID) |> 
    summarise(ID = first(ID),
              TotalPecStrokes = sum(NumOfStrokes),
              Speed.group = first(Speed.group),
              Individual= substr(ID,1,4))
}

calcPecFinAngles <- function(df){
  df |> 
    group_by(ID, frame) |> 
    na.omit() |> 
    mutate(fin = case_when('LeftPecTip' %in% bodypart & !('RightPecTip' %in% bodypart) ~ 'Left',
                           'RightPecTip' %in% bodypart & !('LeftPecTip' %in% bodypart) ~ 'Right',
                           .default = 'Both')) |>
    pecfinvectorcalc() |> arrange(ID,frame,fin) |> 
    pecanglescalc() |> 
    AddBackSpeed(maindata) |> 
    mutate(Individual= substr(ID,1,4)) |> 
    na.omit() |> 
    rowwise() |> 
    mutate(side = case_when(fin == OutsideFin ~ 'Outside',
                            fin != OutsideFin ~ 'Inside'))
}


calcNumOfBackingStrokes <- function(df,threshold){
  df |> 
    filter(pecangle.deg > threshold) |> 
    group_by(ID) |> 
    mutate(tobesummed = 1) |> 
    summarise(ID = first(ID),
              Speed.group = first(Speed.group),
              numberofbackingstrokes = sum(tobesummed),
              Individual= substr(ID,1,4))
}
