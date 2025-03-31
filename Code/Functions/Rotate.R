# Rotate all points according to a predefined set of axes
rotateData <- function(df,axesdata){

  axesdata <- axesdata |> filter(Individual == df$Individual[1])
  
  df2 <- df |> 
    addAxesData(axesdata) |> 
    dorot(axesdata)

  cols2bind <- df2 |>
    ungroup() |>
    select(xr.cm,yr.cm,zr.cm)
  
  df |>
    cbind(cols2bind)
}

# Add the axes data to the main data frame
addAxesData <- function(df, axesdata){
  
  axesdatarot <- axesdata |> buildRotationMatrix()
  left_join(df, axesdatarot, by = c("Individual"))
}

# Build the matrix of the new axes
buildRotationMatrix <- function(axesdata){
  
  RotMatList <- list()
  
  for(i in 1:length(axesdata$Individual)){
    X0 <- axesdata[i,]$pt1_X
    X1 <- axesdata[i,]$pt2_X
    X2 <- axesdata[i,]$pt3_X
    X3 <- axesdata[i,]$pt4_X
    
    Y0 <- axesdata[i,]$pt1_Y
    Y1 <- axesdata[i,]$pt2_Y
    Y2 <- axesdata[i,]$pt3_Y
    Y3 <- axesdata[i,]$pt4_Y
    
    Z0 <- axesdata[i,]$pt1_Z
    Z1 <- axesdata[i,]$pt2_Z
    Z2 <- axesdata[i,]$pt3_Z
    Z3 <- axesdata[i,]$pt4_Z
    
    xVect <- c(X1-X0, Y1-Y0, Z1-Z0)
    yVect <- c(X2-X0, Y2-Y0, Z2-Z0)
    zVect <- c(X3-X0, Y3-Y0, Z3-Z0)
    
    xUnit <- xVect/VectLength(xVect)
    yVectProj <- yVect - (yVect %*% xUnit) * xUnit
    yUnit <- yVectProj/VectLength(yVectProj)
    zprimeVectProj <- zVect - (zVect %*% yUnit) * yUnit
    zVectProj <- zprimeVectProj - (zprimeVectProj %*% xUnit) * xUnit
    zUnit <- zVectProj/VectLength(zVectProj) 
    
    RotMat <- matrix(c(xUnit, yUnit, zUnit),
                     nrow = 3,
                     ncol = 3,
                     byrow = TRUE)
    
    RotMatList[i] <- list(RotMat)
  }
  axesdata$RotMat <- RotMatList
  axesdata
}

# Actually DO the rotation
dorot <- function(df,axesmat) {
  
  xyz <- df |>
    select(x.cm, y.cm, z.cm) 
  
  R <- df$RotMat[[1]]
  
  axesmat <- axesmat |> filter(Individual == first(df$Individual))
  
  xyz <- xyz |> mutate(x.cm = x.cm - axesmat$pt1_X,
           y.cm = y.cm - axesmat$pt1_Y,
           z.cm = z.cm - axesmat$pt1_Z) |>
  as.matrix()
  
  rot <- R %*% (t(xyz)) |>
    t() |>
    as.data.frame()
  colnames(rot) <- c("xr.cm", "yr.cm", "zr.cm")
  
  bind_cols(xyz, rot)
}