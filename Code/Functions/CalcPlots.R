bodyTracesPlot <- function(df, id, frms){
  df <- df |>
    filter(ID == id)
  
  df %>%
    ungroup() %>%
    filter(frame %% frms == 0) %>%
    filter(!str_ends(bodypart, 'Caudal')) %>%
    ggplot(aes(x = xs.cm, y = ys.cm, color = t.sec, group = frame)) +
    geom_path(size = .5) +
    scale_colour_gradient(low = "gray", high = "black") +
    theme_void() +
    geom_point(aes(x = xs.cm, y = ys.cm), size = 1)+
    geom_point(data = ~filter(.x, bodypart == 'Snout'),
               color = 'orange', size = 1) + 
    geom_point(data = ~filter(.x, bodypart == 'Peduncle'),
               color = 'purple', size = 1) +
    xlab('X position') + 
    ylab('Y position') +
    theme(axis.text=element_blank(),
          axis.title=element_blank(),
          legend.title=element_text(size=8),
          legend.text=element_text(size = 8),
          legend.key.size = unit(.25, "cm"),
          plot.margin=grid::unit(c(.05,.05,.05,.05), "cm"),
          axis.ticks.length = unit(0, "pt")) +
    coord_fixed() +
    annotation_scale(plot_unit = 'cm',
                     height = unit(0.025, "cm"),
                     bar_cols = c('black'),
                     text_cex = .6) +
    labs(color ='Time (s)')
}

SnoutVsTailAngularVelocityPlot <- function(df){
  df |> 
    ggplot(aes(x = t.sec, y = value, group = name, color = name)) +
    geom_hline(aes(yintercept = 0), size = .25) +
    geom_line(aes(linetype = name), size = .75) +
    ylab('Angular Velocity (deg/s)') +
    xlab('Time (s)') +
    theme_classic()  +
    scale_color_manual(values = c('orange',
                                  'purple'),
                       labels = c('Snout','Tail')) +
    scale_linetype_manual(values = c('solid','dashed'), labels = c('Snout','Tail')) +
    theme(axis.text=element_text(8),
          axis.title=element_text(size=8,face="bold"),
          legend.title=element_text(size=8),
          legend.text=element_text(size = 8),
          legend.key.size = unit(.75, "cm"),
          legend.position = 'right',
          plot.margin=grid::unit(c(.05,.05,.05,.05), "cm")) +
    labs(color = 'Bodypart', linetype = 'Bodypart') +
    geom_vline(xintercept = mean(df$tsnoutmax), color = "darkgray", size=.75) +
  geom_vline(xintercept = mean(df$ttailmax), color = "darkgray", size=.75)
}

SnoutAnglePlot <- function(df){
  df |> 
    ggplot(aes(x = t.sec, y = snoutangle.deg, group = ID, color = as.character(Speed.group))) +
    theme_bw() +
    ylab('Snout angle (deg)') +
    xlab('Time (s)') +
    labs(color = "Turn Speed") +
    scale_color_manual(labels=c('Fast', 'Slow'),
                       values = c('firebrick','deepskyblue')) +
    theme(axis.text=element_text(size=8),
          axis.title=element_text(size=8,face="bold"),
          legend.title=element_text(size=8),
          legend.text=element_text(size = 8),
          legend.key.size = unit(.5, "cm"),
          plot.margin=grid::unit(c(.05,.05,.05,.05), "cm")) +
    geom_path(size = .5)  
}

SlowSnoutVelocityPlot <- function(df,meansnoutvel){
  df |> 
    group_by(ID) |> 
    mutate(tmax = t.sec[which.max(snoutangle.degps)],
           newT = t.sec - tmax) |> 
    filter(Speed.group == 'Slow') |> 
    ggplot(aes(x = newT, y = snoutangle.degps, group = ID)) +
    theme_bw() +
    ylab('Snout angular vel. (deg/s)') +
    xlab('Time relative to maximum angular velocity (s)') +
    theme(axis.text=element_text(size=8),
          axis.title=element_text(size=8,face="bold"),
          legend.title=element_text(size=8),
          legend.text=element_text(size = 8),
          legend.key.size = unit(.5, "cm"),
          plot.margin=grid::unit(c(.05,.05,.05,.05), "cm")) +
    xlim(c(-1,1)) +
    ylim(c(-250,800)) +
    geom_path(color = 'darkslategray3', size = .25, alpha = .25) +
    geom_path(data = meansnoutvel, aes(x = newT, y = meansnoutvel.degps),
              inherit.aes = FALSE, size = .75, color = 'deepskyblue')
}
  
FastSnoutVelocityPlot <- function(df, meansnoutvel){
  df |> 
    group_by(ID) |> 
    mutate(tmax = t.sec[which.max(snoutangle.degps)],
           newT = t.sec - tmax) |>
    filter(Speed.group == 'Fast') |> 
    ggplot(aes(x = newT, y = snoutangle.degps, group = ID)) +
    theme_bw() +
    ylab('Snout angular vel. (deg/s)') +
    xlab('Time relative to maximum angular velocity (s)') +
    theme(axis.text=element_text(size = 8),
          axis.title=element_text(size=8,face="bold"),
          legend.title=element_text(size=8),
          legend.text=element_text(size = 8),
          legend.key.size = unit(.5, "cm"),
          plot.margin=grid::unit(c(.05,.05,.05,.05), "cm")) +
    xlim(c(-1,1)) +
    ylim(c(-250,800)) +
    geom_path(color = 'indianred1', size = .25, alpha = .25) +
    geom_path(data = meansnoutvel, aes(x = newT, y = meansnoutvel.degps),
              inherit.aes = FALSE, size = .75, color = 'firebrick')
}

SlowMoIPlot <- function(df, meanmoi){
  df |> 
    group_by(ID) |> 
    mutate(percMoI = MoI.gcm2/MaxMoI.gcm2,
           tmin = t.sec[which.max(snoutangle.degps)],
           newT = t.sec - tmin) |> 
    filter(Speed.group == 'Slow') |>
    group_by(ID) |> 
    ggplot(aes(x = newT, y = percMoI, group = ID)) +
    theme_bw() +
    ylab('Normalized \n moment of inertia') +
    xlab('Time relative to maximum \n angular velocity (s)') +
    theme(axis.text=element_text(size=8),
          axis.title=element_text(size=8,face="bold"),
          legend.title=element_text(size=8),
          legend.text=element_text(size = 8),
          legend.key.size = unit(.25, "cm")) +
    geom_path(color = 'darkslategray3', size = .25, alpha = .25) +
    geom_path(data = meanmoi, aes(x = newT, y = meanpercMoI),
              inherit.aes = FALSE, size = .75, color = 'deepskyblue') +
    xlim(c(-1,1)) +
    ylim(.75,1)
}

FastMoIPlot <- function(df, meanmoi){
  df |> 
    group_by(ID) |> 
    mutate(percMoI = MoI.gcm2/MaxMoI.gcm2,
           tmin = t.sec[which.max(snoutangle.degps)],
           newT = t.sec - tmin) |> 
    filter(Speed.group == 'Fast') |> 
    ggplot(aes(x = newT, y = percMoI, group = ID)) +
    theme_bw() +
    ylab('Normalized \n moment of inertia') +
    xlab('Time relative to maximum \n angular velocity (s)') +
    theme(axis.text=element_text(size=8),
          axis.title=element_text(size=8,face="bold"),
          legend.title=element_text(size=8),
          legend.text=element_text(size = 8),
          legend.key.size = unit(.25, "cm")) +
    geom_path(color = 'indianred1', size = .25, alpha = .25) +
    geom_path(data = meanmoi, aes(x = newT, y = meanpercMoI),
              inherit.aes = FALSE, size = .75, color = 'firebrick') +
    xlim(c(-1,1)) +
    ylim(.75,1)
}


SnoutTailComp <- function(df, max){
  SnoutTailDotplot |> 
    filter(Max == max) |> 
    ggplot(aes(y = AngularVelocity, x = Bodypart, color = Bodypart)) +
    geom_swarm(dotsize = .75, alpha = .25) +
    geom_hline(aes(yintercept = 0), size = .25) +
    theme_classic() +
    scale_color_manual(values = c('orange','purple')) +
    stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
                 width = 0.5, size = 1, linetype = "solid") +
    theme(legend.position="none",
          axis.text=element_text(size=8),
          axis.title=element_text(size=8,face="bold"),
          legend.text=element_text(size = 8),
          legend.key.size = unit(.25, "cm"),
          plot.margin=grid::unit(c(.05,.05,.05,.05), "cm")) +
    scale_x_discrete(labels=c('Snout','Tail')) +
    labs(color = 'Turn Speed') +
    ylab('Angular Velocity (deg/s)') +
    xlab('Bodypart') +
    ylim(c(-300, 900))
}

MeanMoiMeanSnoutAngVelPlot <- function(df, meanSnoutAng.lm, meanFastSnoutAng.lm, meanSlowSnoutAng.lm){
  df |> 
    group_by(ID) |> 
    mutate(Individual= substr(ID,1,4)) |> 
    AddBackSpeed(maindata) |> 
    ggplot(aes(y = meansnoutangle.degps, x = (meanpercMoI), color = as.character(Speed.group))) +
    geom_point(aes(shape = Speed.group), size = 2) +
    scale_shape_manual(values = c(2, 19)) +
    theme_classic() +
    xlim(c(.88,.99)) +
    ylim(c(20,400)) +
    geom_abline(slope = coef(meanFastSnoutAng.lm)[["meanpercMoI"]],
                intercept = coef(meanFastSnoutAng.lm)[["(Intercept)"]],
                size = 1,
                color = 'firebrick') +
    geom_abline(slope = coef(meanSlowSnoutAng.lm)[["meanpercMoI"]],
                intercept = coef(meanSlowSnoutAng.lm)[["(Intercept)"]],
                size = 1,
                color = 'deepskyblue') +
    ylab('Mean Snout Angular Velocity (deg/s)') +
    xlab('Mean Normalized MoI (%)') +
    scale_color_manual(labels=c('Fast', 'Slow'),
                       values = c('firebrick','deepskyblue')) +
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=10,face="bold"),
          plot.margin=grid::unit(c(.05,.05,.05,.05), "cm")) +
    labs(color = 'Turn Speed', shape = 'Turn Speed')
}

MoISnoutVelTimingPlot <- function(df){
  df |> 
    group_by(ID) |> 
    mutate(Individual= substr(ID,1,4)) |> 
    ggplot(aes(x = tminMoI, y = tmaxVel, color = as.character(Speed.group))) +
    annotate(geom = "polygon", x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "goldenrod", alpha = .2) +
    geom_point(aes(shape = Speed.group), size = 2) +
    scale_shape_manual(values = c(2, 19)) +
    theme_classic() +
    xlim(c(0,2.7)) +
    ylim(c(0,2.7)) +
    xlab('Time of minimum Moment of Inertia (s)') +
    ylab('Time of maximum Angular Velocity (s)') +
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=10,face="bold"),
          plot.margin=grid::unit(c(.05,.05,.05,.05), "cm")) +
    scale_color_manual(labels=c('Fast', 'Slow'),
                       values = c('firebrick',
                                  'deepskyblue')) +
    labs(color = 'Turn Speed', shape = 'Turn Speed')
}

TorqueMoITimingPlot <- function(df){
  df <- df |> 
    group_by(ID) |> 
    reframe(tmaxT = t.sec[which.max(torque.gcm2ps2)],
            tminMoI = t.sec[which.min(MoI.gcm2)],
            Speed.group = as.character(Speed.group))
  
  df |> 
    group_by(ID) |> 
    mutate(Individual= substr(ID,1,4)) |> 
    ggplot(aes(x = tminMoI, y = tmaxT, color = as.character(Speed.group))) +
    geom_point(aes(shape = Speed.group), size = 2) +
    scale_shape_manual(values = c(2, 19)) +
    theme_classic() +
    annotate(geom = "polygon", x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "goldenrod", alpha = .2) +
    xlab('Time of minimum Moment of Inertia (s)') +
    ylab('Time of maximum Torque (s)') +
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=10,face="bold"),
          plot.margin=grid::unit(c(.05,.05,.05,.05), "cm")) +
    scale_color_manual(labels=c('Fast', 'Slow'),
                       values = c('firebrick',
                                  'deepskyblue')) +
    labs(color = 'Turn Speed', shape = 'Turn Speed')
}

TorqueMoITimingPlot2 <- function(df){
  
  df |> 
    group_by(ID) |> 
    ggplot(aes(y = TimeDiff.sec, x = Speed.group, color = Speed.group)) +
    geom_swarm(aes(shape = Speed.group), dotsize = .75, alpha = .25) +
    scale_shape_manual(values = c(2, 19)) +
    geom_hline(aes(yintercept = 0), size = .5) +
    theme_classic() +
    scale_color_manual(values = c('firebrick','deepskyblue')) +
    stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
                 width = 0.5, size = 1, linetype = "solid") +
    theme(legend.position = 'none',
          axis.text=element_text(size=8),
          axis.title=element_text(size=8,face="bold"),
          legend.text=element_text(size = 8),
          legend.key.size = unit(.25, "cm"),
          plot.margin=grid::unit(c(.05,.05,.05,.05), "cm")) +
    scale_x_discrete(labels=c('Fast','Slow')) +
    labs(color = 'Turn Speed') +
    ylab('Time difference (s)') +
    xlab('Turn Speed') + 
    ylim(c(-1.5, 1.5))
  
}

TotalPecStrokesPlot <- function(df, totalpecstrokes.lm){
  df |> 
    ggplot(aes(x = totaltime, y = TotalPecStrokes, color = as.character(Speed.group))) +
    geom_point(aes(shape = Speed.group), size = .5) +
    scale_shape_manual(values = c(2, 19)) +
    theme_classic() +
    geom_abline(slope = coef(totalpecstrokes.lm)[["totaltime"]],
                intercept = coef(totalpecstrokes.lm)[["(Intercept)"]],
                size = .5,
                color = 'black') +
    xlab('Total time of turn (s)') +
    ylab('Number of pectoral fin strokes') +
    theme(axis.text=element_text(size=8),
          axis.title=element_text(size=8,face="bold"),
          legend.position = 'none',
          plot.margin=grid::unit(c(.05,.05,.05,.05), "cm")) +
    scale_color_manual(labels=c('Fast', 'Slow'),
                       values = c('firebrick',
                                  'deepskyblue')) +
    xlim(c(0,3.5)) +
    ylim(0, 14)
}

PecStrokesPerTimePlot <- function(df, pecstrokes){
  df |> 
    ggplot(aes(x = totaltime, y = StrokesPerTime.sps, color = as.character(Speed.group))) +
    geom_point(aes(shape = Speed.group), size = .5) +
    scale_shape_manual(values = c(2, 19)) +
    theme_classic() +
    geom_smooth(data = pecstrokes, aes(x = totaltime, y = StrokesPerTime.sps),
                method="lm",formula=  y ~ I(1/x), inherit.aes = FALSE, color = 'black', size = .5) +
    xlab('Total time of turn (s)') +
    ylab('Fin beat frequency (Hz)') +
    theme(axis.text=element_text(size=8),
          axis.title=element_text(size=8,face="bold"),
          legend.position = 'none',
          plot.margin=grid::unit(c(.05,.05,.05,.05), "cm")) +
    scale_color_manual(labels=c('Fast', 'Slow'),
                       values = c('firebrick',
                                  'deepskyblue')) +
    xlim(c(0,3.5)) +
    ylim(0, 12.5)
}

BackingDataPlot <- function(df, backingdata.lm){
  df |> 
    ggplot(aes(x = totaltime, y = numberofbackingstrokes, color = as.character(Speed.group))) +
    geom_point(aes(shape = Speed.group), size = .5) +
    scale_shape_manual(values = c(2, 19)) +
    theme_classic() +
    geom_abline(slope = coef(backingdata.lm)[["totaltime"]],
                intercept = coef(backingdata.lm)[["(Intercept)"]],
                size = .5,
                color = 'black') +
    xlab('Total time of turn (s)') +
    ylab('Number of backing strokes') +
    theme(axis.text=element_text(size=8),
          axis.title=element_text(size=8,face="bold"),
          legend.position = 'none',
          plot.margin=grid::unit(c(.05,.05,.05,.05), "cm")) +
    scale_color_manual(labels=c('Fast', 'Slow'),
                       values = c('firebrick',
                                  'deepskyblue')) +
    labs(color = 'Turn Speed', shape = 'Turn Speed') +
    xlim(c(0,3.5)) +
    ylim(0, 7)
}

PecFinAmplitudePlot <- function(df){
  df |> 
  ggplot(aes(y = pecangle.deg, x = side, color = Speed.group), color = Speed.group) +
    geom_swarm(dotsize = .5, alpha = .15) +
    theme_classic() +
    facet_wrap(~Speed.group) +
    scale_color_manual(values = c('firebrick','deepskyblue')) +
    stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
                 width = 0.5, size = 1, linetype = "solid") +
    theme(strip.background = element_blank(),
          strip.text.x = element_blank(),
          axis.text=element_text(size=8),
          axis.title=element_text(size=8,face="bold"),
          legend.position='none',
          plot.margin=grid::unit(c(.05,.05,.05,.05), "cm")) +
    labs(color = 'Turn Speed') +
    ylab('Pectoral Fin Angle (degrees)') +
    xlab('Fin side')
}

angularmomentumplot <- function(df, speedgroup, speedcolor, y1, y2){
  df |> 
    filter(Speed.group == speedgroup) |> 
    ggplot(aes(y = meanmomentum.gnormdegps, x = Time)) +
    geom_swarm(dotsize = .5, alpha = .15, color = speedcolor) +
    theme_classic() +
    stat_summary(fun.y = median, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
                 width = 0.5, size = 1, linetype = "solid", color = speedcolor) +
    geom_hline(yintercept = 0) +
    theme(strip.background = element_blank(),
          strip.text.x = element_blank(),
          axis.text=element_text(size=8),
          axis.title=element_text(size=8,face="bold"),
          legend.position = 'none',
          plot.margin=grid::unit(c(.05,.05,.05,.05), "cm")) +
    ylab('Normalized Angular Momentum') +
    xlab('Turn Portion') +
    ylim(c(y1,y2))
}

linearmomentumplot <- function(df, speedgroup, speedcolor, y1, y2){
  df |> 
    filter(Speed.group == speedgroup) |> 
    ggplot(aes(y = meanlinearmomentum.gnormcmps, x = Time), color = speedcolor) +
    geom_swarm(dotsize = .5, alpha = .15, color = speedcolor) +
    theme_classic() +
    stat_summary(fun.y = median, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
                 width = 0.5, size = 1, linetype = "solid", color = speedcolor) +
    geom_hline(yintercept = 0) +
    theme(strip.background = element_blank(),
          strip.text.x = element_blank(),
          axis.text=element_text(size=8),
          axis.title=element_text(size=8,face="bold"),
          legend.position = 'none',
          plot.margin=grid::unit(c(.05,.05,.05,.05), "cm")) +
    ylab('Normalized Linear Momentum') +
    xlab('Turn Portion') +
    ylim(c(y1,y2))
}

