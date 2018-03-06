# Summary:
# 1. view.single.trial
# 2. view.range.of.trials
# 3. view.summary.trials
# 4. view.single.trial.projections
# 5. view.range.of.trials.projections
# 6. view.single.trial.grasp3d
# 7. view.range.of.trials.grasp3d
# 8. view.single.trial.GA
# 9. view.range.of.trials.GA
# 10. view.summary.trials.GA
# 11. view.single.trial.grasp3d.anim
# 12. view.range.of.trials.grasp3d.anim

######################################################################################################## 1.
# view.single.trial <- function(tr, dataset, details=F)
# {
#   # debug lines
#   # dataset <- allData
#   # tr <- 12
#   
#   dataset$indexOccluded <- with(dataset, ifelse(is.na(indexXraw), 1, 0))
#   dataset$thumbOccluded <- with(dataset, ifelse(is.na(thumbXraw), 1, 0))
#   
#   if(details)
#   {
#     
#     #### first panel: overview of GA and its related kinematics
#     
#     # GA
#     graph.GA <- ggplot() + 
#       geom_point(data=subset(dataset, trialN==tr), aes(time, GA), size=4.5, na.rm=T) + 
#       geom_point(data=subset(dataset, trialN==tr & fingersOccluded==1), aes(time, GA), size=4.5, color='yellow3') + 
#       labs(title=sprintf("trial %d",tr)) + labs(x=NULL) +
#       theme(axis.text.x=element_blank(), 
#             axis.title.x=element_blank(),
#             axis.ticks.x=element_blank()
#       ) + theme_bw()
#     
#     # GAallKinRaw
#     graph.GAallKinRaw <- ggplot() + geom_line(data=subset(dataset, trialN==tr), aes(time, GAallKinRaw), color='brown', size=2, na.rm=T)  + 
#       labs(x=NULL) + theme(axis.text.x=element_blank(), 
#                            axis.title.x=element_blank(),
#                            plot.title=element_blank(),
#                            axis.ticks.x=element_blank()
#       ) + theme_bw()
#     
#     # fingersAllKinRaw
#     graph.fingersAllKinRaw <- ggplot() + geom_line(data=subset(dataset, trialN==tr), aes(time, fingersAllKinRaw), color='darkmagenta', size=2, na.rm=T) + theme_bw()
#     
#     # putting them together
#     GA <- ggplot_gtable(ggplot_build(graph.GA))
#     GAallKin <- ggplot_gtable(ggplot_build(graph.GAallKinRaw))
#     fingersAllKin <- ggplot_gtable(ggplot_build(graph.fingersAllKinRaw))
#     maxWidth = unit.pmax(GA$widths[2:3], GAallKin$widths[2:3], fingersAllKin$widths[2:3])
#     GA$widths[2:3] <- maxWidth
#     GAallKin$widths[2:3] <- maxWidth
#     fingersAllKin$widths[2:3] <- maxWidth
#     # grid.arrange(GA, GAallKin, fingersAllKin)
#     
#     #### second panel: fingers movement and related kinematics
#     
#     graph.fingers <- ggplot() + 
#       geom_point(data=subset(dataset, trialN==tr), aes(time, indexZ), size=4.5, color='red', na.rm=T) + 
#       geom_point(data=subset(dataset, trialN==tr & indexOccluded==1), aes(time, indexZ), size=4.5, color='palegoldenrod') + 
#       labs(title=sprintf("trial %d",tr)) + 
#       geom_point(data=subset(dataset, trialN==tr), aes(time, thumbZ), color='blue', size=3.5, na.rm=T) + 
#       geom_point(data=subset(dataset, trialN==tr & thumbOccluded==1), aes(time, thumbZ), color='paleturquoise', size=3.5) + 
#       geom_point(data=subset(dataset, trialN==tr), aes(time, GA), color='black', na.rm=T) + labs(x=NULL, y="fingers Z") +
#       geom_point(data=subset(dataset, trialN==tr & fingersOccluded==1), aes(time, GA), color='yellow3') + 
#       theme(axis.text.x=element_blank(), 
#             axis.title.x=element_blank(),
#             axis.ticks.x=element_blank()
#       ) + theme_bw() 
#     
#     graph.fingMotion <- ggplot() + geom_line(data=subset(dataset, trialN==tr), aes(time, indexAllKinRaw), color='red', size=1.5) +
#       geom_line(data=subset(dataset, trialN==tr), aes(time, thumbAllKinRaw), color='blue', size=1.5) + ylab("Fingers allKinRaw") +
#       labs(x=NULL) + theme(axis.text.x=element_blank(), 
#                            axis.title.x=element_blank(),
#                            plot.title=element_blank(),
#                            axis.ticks.x=element_blank()
#       ) + theme_bw()
#     
#     graph.graspMotion <- ggplot() + geom_line(data=subset(dataset, trialN==tr), aes(time, GPallKinRaw), color='green4', size=2) + ylab("GP allKinRaw") + theme_bw()
#     
#     # putting them together
#     fingers <- ggplot_gtable(ggplot_build(graph.fingers))
#     fingMot <- ggplot_gtable(ggplot_build(graph.fingMotion))
#     graspMot <- ggplot_gtable(ggplot_build(graph.graspMotion))
#     maxWidth = unit.pmax(fingers$widths[2:3], fingMot$widths[2:3], graspMot$widths[2:3])
#     fingers$widths[2:3] <- maxWidth
#     fingMot$widths[2:3] <- maxWidth
#     graspMot$widths[2:3] <- maxWidth
#     # grid.arrange(fingers, Xfing, Yfing, Zfing, nrow=4)
#     
#     return(grid.arrange(fingers, GA, fingMot, GAallKin, graspMot, fingersAllKin, ncol=2, nrow=3))
#     
#   }else
#   {
#     graph.fingers <- ggplot() + geom_point(data=subset(dataset, trialN==tr), aes(time, indexZ), size=4.5, color='red', na.rm=T) + 
#       geom_point(data=subset(dataset, trialN==tr & indexOccluded==1), aes(time, indexZ), size=4.5, color='palegoldenrod', na.rm=T) + 
#       labs(title=sprintf("trial %d",tr)) + 
#       geom_point(data=subset(dataset, trialN==tr), aes(time, thumbZ), color='blue', size=3.5, na.rm=T) + 
#       geom_point(data=subset(dataset, trialN==tr & thumbOccluded==1), aes(time, thumbZ), color='paleturquoise', size=3.5, na.rm=T) + 
#       geom_point(data=subset(dataset, trialN==tr), aes(time, GA), color='black', na.rm=T) + 
#       geom_point(data=subset(dataset, trialN==tr & fingersOccluded==1), aes(time, GA), color='yellow3') + labs(x="time", y="fingers Z") + theme_bw()
#     
#     return(graph.fingers)
#   }
#   ## TODO: 
#   ## legends
#   ## return multiple graph
#   
# }

######################################################################################################## 2.
#' @export
view.range.of.trials <- function(trials, dataset, details=F){
  tr <- 1
  while(tr <= length(trials))
  {
    print(view.single.trial(trials[tr], dataset, details))
    print("Type 6 and hit Return to go to the next trial. Type 4 and hit Return to go to the previous trial. Type q and hit Return to quit.")
    line <- readline()
    if(line=="4" & tr > 1)
    {
      tr <- tr-1
    }
    if(line=="6")
    {
      tr <- tr+1
    }
    if((line %in% c("4","6"))==F)
    {
      stop("Exit after user keypress.")
    }
  }
}

######################################################################################################## 3.
#' view.summary.trials
#' @export
view.summary.trials <- function(trials=NULL, dataset, grid.size=20, frame=T, print=T, res=28)
{
  dataset$indexOccluded <- with(dataset, ifelse(is.na(indexXraw), 1, 0))
  dataset$thumbOccluded <- with(dataset, ifelse(is.na(thumbXraw), 1, 0))

  if(is.null(trials))
  {
    trials <- unique(dataset$trialN)
    num.trials <- length(trials)
  } else
  {
    num.trials <- length(trials)
  }
  
  if(print)
  {
    pdf(file=sprintf("Trials_summary.pdf"), width=res, height=res)
  }
  
  for(i in 1:ceiling(num.trials/grid.size))
  {
    trials.temp <- trials[(1:grid.size)+(grid.size*(i-1))]
    dataset.graph <- subset(dataset, trialN %in% trials.temp[!is.na(trials.temp)])
  
    if(!frame)
    {
      graph.fingers <- ggplot() + 
        geom_point(data=dataset.graph, aes(time, indexZ), size=4.5, color='red', na.rm=T) + 
        geom_point(data=subset(dataset.graph, indexOccluded==1), aes(time, indexZ), size=4.5, color='palegoldenrod', na.rm=T) + 
        labs(title=sprintf("Trials summary %d/%d",i,ceiling(num.trials/grid.size))) + 
        geom_point(data=dataset.graph, aes(time, thumbZ), color='blue', size=3.5, na.rm=T) + 
        geom_point(data=subset(dataset.graph, thumbOccluded==1), aes(time, thumbZ), color='paleturquoise', size=3.5, na.rm=T) + 
        geom_point(data=dataset.graph, aes(time, GA), color='black', na.rm=T) + 
        geom_point(data=subset(dataset.graph, fingersOccluded==1), aes(time, GA), color='yellow3') + labs(x="time", y="fingers Z") + 
        facet_wrap(~trialN) + theme_bw()
    } else
    {
      graph.fingers <- ggplot() + 
        geom_point(data=dataset.graph, aes(frameN, indexZ), size=4.5, color='red', na.rm=T) + 
        geom_point(data=subset(dataset.graph, indexOccluded==1), aes(frameN, indexZ), size=4.5, color='palegoldenrod', na.rm=T) + 
        labs(title=sprintf("Trials summary %d/%d",i,ceiling(num.trials/grid.size))) + 
        geom_point(data=dataset.graph, aes(frameN, thumbZ), color='blue', size=3.5, na.rm=T) + 
        geom_point(data=subset(dataset.graph, thumbOccluded==1), aes(frameN, thumbZ), color='paleturquoise', size=3.5, na.rm=T) + 
        geom_point(data=dataset.graph, aes(frameN, GA), color='black', na.rm=T) + 
        geom_point(data=subset(dataset.graph, fingersOccluded==1), aes(frameN, GA), color='yellow3') + labs(x="time", y="fingers Z") + 
        facet_wrap(~trialN) + theme_bw()
    }

    return(graph.fingers)
  }
  
  if(print)
  {
    cat('Graphs saved in ', getwd(), '/Trials_summary.pdf\n', sep='')
    dev.off()
  }
}

######################################################################################################## 4.

#' display the trajectory of the reaching movement on the three major projection planes
#' @export
view.single.trial.projections <- function(tr, datatraj, datakin)
{
  datatraj$indexOccluded <- with(datatraj, ifelse(is.na(indexXraw), 1, 0))
  datatraj$thumbOccluded <- with(datatraj, ifelse(is.na(thumbXraw), 1, 0))
  
  graph1 <- ggplot(subset(datatraj, trialN==tr), aes(indexX, indexY, size=time)) + geom_point(colour="red") + labs(x="X", y="Y", title="Frontal projection") +
    geom_point(data=subset(datatraj, trialN==tr & indexOccluded==1), aes(indexX, indexY, size=time), color="palegoldenrod") + 
    geom_point(data=subset(datatraj, trialN==tr), aes(thumbX, thumbY, size=time), colour="blue") +
    geom_point(data=subset(datatraj, trialN==tr & thumbOccluded==1), aes(thumbX, thumbY, size=time), color="paleturquoise") + 
    geom_point(data=subset(datakin, trialN==tr), aes(IPx.MGA, IPy.MGA), color="black", size=6) +
    geom_point(data=subset(datakin, trialN==tr), aes(TPx.MGA, TPy.MGA), color="black", size=6) +
    geom_point(data=subset(datakin, trialN==tr), aes(x=FIPx, y=FIPy), color='green4', size=6) +
    geom_point(data=subset(datakin, trialN==tr), aes(FTPx, FTPy), color='yellow3', size=6) + theme_bw()
  
  graph2 <- ggplot(subset(datatraj, trialN==tr), aes(indexX, indexZ, size=time)) + geom_point(colour="red") + labs(x="X", y="Z", title="Transversal projection") +
    geom_point(data=subset(datatraj, trialN==tr & indexOccluded==1), aes(indexX, indexZ, size=time), color="palegoldenrod") + 
    geom_point(data=subset(datatraj, trialN==tr), aes(thumbX, thumbZ, size=time), colour="blue") +
    geom_point(data=subset(datatraj, trialN==tr & thumbOccluded==1), aes(thumbX, thumbZ, size=time), color="paleturquoise") + 
    geom_point(data=subset(datakin, trialN==tr), aes(IPx.MGA, IPz.MGA), color="black", size=6) +
    geom_point(data=subset(datakin, trialN==tr), aes(TPx.MGA, TPz.MGA), color="black", size=6) +
    geom_point(data=subset(datakin, trialN==tr), aes(x=FIPx, y=FIPz), color='green4', size=6) +
    geom_point(data=subset(datakin, trialN==tr), aes(FTPx, FTPz), color='yellow3', size=6) + theme_bw()
  
  graph3 <- ggplot(subset(datatraj, trialN==tr), aes(indexZ, indexY, size=time)) + geom_point(colour="red") + labs(x="Z", y="Y", title="Sagittal projection") +
    geom_point(data=subset(datatraj, trialN==tr & indexOccluded==1), aes(indexZ, indexY, size=time), color="palegoldenrod") + 
    geom_point(data=subset(datatraj, trialN==tr), aes(thumbZ, thumbY, size=time), colour="blue") + 
    geom_point(data=subset(datatraj, trialN==tr & thumbOccluded==1), aes(thumbZ, thumbY, size=time), color="paleturquoise") + 
    geom_point(data=subset(datakin, trialN==tr), aes(IPz.MGA, IPy.MGA), color="black", size=6) +
    geom_point(data=subset(datakin, trialN==tr), aes(TPz.MGA, TPy.MGA), color="black", size=6) +
    geom_point(data=subset(datakin, trialN==tr), aes(FIPz, FIPy), color='green4', size=6) +
    geom_point(data=subset(datakin, trialN==tr), aes(FTPz, FTPy), color='yellow3', size=6) + theme_bw()
  
  print(grid.arrange(graph1,graph2,graph3, ncol=3, main=sprintf("trial %d",tr)))
}

######################################################################################################## 5.
#' @export
view.range.of.trials.projections <- function(trials, datatraj, datakin){
  tr <- 1
  while(tr <= length(trials))
  {
    view.single.trial.projections(trials[tr], datatraj, datakin)
    print("Type 6 and hit Return to go to the next trial. Type 4 and hit Return to go to the previous trial. Type q and hit Return to quit.")
    line <- readline()
    if(line=="4" & tr > 1)
    {
      tr <- tr-1
    }
    if(line=="6")
    {
      tr <- tr+1
    }
    if((line %in% c("4","6"))==F)
    {
      stop("Exit after user keypress.")
    }
  }
}

######################################################################################################## 6.

#' interactive 3d plot
#' @export
view.single.trial.grasp3d <- function(tr, datatraj, datakin, entireMov=T)
{
  datatraj$indexOccluded <- with(datatraj, ifelse(is.na(indexXraw), 1, 0))
  datatraj$thumbOccluded <- with(datatraj, ifelse(is.na(thumbXraw), 1, 0))
  
  plot3d(subset(datatraj, trialN==tr & frameN<=subset(datakin, trialN==tr)$frameToFGA)[c("indexX","indexZ","indexY")], col="red", radius=5, main=sprintf("trial %d",tr), xlab="X", ylab="Z", zlab="Y", type='s')
  spheres3d(subset(datatraj, trialN==tr & frameN<=subset(datakin, trialN==tr)$frameToFGA)[c("thumbX","thumbZ","thumbY")], col="blue", radius=5)
  spheres3d(subset(datatraj, trialN==tr & indexOccluded==1 & frameN<=subset(datakin, trialN==tr)$frameToFGA)[c("indexX","indexZ","indexY")], col="palegoldenrod", radius=5)
  spheres3d(subset(datatraj, trialN==tr & thumbOccluded==1 & frameN<=subset(datakin, trialN==tr)$frameToFGA)[c("thumbX","thumbZ","thumbY")], col="paleturquoise", radius=5)
  if(entireMov)
  {
    spheres3d(subset(datatraj, trialN==tr & frameN>subset(datakin, trialN==tr)$frameToFGA)[c("indexX","indexZ","indexY")], col="grey60", radius=4)
    spheres3d(subset(datatraj, trialN==tr & frameN>subset(datakin, trialN==tr)$frameToFGA)[c("thumbX","thumbZ","thumbY")], col="grey70", radius=4)
  }
  spheres3d(subset(datakin, trialN==tr)[c("IPx.MGA","IPz.MGA","IPy.MGA")], color="black", radius=10)
  spheres3d(subset(datakin, trialN==tr)[c("TPx.MGA","TPz.MGA","TPy.MGA")], color="black", radius=10)
  spheres3d(subset(datakin, trialN==tr)[c("FIPx","FIPz","FIPy")], col="green4", radius=10)
  spheres3d(subset(datakin, trialN==tr)[c("FTPx","FTPz","FTPy")], col="yellow3", radius=10)
}

######################################################################################################## 7.
#' @export
view.range.of.trials.grasp3d <- function(trials, datatraj, datakin, entireMov=F){
  tr <- 1
  while(tr <= length(trials))
  {
    view.single.trial.grasp3d(trials[tr], datatraj, datakin, entireMov)
    print("Type 6 and hit Return to go to the next trial. Type 4 and hit Return to go to the previous trial. Type q and hit Return to quit.")
    line <- readline()
    if(line=="4" & tr > 1)
    {
      tr <- tr-1
    }
    if(line=="6")
    {
      tr <- tr+1
    }
    if((line %in% c("4","6"))==F)
    {
      stop("Exit after user keypress.")
    }
  }
}

######################################################################################################## 8.
# view.single.trial.GA <- function(tr, datatraj, datakin, details=F, frame=T)
# {
#   # debug lines
#   # datatraj <- allData
#   # datakin <- kinData
#   # tr <- 1
#   
#   datatraj$indexOccluded <- with(datatraj, ifelse(is.na(indexXraw), 1, 0))
#   datatraj$thumbOccluded <- with(datatraj, ifelse(is.na(thumbXraw), 1, 0))
#   
#   if(frame==F)
#   {
#     if(details)
#     {
#       
#       #### first panel: overview of GA and its related kinematics
#       
#       # GA
#       graph.GA <- ggplot() + geom_point(data=subset(datatraj, trialN==tr), aes(time, GA), size=4.5, na.rm=T) + 
#         labs(title=sprintf("trial %d",tr)) + labs(x=NULL) + 
#         geom_point(data=subset(datatraj, trialN==tr & fingersOccluded==1), aes(time, GA), size=4.5, color='yellow3') + 
#         geom_point(data=subset(datakin, trialN==tr), aes(timeToMGA, MGA), color="red", size=9) +
#         geom_point(data=subset(datakin, trialN==tr), aes(timeToFGA, FGA), color="green", size=6) +
#         theme(axis.text.x=element_blank(), 
#               axis.title.x=element_blank(),
#               axis.ticks.x=element_blank()
#         ) + theme_bw()
#       
#       # GAallKinRaw
#       graph.GAallKinRaw <- ggplot() + geom_line(data=subset(datatraj, trialN==tr), aes(time, GAallKinRaw), color='brown', size=2, na.rm=T)  + 
#         labs(x=NULL) + theme(axis.text.x=element_blank(), 
#                              axis.title.x=element_blank(),
#                              plot.title=element_blank(),
#                              axis.ticks.x=element_blank()
#         ) + theme_bw()
#       
#       # fingersAllKinRaw
#       graph.fingersAllKinRaw <- ggplot() + geom_line(data=subset(datatraj, trialN==tr), aes(time, fingersAllKinRaw), color='darkmagenta', size=2, na.rm=T) + theme_bw()
#       
#       # putting them together
#       GA <- ggplot_gtable(ggplot_build(graph.GA))
#       GAallKin <- ggplot_gtable(ggplot_build(graph.GAallKinRaw))
#       fingersAllKin <- ggplot_gtable(ggplot_build(graph.fingersAllKinRaw))
#       maxWidth = unit.pmax(GA$widths[2:3], GAallKin$widths[2:3], fingersAllKin$widths[2:3])
#       GA$widths[2:3] <- maxWidth
#       GAallKin$widths[2:3] <- maxWidth
#       fingersAllKin$widths[2:3] <- maxWidth
#       # grid.arrange(GA, GAallKin, fingersAllKin)
#       
#       #### second panel: fingers movement and related kinematics
#       
#       graph.fingers <- ggplot() + geom_point(data=subset(datatraj, trialN==tr), aes(time, indexZ), size=4.5, color='red', na.rm=T) + 
#         labs(title=sprintf("trial %d",tr)) + geom_point(data=subset(datatraj, trialN==tr), aes(time, thumbZ), color='blue', size=3.5, na.rm=T) + 
#         geom_point(data=subset(datatraj, trialN==tr), aes(time, GA), color='black', na.rm=T) + labs(x=NULL, y="fingers Z") +
#         geom_point(data=subset(datatraj, trialN==tr & indexOccluded==1), aes(time, indexZ), size=4.5, color='palegoldenrod') + 
#         geom_point(data=subset(datatraj, trialN==tr & thumbOccluded==1), aes(time, thumbZ), color='paleturquoise', size=3.5) + 
#         geom_point(data=subset(datatraj, trialN==tr & fingersOccluded==1), aes(time, GA), color='yellow3') + 
#         geom_point(data=subset(datakin, trialN==tr), aes(timeToMGA, MGA), color="red", size=9) +
#         geom_point(data=subset(datakin, trialN==tr), aes(timeToFGA, FGA), color="green", size=6) +
#         geom_point(data=subset(datakin, trialN==tr), aes(timeToMGA, IPz.MGA), color="red", size=10) +
#         geom_point(data=subset(datakin, trialN==tr), aes(timeToMGA, TPz.MGA), color="blue", size=10) +
#         geom_point(data=subset(datakin, trialN==tr), aes(timeToFGA, FIPz), color="green4", size=9) +
#         geom_point(data=subset(datakin, trialN==tr), aes(timeToFGA, FTPz), color="yellow3", size=9) +
#         theme(axis.text.x=element_blank(), 
#               axis.title.x=element_blank(),
#               axis.ticks.x=element_blank()
#         ) + theme_bw()
#       
#       graph.fingMotion <- ggplot() + geom_line(data=subset(datatraj, trialN==tr), aes(time, indexAllKinRaw), color='red', size=1.5) +
#         geom_line(data=subset(datatraj, trialN==tr), aes(time, thumbAllKinRaw), color='blue', size=1.5) + ylab("Fingers allKinRaw") +
#         labs(x=NULL) + theme(axis.text.x=element_blank(), 
#                              axis.title.x=element_blank(),
#                              plot.title=element_blank(),
#                              axis.ticks.x=element_blank()
#         ) + theme_bw()
#       
#       graph.graspMotion <- ggplot() + geom_line(data=subset(datatraj, trialN==tr), aes(time, GPallKinRaw), color='green4', size=2) + ylab("GP allKinRaw") + theme_bw()
#       
#       # putting them together
#       fingers <- ggplot_gtable(ggplot_build(graph.fingers))
#       fingMot <- ggplot_gtable(ggplot_build(graph.fingMotion))
#       graspMot <- ggplot_gtable(ggplot_build(graph.graspMotion))
#       maxWidth = unit.pmax(fingers$widths[2:3], fingMot$widths[2:3], graspMot$widths[2:3])
#       fingers$widths[2:3] <- maxWidth
#       fingMot$widths[2:3] <- maxWidth
#       graspMot$widths[2:3] <- maxWidth
#       # grid.arrange(fingers, Xfing, Yfing, Zfing, nrow=4)
#       
#       print(grid.arrange(fingers, GA, fingMot, GAallKin, graspMot, fingersAllKin, ncol=2, nrow=3))
#       
#     }else
#     {
#       graph.fingers <- ggplot() + 
#         geom_point(data=subset(datatraj, trialN==tr), aes(time, indexZ), size=4.5, color='red', na.rm=T) + 
#         labs(title=sprintf("trial %d",tr)) + geom_point(data=subset(datatraj, trialN==tr), aes(time, thumbZ), color='blue', size=3.5, na.rm=T) + 
#         geom_point(data=subset(datatraj, trialN==tr), aes(time, GA), color='black', na.rm=T) + labs(x="time", y="fingers Z") +
#         geom_point(data=subset(datatraj, trialN==tr & indexOccluded==1), aes(time, indexZ), size=4.5, color='palegoldenrod') + 
#         geom_point(data=subset(datatraj, trialN==tr & thumbOccluded==1), aes(time, thumbZ), color='paleturquoise', size=3.5) + 
#         geom_point(data=subset(datatraj, trialN==tr & fingersOccluded==1), aes(time, GA), color='yellow3') + 
#         geom_point(data=subset(datakin, trialN==tr), aes(timeToMGA, MGA), color="red", size=9) +
#         geom_point(data=subset(datakin, trialN==tr), aes(timeToFGA, FGA), color="green", size=6) +
#         geom_point(data=subset(datakin, trialN==tr), aes(timeToMGA, IPz.MGA), color="red", size=10) +
#         geom_point(data=subset(datakin, trialN==tr), aes(timeToMGA, TPz.MGA), color="blue", size=10) +
#         geom_point(data=subset(datakin, trialN==tr), aes(timeToFGA, FIPz), color="green4", size=9) +
#         geom_point(data=subset(datakin, trialN==tr), aes(timeToFGA, FTPz), color="yellow3", size=9) + theme_bw()
#       
#       return(graph.fingers)
#     }
#   }else
#   {
#     # debug lines
#     # datatraj <- allData
#     # datakin <- kinData
#     # tr <- 1
#     if(details)
#     {
#       
#       #### first panel: overview of GA and its related kinematics
#       
#       # GA
#       graph.GA <- ggplot() + geom_point(data=subset(datatraj, trialN==tr), aes(frameN, GA), size=4.5, na.rm=T) + 
#         geom_point(data=subset(datatraj, trialN==tr & fingersOccluded==1), aes(frameN, GA), size=4.5, color='yellow3') + 
#         labs(title=sprintf("trial %d",tr)) + labs(x=NULL) + geom_point(data=subset(datakin, trialN==tr), aes(frameToMGA, MGA), color="red", size=9) +
#         geom_point(data=subset(datakin, trialN==tr), aes(frameToFGA, FGA), color="green", size=6) +
#         theme(axis.text.x=element_blank(), 
#               axis.title.x=element_blank(),
#               axis.ticks.x=element_blank()
#         ) + theme_bw()
#       
#       # GAallKinRaw
#       graph.GAallKinRaw <- ggplot() + geom_line(data=subset(datatraj, trialN==tr), aes(frameN, GAallKinRaw), color='brown', size=2, na.rm=T)  + 
#         labs(x=NULL) + theme(axis.text.x=element_blank(), 
#                              axis.title.x=element_blank(),
#                              plot.title=element_blank(),
#                              axis.ticks.x=element_blank()
#         ) + theme_bw()
#       
#       # fingersAllKinRaw
#       graph.fingersAllKinRaw <- ggplot() + geom_line(data=subset(datatraj, trialN==tr), aes(frameN, fingersAllKinRaw), color='darkmagenta', size=2, na.rm=T) + theme_bw()
#       
#       # putting them together
#       GA <- ggplot_gtable(ggplot_build(graph.GA))
#       GAallKin <- ggplot_gtable(ggplot_build(graph.GAallKinRaw))
#       fingersAllKin <- ggplot_gtable(ggplot_build(graph.fingersAllKinRaw))
#       maxWidth = unit.pmax(GA$widths[2:3], GAallKin$widths[2:3], fingersAllKin$widths[2:3])
#       GA$widths[2:3] <- maxWidth
#       GAallKin$widths[2:3] <- maxWidth
#       fingersAllKin$widths[2:3] <- maxWidth
#       # grid.arrange(GA, GAallKin, fingersAllKin)
#       
#       #### second panel: fingers movement and related kinematics
#       
#       graph.fingers <- ggplot() + geom_point(data=subset(datatraj, trialN==tr), aes(frameN, indexZ), size=4.5, color='red', na.rm=T) + 
#         labs(title=sprintf("trial %d",tr)) + geom_point(data=subset(datatraj, trialN==tr), aes(frameN, thumbZ), color='blue', size=3.5, na.rm=T) + 
#         geom_point(data=subset(datatraj, trialN==tr), aes(frameN, GA), color='black', na.rm=T) + labs(x=NULL, y="fingers Z") +
#         geom_point(data=subset(datatraj, trialN==tr & indexOccluded==1), aes(frameN, indexZ), size=4.5, color='palegoldenrod') + 
#         geom_point(data=subset(datatraj, trialN==tr & thumbOccluded==1), aes(frameN, thumbZ), color='paleturquoise', size=3.5) + 
#         geom_point(data=subset(datatraj, trialN==tr & fingersOccluded==1), aes(frameN, GA), color='yellow3') + 
#         geom_point(data=subset(datakin, trialN==tr), aes(frameToMGA, MGA), color="red", size=9) +
#         geom_point(data=subset(datakin, trialN==tr), aes(frameToFGA, FGA), color="green", size=6) +
#         geom_point(data=subset(datakin, trialN==tr), aes(frameToMGA, IPz.MGA), color="red", size=10) +
#         geom_point(data=subset(datakin, trialN==tr), aes(frameToMGA, TPz.MGA), color="blue", size=10) +
#         geom_point(data=subset(datakin, trialN==tr), aes(frameToFGA, FIPz), color="green4", size=9) +
#         geom_point(data=subset(datakin, trialN==tr), aes(frameToFGA, FTPz), color="yellow3", size=9) +
#         theme(axis.text.x=element_blank(), 
#               axis.title.x=element_blank(),
#               axis.ticks.x=element_blank()
#         ) + theme_bw() 
#       
#       graph.fingMotion <- ggplot() + geom_line(data=subset(datatraj, trialN==tr), aes(frameN, indexAllKinRaw), color='red', size=1.5) +
#         geom_line(data=subset(datatraj, trialN==tr), aes(frameN, thumbAllKinRaw), color='blue', size=1.5) + ylab("Fingers allKinRaw") +
#         labs(x=NULL) + theme(axis.text.x=element_blank(), 
#                              axis.title.x=element_blank(),
#                              plot.title=element_blank(),
#                              axis.ticks.x=element_blank()
#         ) + theme_bw()
#       
#       graph.graspMotion <- ggplot() + geom_line(data=subset(datatraj, trialN==tr), aes(frameN, GPallKinRaw), color='green4', size=2) + ylab("GP allKinRaw") + theme_bw()
#       
#       # putting them together
#       fingers <- ggplot_gtable(ggplot_build(graph.fingers))
#       fingMot <- ggplot_gtable(ggplot_build(graph.fingMotion))
#       graspMot <- ggplot_gtable(ggplot_build(graph.graspMotion))
#       maxWidth = unit.pmax(fingers$widths[2:3], fingMot$widths[2:3], graspMot$widths[2:3])
#       fingers$widths[2:3] <- maxWidth
#       fingMot$widths[2:3] <- maxWidth
#       graspMot$widths[2:3] <- maxWidth
#       # grid.arrange(fingers, Xfing, Yfing, Zfing, nrow=4)
#       
#       print(grid.arrange(fingers, GA, fingMot, GAallKin, graspMot, fingersAllKin, ncol=2, nrow=3))
#       
#     }else
#     {
#       graph.fingers <- ggplot() + geom_point(data=subset(datatraj, trialN==tr), aes(frameN, indexZ), size=4.5, color='red', na.rm=T) + 
#         labs(title=sprintf("trial %d",tr)) + geom_point(data=subset(datatraj, trialN==tr), aes(frameN, thumbZ), color='blue', size=3.5, na.rm=T) + 
#         geom_point(data=subset(datatraj, trialN==tr), aes(frameN, GA), color='black', na.rm=T) + labs(x="frame", y="fingers Z") +
#         geom_point(data=subset(datatraj, trialN==tr & indexOccluded==1), aes(frameN, indexZ), size=4.5, color='palegoldenrod') + 
#         geom_point(data=subset(datatraj, trialN==tr & thumbOccluded==1), aes(frameN, thumbZ), color='paleturquoise', size=3.5) + 
#         geom_point(data=subset(datatraj, trialN==tr & fingersOccluded==1), aes(frameN, GA), color='yellow3') + 
#         geom_point(data=subset(datakin, trialN==tr), aes(frameToMGA, MGA), color="red", size=9) +
#         geom_point(data=subset(datakin, trialN==tr), aes(frameToFGA, FGA), color="green", size=6) +
#         geom_point(data=subset(datakin, trialN==tr), aes(frameToMGA, IPz.MGA), color="red", size=10) +
#         geom_point(data=subset(datakin, trialN==tr), aes(frameToMGA, TPz.MGA), color="blue", size=10) +
#         geom_point(data=subset(datakin, trialN==tr), aes(frameToFGA, FIPz), color="green4", size=9) +
#         geom_point(data=subset(datakin, trialN==tr), aes(frameToFGA, FTPz), color="yellow3", size=9) + theme_bw()
#       
#       return(graph.fingers)
#     }
#   }
#   ## TODO: 
#   ## legends
#   ## return multiple graph
#   
# }

######################################################################################################## 9.
#' @export
view.range.of.trials.GA <- function(trials, datatraj, datakin, details=F, frame=T){
  tr <- 1
  while(tr <= length(trials))
  {
    print(view.single.trial.GA(trials[tr], datatraj, datakin, details, frame))
    print("Type 6 and hit Return to go to the next trial. Type 4 and hit Return to go to the previous trial. Type q and hit Return to quit.")
    line <- readline()
    if(line=="4" & tr > 1)
    {
      tr <- tr-1
    }
    if(line=="6")
    {
      tr <- tr+1
    }
    if((line %in% c("4","6"))==F)
    {
      stop("Exit after user keypress.")
    }
  }
}

######################################################################################################## 10.
# view.summary.trials.GA <- function(trials=NULL, datatraj, datakin, grid.size=20, frame=T, print=T, res=28)
# {
#   datatraj$indexOccluded <- with(datatraj, ifelse(is.na(indexXraw), 1, 0))
#   datatraj$thumbOccluded <- with(datatraj, ifelse(is.na(thumbXraw), 1, 0))
#   
#   if(is.null(trials))
#   {
#     trials <- unique(datatraj$trialN)
#     num.trials <- length(trials)
#   } else
#   {
#     num.trials <- length(trials)
#   }
#   
#   if(print)
#   {
#     pdf(file=sprintf("Trials_summary_GA.pdf"), width=res, height=res)
#   }
#   
#   for(i in 1:ceiling(num.trials/grid.size))
#   {
#     trials.temp <- trials[(1:grid.size)+(grid.size*(i-1))]
#     datatraj.graph <- subset(datatraj, trialN %in% trials.temp[!is.na(trials.temp)])
#     datakin.graph <- subset(datakin, trialN %in% trials.temp[!is.na(trials.temp)])
#     
#     if(!frame)
#     {
#       graph.fingers <- ggplot() + 
#       geom_point(data=subset(datatraj.graph), aes(time, indexZ), size=4.5, color='red', na.rm=T) + 
#       labs(title=sprintf("Trial summary %d/%d",i,ceiling(num.trials/grid.size))) + 
#       geom_point(data=subset(datatraj.graph), aes(time, thumbZ), color='blue', size=3.5, na.rm=T) + 
#       geom_point(data=subset(datatraj.graph), aes(time, GA), color='black', na.rm=T) + labs(x="time", y="fingers Z") +
#       geom_point(data=subset(datatraj.graph, indexOccluded==1), aes(time, indexZ), size=4.5, color='palegoldenrod') + 
#       geom_point(data=subset(datatraj.graph, thumbOccluded==1), aes(time, thumbZ), color='paleturquoise', size=3.5) + 
#       geom_point(data=subset(datatraj.graph, fingersOccluded==1), aes(time, GA), color='yellow3') + 
#       geom_point(data=subset(datakin.graph), aes(timeToMGA, MGA), color="red", size=9) +
#       geom_point(data=subset(datakin.graph), aes(timeToFGA, FGA), color="green", size=6) +
#       geom_point(data=subset(datakin.graph), aes(timeToTGA, TGA), color="cyan", size=6) +
#       geom_point(data=subset(datakin.graph), aes(timeToMGA, IPz.MGA), color="red", size=10) +
#       geom_point(data=subset(datakin.graph), aes(timeToMGA, TPz.MGA), color="blue", size=10) +
#       geom_point(data=subset(datakin.graph), aes(timeToFGA, FIPz), color="green4", size=9) +
#       geom_point(data=subset(datakin.graph), aes(timeToFGA, FTPz), color="yellow3", size=9) + 
#       geom_point(data=subset(datakin.graph), aes(timeToTGA, IPz.TGA), color="cyan", size=9) +
#       geom_point(data=subset(datakin.graph), aes(timeToTGA, TPz.TGA), color="cyan", size=9) + 
#       facet_wrap(~trialN, scales="free") + theme_bw()
#     } else
#     {
#       graph.fingers <- ggplot() + 
#         geom_point(data=subset(datatraj.graph), aes(frameN, indexZ), size=4.5, color='red', na.rm=T) + 
#         labs(title=sprintf("Trial summary %d/%d",i,ceiling(num.trials/grid.size))) + 
#         geom_point(data=subset(datatraj.graph), aes(frameN, thumbZ), color='blue', size=3.5, na.rm=T) + 
#         geom_point(data=subset(datatraj.graph), aes(frameN, GA), color='black', na.rm=T) + labs(x="frameN", y="fingers Z") +
#         geom_point(data=subset(datatraj.graph, indexOccluded==1), aes(frameN, indexZ), size=4.5, color='palegoldenrod') + 
#         geom_point(data=subset(datatraj.graph, thumbOccluded==1), aes(frameN, thumbZ), color='paleturquoise', size=3.5) + 
#         geom_point(data=subset(datatraj.graph, fingersOccluded==1), aes(frameN, GA), color='yellow3') + 
#         geom_point(data=subset(datakin.graph), aes(frameToMGA, MGA), color="red", size=9) +
#         geom_point(data=subset(datakin.graph), aes(frameToFGA, FGA), color="green", size=6) +
#         geom_point(data=subset(datakin.graph), aes(frameToTGA, TGA), color="cyan", size=6) +
#         geom_point(data=subset(datakin.graph), aes(frameToMGA, IPz.MGA), color="red", size=10) +
#         geom_point(data=subset(datakin.graph), aes(frameToMGA, TPz.MGA), color="blue", size=10) +
#         geom_point(data=subset(datakin.graph), aes(frameToFGA, FIPz), color="green4", size=9) +
#         geom_point(data=subset(datakin.graph), aes(frameToFGA, FTPz), color="yellow3", size=9) + 
#         geom_point(data=subset(datakin.graph), aes(frameToTGA, IPz.TGA), color="cyan", size=9) +
#         geom_point(data=subset(datakin.graph), aes(frameToTGA, TPz.TGA), color="cyan", size=9) + 
#         facet_wrap(~trialN, scales="free") + theme_bw()      
#     }
#     
#     print(graph.fingers)
#   }
#   
#   if(print)
#   {
#     cat('Graphs saved in ', getwd(), '/Trials_summary.pdf\n', sep='')
#     dev.off()
#   }
# }

######################################################################################################## 11.
#' @export
view.single.trial.grasp3d.anim <- function(tr, datatraj, datakin, entireMov=F, type='p')
{
  datatraj$indexOccluded <- with(datatraj, ifelse(is.na(indexXraw), 1, 0))
  datatraj$thumbOccluded <- with(datatraj, ifelse(is.na(thumbXraw), 1, 0))
  datatraj.trial <- subset(datatraj, trialN==tr)[c("frameN","indexX","indexZ","indexY","thumbX","thumbZ","thumbY","fingersOccluded","indexOccluded","thumbOccluded")]
  datakin.trial <- subset(datakin, trialN==tr)

  minx <- min(min(subset(datatraj, trialN==tr)$indexX, na.rm=T),min(subset(datatraj, trialN==tr)$thumbX, na.rm=T),na.rm=T)
  miny <- min(min(subset(datatraj, trialN==tr)$indexY, na.rm=T),min(subset(datatraj, trialN==tr)$thumbY, na.rm=T),na.rm=T)
  minz <- min(min(subset(datatraj, trialN==tr)$indexZ, na.rm=T),min(subset(datatraj, trialN==tr)$thumbZ, na.rm=T),na.rm=T)
  maxx <- max(max(subset(datatraj, trialN==tr)$indexX, na.rm=T),max(subset(datatraj, trialN==tr)$thumbX, na.rm=T),na.rm=T)
  maxy <- max(max(subset(datatraj, trialN==tr)$indexY, na.rm=T),max(subset(datatraj, trialN==tr)$thumbY, na.rm=T),na.rm=T)
  maxz <- max(max(subset(datatraj, trialN==tr)$indexZ, na.rm=T),max(subset(datatraj, trialN==tr)$thumbZ, na.rm=T),na.rm=T)

  colInd <- c("red","palegoldenrod")
  colThu <- c("blue","paleturquoise")
  
  if(type=='p')
  {
    plot3d(datatraj.trial[c("indexX","indexZ","indexY")], col="red", size=15, main=sprintf("trial %d",tr), 
           xlab="X", ylab="Z", zlab="Y", 
           xlim=c(minx,maxx), ylim=c(minz,maxz), zlim=c(miny,maxy), 
           type='n')
    for(i in cc.min(datatraj.trial$frameN):subset(datakin, trialN==tr)$frameToFGA)
    {
      if(is.na(datatraj.trial["indexX"][i,])==F)
      {
        points3d(datatraj.trial[c("indexX","indexZ","indexY")][i,], 
                 col=colInd[datatraj.trial["indexOccluded"][i,]+1], size=15)
      }
      if(is.na(datatraj.trial["thumbX"][i,])==F)
      {
        points3d(datatraj.trial[c("thumbX","thumbZ","thumbY")][i,], 
                 col=colThu[datatraj.trial["thumbOccluded"][i,]+1], size=15)
      }
    }
    
    
    if(entireMov)
    {
      for(i in (i+1):cc.max(datatraj.trial$frameN))
      {
        if(is.na(datatraj.trial["indexX"][i,])==F)
        {
          points3d(datatraj.trial[c("indexX","indexZ","indexY")][i,], col="grey60", size=10)
        }
        if(is.na(datatraj.trial["thumbX"][i,])==F)
        {
          points3d(datatraj.trial[c("thumbX","thumbZ","thumbY")][i,], col="grey70", size=10)
        }
      }
    }
    points3d(datakin.trial[c("IPx.MGA","IPz.MGA","IPy.MGA")], color="black", size=25)
    points3d(datakin.trial[c("TPx.MGA","TPz.MGA","TPy.MGA")], color="black", size=25)
    points3d(datakin.trial[c("FIPx","FIPz","FIPy")], col="green4", size=25)
    points3d(datakin.trial[c("FTPx","FTPz","FTPy")], col="yellow3", size=25)
  }
  if(type=='s')
  {
    plot3d(datatraj.trial[c("indexX","indexZ","indexY")], col="red", radius=5, main=sprintf("trial %d",tr), 
           xlab="X", ylab="Z", zlab="Y", 
           xlim=c(minx,maxx), ylim=c(minz,maxz), zlim=c(miny,maxy),
           type='n')
    for(i in cc.min(datatraj.trial$frameN):datakin.trial$frameToFGA)
    {
      if(is.na(datatraj.trial["indexX"][i,])==F)
      {
        spheres3d(datatraj.trial[c("indexX","indexZ","indexY")][i,], 
                  col=colInd[datatraj.trial["indexOccluded"][i,]+1], radius=5)
      }
      if(is.na(datatraj.trial["thumbX"][i,])==F)
      {
        spheres3d(datatraj.trial[c("thumbX","thumbZ","thumbY")][i,], 
                  col=colThu[datatraj.trial["thumbOccluded"][i,]+1], radius=5)
      }
    }
    if(entireMov)
    {
      for(i in (i+1):cc.max(datatraj.trial$frameN))
      {
        if(is.na(datatraj.trial["indexX"][i,])==F)
        {
          spheres3d(datatraj.trial[c("indexX","indexZ","indexY")][i,], col="grey60", radius=4)
        }
        if(is.na(datatraj.trial["thumbX"][i,])==F)
        {
          spheres3d(datatraj.trial[c("thumbX","thumbZ","thumbY")][i,], col="grey70", radius=4)
        }
      }
    }
    spheres3d(datakin.trial[c("IPx.MGA","IPz.MGA","IPy.MGA")], color="black", radius=10)
    spheres3d(datakin.trial[c("TPx.MGA","TPz.MGA","TPy.MGA")], color="black", radius=10)
    spheres3d(datakin.trial[c("FIPx","FIPz","FIPy")], col="green4", radius=10)
    spheres3d(datakin.trial[c("FTPx","FTPz","FTPy")], col="yellow3", radius=10)
  }
}

######################################################################################################## 12.
#' @export
view.range.of.trials.grasp3d.anim <- function(trials, datatraj, datakin, entireMov=F, type='p'){
  tr <- 1
  while(tr <= length(trials))
  {
    view.single.trial.grasp3d.anim(trials[tr], datatraj, datakin, entireMov, type)
    print("Type 6 and hit Return to go to the next trial. Type 4 and hit Return to go to the previous trial. Type q and hit Return to quit.")
    line <- readline()
    if(line=="4" & tr > 1)
    {
      tr <- tr-1
    }
    if(line=="6")
    {
      tr <- tr+1
    }
    if((line %in% c("4","6"))==F)
    {
      stop("Exit after user keypress.")
    }
  }
}