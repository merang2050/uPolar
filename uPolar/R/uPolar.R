#' @title uPolar An Interactive 2D Visualization Tool for Microscopic Time-series Images
#'
#' @description AN easy-to-use visualizing tool for Microfluidic based microscopy images to monitor cellular events in biomedical time-series data.
#'
#' @details  Packages :     devtools::install.packages(c("dplyr,plotly,plyr"))
#'
#' @param df dataset (CSV format), Features be named : Time, X, Y, Area (optional), RLS (optional)
#'
#' @param refPoint Given numerical image reference point to calculate distance, format: c(xr,yr), default (refPoint= NULL) c(0,0)
#'
#' @param title Plot title, default (title = NULL)
#'
#' @param refLine  numerical value for reference line (red) on the plot, default (refLine = NULL)
#'
#' @param Area Area = FALSE (no area data available) , Area =TRUE (area data available)
#'
#' @param aAdjust  numerical value for adjust cell size by multiplication, default (aAdjust = NULL)
#'
#' @param zeroAdjust  numerical value Adjust minimum cell distance from plot center point, default (zeroPoint = NULL)
#'
#' @param track color tag tracking for cells movement and division, trak = FALSE (no tag tracking), track = TRUE (tag tracking)
#'
#' @param RLS RLS = FALSE (no RLS data available), RLS = TRUE (RLS data available)
#'
#' @return Microfludics Time Series uploar plot
#'
#' @format uPolar(df,refPoint=NULL,title=NULL,refLine=NULL,Area=FALSE,aAdjust=NULL,zeroAdjust= NULL,track = FALSE,RLS= FALSE)
#'
#' @examples
#' #  df= read.csv("../data/BC8_tp10_TpTmXYAreaRLS.csv")
#' #  uPolar(df,refPoint=NULL,title=NULL,refLine=NULL,Area=FALSE,aAdjust=NULL,
#' #        zeroAdjust= NULL,track = FALSE,RLS= FALSE)
#'
#' @export
#'
uPolar <- function(df,refPoint=NULL,title=NULL,refLine=NULL,Area=FALSE,aAdjust=NULL,zeroAdjust= NULL,track = FALSE,RLS= FALSE){
  ###################################################### checking enteries ######################################################
  if ( missing(df)){
    print(' missing "data" entery !!!')
  }
  if (is.null(refPoint)){
    refPoint = c(0,0)
    print(' default reference point  c(0,0) !!!')
  }
  if (is.null(refLine)){   # if area colunm is unavailable
    refLine =  5 ;
    print('  defualt reference line = 5 !!!')
  }
  if (is.null(title)){   # if area colunm is unavailable
    title =  "no title" ;
    print('  defualt title !!!')
  }
  if(isFALSE(Area)){
    df$Area <- 5
    cSize <- df$Area
    print('defualt cell size = 5 !!!')
  }
  else{
    cSize<- df$Area
    print('Area is available!!!')
  }
  if (is.null(zeroAdjust)){
    zeroAdjust =  0 ;
    print('no adjustment from zero point !!!')
  }
  if (isFALSE(RLS)){
    RLS = NULL  ;
    print('NO RLS available !!!')
  }else{
    RLS = df$RLS
    print('RLS is available !!!')
  }
  cbind.fill <- function(...) {
    df.tsps1 <- lapply(list(...),t)
    df.tsps2 <- lapply(df.tsps1, as.data.frame)
    return (data.frame(t(rbind.fill(df.tsps2))))
  }
  # Distance function
  rxyDist <-  function(x1, y1, x2, y2) {sqrt( (x1 - x2)^2 + (y1-y2)^2 )};
  # Time to degree function
  t2d <- function (df){
    time <- df$Time
    num_images <- length(unique(time))
    theta = c()
    dg = 0   # initilize degree
    for  (i in 1 : length(time)){
      r1  = time[i];
      r2 =  time[i+1 ];
      if (is.na(r2)){
        theta[i] = dg
        dg = 1
      }
      else if (r1 < r2){
        theta[i] = dg
        dg= dg + (360/num_images)   # divid time to degree and add to next degree
      }
      else{
        theta[i] = dg
      }
    }
    df$Theta <- theta
    print(" Step1 : Time converted to degree  >>>>>>>>>>>>>>>>>>>>")
    return(df)
  }
  # Convert distance and theta to  order of time for plotly format
  c2r <- function(df,refPoint,refLine){
    ref_X <- refPoint[1]
    ref_Y <- refPoint[2]
    for ( n in 1: nrow(df)){
      if (df$X[n] ==0 & df$Y[n]==0){
        df$Dist[n] = refLine
      }
      else{
        df$Dist[n]  <- rxyDist(df$X[n], df$Y[n],ref_X,ref_Y);
      }
    }
    df.row = data.frame()
    df.obj = data.frame()
    num_images <- length(unique(df$Time))
    max.r = max(df$Dist)        # max raduis
    min.r = min(df$Dist)        # min raduis
    for (k  in 1: num_images){
      df.t = df[df$Time== k,]
      # count object
      if(mean(df.t$X) == 0 & mean(df.t$Y) == 0){
        objs <-  0
        df.obj = rbind(df.obj, objs)
      }
      else{
        objs <-  nrow(df.t)
        df.obj = rbind(df.obj, objs)
      }
      raduis <- df.t$Dist       # collect Distance
      theta <-  df.t$Theta      # collect angle
      df.com <- cbind(raduis,theta)
      df.row = cbind.fill(df.row,df.com)
    }
    df.obj= as.vector(t(df.obj))    # conver object data.frame to vector
    print(" Step2 : Data Converted to Plotly format >>>>>>>>>>>>>>")
    return (list(v1=df.row,v2=df.obj,v3= num_images,v4= max.r,v5=min.r,v6 = refLine))
  }
  ################### Apply plotly plot  ##################
  p2p <- function (df.c2r,title,cSize,aAdjust,zeroAdjust,track,RLS){
    df.rt= df.c2r$v1       # raduis and theta
    df.obj= df.c2r$v2      # total object
    num_images= df.c2r$v3  # total image
    maxD= df.c2r$v4        # max distance
    mixD= df.c2r$v5        # min distance
    refLine <- df.c2r$v6
    rls=c()
    print(" Step3: Preparing for Plot >>>>>>>>>>>>>>>>>>>>>>>>>>>>")
    #####################  Generat Polar Plot #####################
    p <- plot_ly(
      type = 'scatterpolargl',
      mode = 'markers_line'
    )
    ######################## Zero-line Plot ######################
    p <- add_trace(
      p,
      mode= 'lines+markers',
      r = as.list(rep(zeroAdjust+refLine, 360)),
      theta = c(seq(1, 360, by=1)),
      name = paste('refLine:','\n'),
      line = list(
        size = 1,
        color=  'rgb(255,0,0)'  ),
      marker = list(
        color = 'rgb(255,0,0)',
        size = 2,
        line = list(
          color = 'black'
        ),
        opacity = 0.5
      )
    )
    j = 1
    k = 2
    for (i in 1:(length(df.rt)/2)){
      ####################### Color Condition #####################
      if(df.obj[i] == 0){
        c=   'rgb(250, 250, 250)'  #     no cell
        s=c
      }
      else if (df.obj[i] == 1){
        c=  'rgb(10, 10, 10)'     #     1 cells
        s=c
      }
      else if (df.obj[i] == 2){
        c=  'rgb(120, 150, 255)'   #    2 cells
        s= c('rgb(178, 34, 34)','rgb(148,0,211)')
      }
      else if (df.obj[i] == 3){
        c = 'rgb(219,120,147)'     #     3 cells
        s= c('rgb(178, 34, 34)','rgb(148,0,211)','rgb(65, 105,225)')
      }
      else if (df.obj[i] == 4 ){
        c=  'rgb(10, 140, 30)'     #     4 cells
        s= c('rgb(178, 34, 34)','rgb(148,0,211)','rgb(65, 105,225)','rgb(46,139, 87)')
      }
      else if (df.obj[i] == 5 ){
        c=  'rgb(148, 10, 180)'    #     5 cells
        s= c('rgb(178, 34, 34)','rgb(148,0,211)','rgb(65, 105,225)','rgb(46,139, 87)',
             'rgb(225, 165, 0)')
      }
      else if (df.obj[i] == 6 ){
        c=  'rgb(32, 178, 170)'    #     6 cells
        s= c('rgb(178, 34, 34)','rgb(148,0,211)','rgb(65, 105,225)','rgb(46,139, 87)',
             'rgb(225, 165, 0)','rgb(105, 105, 105)')
      }
      else if (df.obj[i] == 7 ){
        c=  'rgb(127, 190, 212)'    #     7 cells
        s= c('rgb(178, 34, 34)','rgb(148,0,211)','rgb(65, 105,225)','rgb(46,139, 87)',
             'rgb(225, 165, 0)','rgb(105, 105, 105)','rgb(25, 25, 112)')
      }
      else if (df.obj[i] == 8 ){
        c=  'rgb(173, 120, 140)'    #     8 cells
        s= c('rgb(178, 34, 34)','rgb(148,0,211)','rgb(65, 105,225)','rgb(46,139, 87)',
             'rgb(225, 165, 0)','rgb(105, 105, 105)','rgb(25, 25, 112)','rgb(139,69,19)')
      }
      else if (df.obj[i] == 9 ){
        c=  'rgb(100, 20, 200)'       #     9 cells
        s= c('rgb(178, 34, 34)','rgb(148,0,211)','rgb(65, 105,225)','rgb(46,139, 87)',
             'rgb(225, 165, 0)','rgb(105, 105, 105)','rgb(25, 25, 112)','rgb(139,69,19)',
             'rgb(255,99,71)')
      }
      else if (df.obj[i] == 10 ){
        c=  'rgb(139, 0, 139)'     #     10 cells
        s= c('rgb(178, 34, 34)','rgb(148,0,211)','rgb(65, 105,225)','rgb(46,139, 87)',
             'rgb(225, 165, 0)','rgb(105, 105, 105)','rgb(25, 25, 112)','rgb(139,69,19)',
             'rgb(255,99,71) ','rgb(34,80,3)')
      }
      else if (df.obj[i] == 11 ){
        c=  'rgb(255, 10, 255)'    #     11 cells
        s= c('rgb(178, 34, 34)','rgb(148,0,211)','rgb(65, 105,225)','rgb(46,139, 87)',
             'rgb(225, 165, 0)','rgb(105, 105, 105)','rgb(25, 25, 112)','rgb(139,69,19)',
             'rgb(255,99,71) ','rgb(34,139,3)','rgb(1,1,139)')
      }
      else if (df.obj[i] == 12 ){
        c=  'rgb(210, 100, 30)'    #     12 cells
        s= c('rgb(178, 34, 34)','rgb(148,0,211)','rgb(65, 105,225)','rgb(46,139, 87)',
             'rgb(225, 165, 0)','rgb(105, 105, 105)','rgb(25, 25, 112)','rgb(139,69,19)',
             'rgb(255,99,71) ','rgb(34,139,3)','rgb(1,1,139)','rgb(188,143,100)')
      }
      else {
        c=  'rgb(0, 255, 0)'    #   above 12 cells
        s = 'rgb(255, 0, 0)'    #   above 12 cells
      }
      #####################  apply color tracking #####################
      if( isFALSE(track)){
        s=c
      }
      ############################ Read RLS data #######################
      if(!is.null(RLS)){
        if(mean(RLS[i])==1){
          dg =  ( i * 360 ) / num_images
          rls=rbind(rls,dg)
        }
      }
      ####################### Plot cells data ########################
      p <- add_trace(
        p,
        mode= 'lines+markers',
        r = zeroAdjust+df.rt[,j],
        theta = df.rt[,k],
        name = paste('t:',i,'\n','C:',df.obj[i]),  # tm  : Time Frame , Cell : Number of Cells
        line = list(
          size = 0.2,
          color= c),
        marker = list(
          color =  s,
          size = cSize[i] * aAdjust,
          line = list(
            color = c
          ),
          opacity = 10,
          showlegend = F,
          line = c)
      )
      j <- j + 2
      k <- k + 2
    }
    ###################### RLS Plot (out layer) #######################
    if(!is.null(RLS)){
      p <- add_trace(
        p,
        mode= 'markers',
        r = as.list(rep(5+maxD+zeroAdjust, 360)),
        theta = c(rls),
        name = paste('refLine:','\n'),
        marker = list(
          color = 'rgb(255,0,0)',
          symbol= "star",           # display as STAR
          size = 10,
          opacity = 10)
      )
    }
    print(" Step4: Preparing for layout >>>>>>>>>>>>>>>>>>>>>>>>>>")
    #################### Plot layout features  #####################
    p <- layout(
      p,
      title =  paste('>     <',  title ),
      showlegend = T,
      paper_bgcolor = "rgb(256, 256, 256)",  #  outside color
      polar = list(
        bgcolor = "rgb(200, 200, 200)",     #   polar background color
        angularaxis = list(
          tickwidth = 3,
          linewidth = 2,
          layer = 'below traces'
        ),
        radialaxis = list(
          showline = T,
          linewidth = 1,
          tickwidth = 1,
          gridcolor = 'rgb(225,225,225)',
          gridwidth = 0.2

        )
      )
    )
  }
  df.t2d <- t2d(df)
  df.c2r <- c2r(df.t2d,refPoint,refLine)
  p<- p2p(df.c2r,title,cSize,aAdjust,zeroAdjust,track,RLS)

  print(" Step5: Finializing the Plot >>>>>>>>>>>>>>>>>>>>>>>>>>")
  p
}
##################################################################################################
################################### How to run the function from here ############################
# df:           dataset [ csv format : Time, X , Y ,  Area(optional) , RLS (optional)]
# refPoint:     Given reference point to calculate distance [ defualt= c(0,0) ]
# title:        Plot title
# refLine:      reference line(red) on the plot
# Area:         TURE: Area available, FALSE: NO Area available
# zeroAdjust:   Adjust plot from plot center point
# track :       TURE: color tag available, FALSE: NO color tag available
# RLS :         TURE: RLS available, FALSE: NO RLS available
########################################### Test Plot ###########################################
#library(dplyr)
#library(plotly)
#library(plyr)

# dataset
#df= read.csv("../data/BC8_tp10_TpTmXYAreaRLS.csv")
#function
#uPolar(df,refPoint=NULL,title=NULL,refLine=NULL,Area=FALSE,aAdjust=NULL,zeroAdjust= NULL,track = FALSE,RLS= FALSE)
##################################################################################################################
