#' @title uPolar Visualization tool for time series of microfluidic microscopic images
#'
#' @description AN easy-to-use visualizing tool for Microfluidic based microscopy images to monitor cellular events in biomedical time-series data.
#'
#'@details  Packages :     devtools::install.packages(c("dplyr,plotly,plyr"))
#'
#' ibrary(plotly)
#'
#' library(dplyr)
#'
#' library(plyr)
#'
#' @param df  Dataset, features order ( Time for each image , Total cell number, cell area (optional), cell distance )
#'
#' @param time_col  Time number for each image
#'
#' @param dist_col  Cell distance from reference point to calculate radius
#'
#' @param area_col  Individual cell area, if area is unavailable set to NA
#'
#' @param offset  Raduis between maximum cell raduis and plolar plot outlayer line (better visualization),  use  'NA'  for default
#'
#' @param adjust  Cell area adjustment for over size cells (better visualization), use  'NA' if area dataset is not available
#'
#'@param track  color coding cell in orderto track cell movement and division ( FALSE = no tracking , TRUE = tracking)
#'
#' @param RLS RLS dataset for each time-point (division happened = 1, no division = 0), use  'NA'  if RLS data is not availablle
#'
#' @return  Microfludics Time Series ploar plot (RLS optional)
#'
#' @format uPolar(time_col,dist_col,area_col,offset,adjust,track,RLS)
#'
#' @examples
#' library(dplyr)
#' library(plotly)
#' library(plyr)
#' df = read.csv("../data/BC8_Tp10.csv")
#' uPolar(df, 1,2,NA,NA,NA,FALSE,NA)
uPolar <- function(df,time_col,dist_col,area_col,offset,adjust,track,RLS){

  ########################### checking enteries ########################################
  if ( missing(df)){
    print('missing "data" entery !!!')
  }

  if (missing(time_col)){
    print('missing time column number !!!')
  }

  if (is.na(dist_col)){
    print('missing distance column number !!!')
  }

  if (is.na(offset)){
    offset = 5;
    print(' Offset set to 5 !!!')
  }

  if (mean(is.na(RLS))){

    df_rls = 0;
    print('RLS data not available !!!')

    }
    else {

      df_rls = 1;
    }

  if( missing(area_col) | is.na(area_col)){   # if area colunm is unavailable
    df.area = 0 ;
    print('Area data not available !!!')

    }
    else{
      df.area = df[,area_col];
      print('Area data available, set c.Adjust to 1 - 50 !!!')
    }


  t2d <- function (df, time_col){

    num_images <- length(unique(df[,time_col] ))

    df$theta = 'NA'
    dg =0   # initilize degree

    for  (i in 1 : nrow(df)){

      r1  = df[i, ];
      r2 =  df[i+1, ];

      if (is.na(r2[,time_col])){
        #if (is.na(r2$time_num)){  # set degree to zero for unavailable data

        df$theta[i] = dg
        dg = 0

      }

      else if (r1[,time_col] < r2[,time_col]){

        df$theta[i] = dg
        dg= dg + (359/num_images)   # divid time to degree and add to next degree

      }

      else

        df$theta[i] = dg
    }

    print(' Step1 : Time converted to degree (theta) ')
    return(df)

  }

  cbind.fill <- function(...) {
    df.tsps1 <- lapply(list(...),t)
    df.tsps2 <- lapply(df.tsps1, as.data.frame)
    return (data.frame(t(rbind.fill(df.tsps2))))
  }


  # Convert distance and theta to  order of time for plotly format
  c2r <- function(df,time_col,dist_col,area_col,theta_col){

    df.row = data.frame()
    df.obj = data.frame()
    num_images <- length(unique(df[,time_col] ))


    max.r = max(df[,dist_col])        # max raduis
    min.r = min(df[,dist_col])        # min raduis

    for (k  in 1: num_images){

      df.t = df[df[,time_col]==k,]
      df.t = df.t[order(df.t$dist),]  # sort sidtance


      if(mean(df.t$dist== 0 & nrow(df.t) ==1)) {

        objs <-  0
      }
      else{

        objs <-  nrow(df.t)
      }

      raduis <- df.t[,dist_col]  # Distance
      theta <-  df.t[,theta_col]       # Angle
      df1 <- cbind(raduis,theta)

      df.row = cbind.fill(df.row,df1)
      df.obj = rbind(df.obj, objs)

    }
    #df.row  <- df.row [,-c(1)]     # remove first colunm
    df.obj= as.vector(t(df.obj))   # conver object data.frame to vector

    print(' Step2 : Data Converted to Plotly format ')

    return (list(v1=df.row,v2=df.obj,v3=df.area, v4=max.r,v5=min.r,v6= num_images))

  }

  ##### tronspose row to colunm ###################################################

  p2p <- function (df,offset,adjust,track,RLS){

    df.1= df$v1  # raduis and theta
    df.2= df$v2  # total Object
    df.3= df$v3  # area
    max.area = median(df.3)
    num_images <- df$v6
    rls=c()

    print(' Step3: Preparing for Plot ')


    if ( is.na(adjust) | mean(df.3==0) & adjust==0){
      adjust = 0;
      print('  default Cell Size  set to 5  ')
    }

    #####################  Generat Polar Plot #####################
    p <- plot_ly(
      type = 'scatterpolargl',
      r = c((df$v5)-30, df$v4+offset),
      mode = 'markers_line'
    )

    ######################## Zero-line Plot ######################
    p <- add_trace(
      p,
      mode= 'lines+markers',
      r = as.list(rep(0, 360)),
      theta = c(seq(0, 360, by=1)),
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

    for (i in 1:(length(df.1)/2)){

    ################### Adjust Cell Size #########################

      if (adjust==0){
        cellSize = 5
      }
      else{
        cellSize= df.3[i]/max.area+adjust
      }

      ####################### Color Condition #####################

      if(df.2[i] == 0){
        c=   'rgb(255, 255, 255)'  #     no cell
        s=c
      }
      else if (df.2[i] == 1){
        c=  'rgb(10, 10, 10)'     #     1 cells
        s=c
      }
      else if (df.2[i] == 2){
        c=  'rgb(120, 150, 237)'   #    2 cells
        s= c('rgb(178, 34, 34)','rgb(148,0,211)')

      }
      else if (df.2[i] == 3){
        c = 'rgb(219,120,147)'     #     3 cells
        s= c('rgb(178, 34, 34)','rgb(148,0,211)','rgb(65, 105,225)')
      }
      else if (df.2[i] == 4 ){
        c=  'rgb(10, 140, 30)'     #     4 cells
        s= c('rgb(178, 34, 34)','rgb(148,0,211)','rgb(65, 105,225)','rgb(46,139, 87)')
      }
      else if (df.2[i] == 5 ){
        c=  'rgb(148, 10, 180)'    #     5 cells
        s= c('rgb(178, 34, 34)','rgb(148,0,211)','rgb(65, 105,225)','rgb(46,139, 87)',
             'rgb(225, 165, 0)')
      }

      else if (df.2[i] == 6 ){
        c=  'rgb(32, 178, 170)'    #     6 cells
        s= c('rgb(178, 34, 34)','rgb(148,0,211)','rgb(65, 105,225)','rgb(46,139, 87)',
             'rgb(225, 165, 0)','rgb(105, 105, 105)')
      }

      else if (df.2[i] == 7 ){
        c=  'rgb(127, 255, 212)'    #     7 cells
        s= c('rgb(178, 34, 34)','rgb(148,0,211)','rgb(65, 105,225)','rgb(46,139, 87)',
             'rgb(225, 165, 0)','rgb(105, 105, 105)','rgb(25, 25, 112)')
      }

      else if (df.2[i] == 8 ){
        c=  'rgb(173, 150, 230)'    #     8 cells
        s= c('rgb(178, 34, 34)','rgb(148,0,211)','rgb(65, 105,225)','rgb(46,139, 87)',
             'rgb(225, 165, 0)','rgb(105, 105, 105)','rgb(25, 25, 112)','rgb(139,69,19)')
      }

      else if (df.2[i] == 9 ){
        c=  'rgb(1, 0, 128)'       #     9 cells
        s= c('rgb(178, 34, 34)','rgb(148,0,211)','rgb(65, 105,225)','rgb(46,139, 87)',
             'rgb(225, 165, 0)','rgb(105, 105, 105)','rgb(25, 25, 112)','rgb(139,69,19)',
             'rgb(255,99,71)')
      }

      else if (df.2[i] == 10 ){
        c=  'rgb(139, 0, 139)'     #     10 cells
        s= c('rgb(178, 34, 34)','rgb(148,0,211)','rgb(65, 105,225)','rgb(46,139, 87)',
             'rgb(225, 165, 0)','rgb(105, 105, 105)','rgb(25, 25, 112)','rgb(139,69,19)',
             'rgb(255,99,71) ','rgb(34,139,3)')
      }

      else if (df.2[i] == 11 ){
        c=  'rgb(255, 10, 255)'    #     11 cells
        s= c('rgb(178, 34, 34)','rgb(148,0,211)','rgb(65, 105,225)','rgb(46,139, 87)',
             'rgb(225, 165, 0)','rgb(105, 105, 105)','rgb(25, 25, 112)','rgb(139,69,19)',
             'rgb(255,99,71) ','rgb(34,139,3)','rgb(1,1,139)')
      }

      else if (df.2[i] == 12 ){
        c=  'rgb(210, 100, 30)'    #     12 cells
        s= c('rgb(178, 34, 34)','rgb(148,0,211)','rgb(65, 105,225)','rgb(46,139, 87)',
             'rgb(225, 165, 0)','rgb(105, 105, 105)','rgb(25, 25, 112)','rgb(139,69,19)',
             'rgb(255,99,71) ','rgb(34,139,3)','rgb(1,1,139)','rgb(188,143,100)')
      }

      else {
        c=  'rgb(0, 255, 0)'    #   above 12 cells
        s = 'rgb(255, 0, 0)'    #   above 12 cells
      }


      #######  apply tracking #######
      if( track != 'TRUE'){
        s=c
      }


      p <- add_trace(
        p,
        mode= 'lines+markers',
        r = df.1[,j],
        theta = df.1[,k],
        name = paste('t:',i,'\n','C:',df.2[i]),  # tm  : Time Frame , Cell : Number of Cells
        line = list(
          size = 0.2,
          color= c),
        marker = list(
          color =  s,
          size = cellSize,
          line = list(
            color = c
          ),
          opacity = 10,
          showlegend = F,
          line = c)
      )

      ################################ Read RLS data #######################

      # collect RLS counting and convert time to degree for theta

      if(df_rls==1){

          if(mean(RLS[i])==1){
            dg =  ( i * 359 ) / num_images
            rls=rbind(rls,dg)
          }
      }

      j <- j + 2
      k <- k + 2
    }

    ######################## RLS Plot (out layer) ######################
    if(df_rls==1){

        p <- add_trace(
          p,
          mode= 'markers',
          r = as.list(rep(df$v4+offset, 359)),
          theta = c(rls),
          marker = list(
            color = 'rgb(100,100,100)',
            symbol= "star",           # display as STAR
            size = 15,
            opacity = 10)
        )
    }
    print(' Step4: Preparing for layout ')

    #################### Plot layout features  #####################
    p <- layout(
      p,
      title =  paste('Add your image number :', 0 ),
      showlegend = T,
      paper_bgcolor = "rgb(256, 256, 256)",  #  outside color
      polar = list(
        bgcolor = "rgb(230, 230, 230)",     #   polar background color
        angularaxis = list(
          tickwidth = 3,
          linewidth = 2,
          layer = 'below traces'
        ),
        radialaxis = list(
          showline = T,
          linewidth = 1,
          tickwidth = 1,
          gridcolor = 'rgb(256,256,256)',
          gridwidth = 0.3
        )
      )
    )
  }

  ############# Calling functions #################

  # 0 : Dataset
  # 1 : time col

  #t2d <- function (df, time_col){
  df.t2d = t2d(df,1)
  ############################
  # 0 : data from t2d function
  # 1 : time col
  # 2 : area_col
  # 3 : dist_col
  # 4 : theta_col

  #c2r <- function(df,time_col,dist_col,area_col,theta_col){
  df.c2r <- c2r(df.t2d,1,2,3,4)
  #########################################
  # 0 : data
  # 1 : offset raduis from outlayer ( Default = 0 )
  # 2 : Adjust cell size to be desplay with actuall zise or defult = 0
  # 3 : Track cell by color code , FALSE or TRUE
  # 4 : RLS data  NA = unavailable RLS data or  RLS dataset

  #p2p <- function (df,offset,c.Adjust,RLS){
  p<- p2p(df.c2r,offset,adjust,track,RLS)
  ########################################

  ######## Plot (p) ##########

  print(' Step5: Plot uPolar ')
  p
}


####################################################################################################
##################################### How to run the function from here ############################

# 0 : df, dataset
# 1 : time col , time colunm number in dataset
# 2 : dist_col,  distance colunm number in dataset
# 3 : area_col, area colunm number in dataset, if the area is unavailable, set to 'NA'
# 4 : offset,  ajdust max value of data from out-layer for better data visulization
# 5 : ajust,  adjust cell size if the  data area is available, otherwise set to  'NA'
# 6 : track,  tracking cell by color code  (FALSE = no tracking , TRUE= tracking)
# 7 : RLS, RLS dataset time-point ( division happened = 1, no division = 0), set to 'NA' if it is not available

########################################### Test Plot ##############################################

#df = read.csv("../data/BC8_Tp10.csv")                 # read data for trap 10
#df= select(df,"time","dist","area")                   # arrange feature in order
#df.tp1 = read.csv("../data/BC8_RLS_tp10.csv")         # read RLS for trap 10
#RLS =df.tp1$com                                       # select RLS column data (optional)
#uPolar(df,1,2,NA,NA,NA,FALSE,NA)                       # apply uPolar function

###################################################################################################
