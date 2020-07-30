#' @title uPolar Visualization tool for time series of microfluidic microscopic images
#'
#' @description AN easy-to-use visualizing tool for Microfluidic based microscopy images to monitor cellular events in biomedical time-series data.
#'
#' @details  Packages :     devtools::install.packages(c("dplyr,plotly,plyr"))
#'
#' @param time_col time array from dataset
#'
#' @param dist_col distance array from dataset to calculate radius
#'
#' @param area_col  area array, if area is unavailable set to NA
#'
#' @param adjust Cell area adjustment for over size cells (better visualization), use  'NA' if area dataset is not available
#'
#' @param refLine  reference line to  for cell alignment monitoring  use  'NA'  for default
#'
#' @param track color code tracking for cells movement and division ( FALSE = no tracking , TRUE = tracking)
#'
#' @param RLS RLS  array from dataset for each time-point (division happened = 1, no division = 0), use  'NA'  if RLS data is not availablle
#'
#' @return Microfludics Time Series ploar plot (RLS optional)
#'
#' @format uPolar(time_col,dist_col,area_col,adjust,refLine,track,RLS)
#'
#' @examples
#' #  df= read.csv("../data/BC8_tp10.csv")
#' #  rls.tp10 = read.csv("../data/BC8_RLS_tp10.csv")
#' #  uPolar(df$time_num,df$dist,df$area,0.05,34,FALSE,rls.tp10$com)
#'
#' @export
#'
uPolar <- function(time_col,dist_col,area_col,adjust,refLine,track,RLS){


  print(' inililizing functions  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> ')


  ###########################  convet time to degree ######################
  t2d <- function (time_col){

    num_images <- length(unique(time_col ))
    theta = c()
    dg =0   # initilize degree

    for  (i in 1 : length(time_col)){
      r1  = time_col[i];
      r2 =  time_col[i+1 ];

      if (is.na(r2)){
        theta[i] = dg
        dg = 0
      }

      else if (r1 < r2){

        theta[i] = dg
        dg= dg + (359/num_images)   # divid time to degree and add to next degree
      }

      else{

        theta[i] = dg
      }
    }

    print(' Step1 : Time converted to degree  >>>>>>>>>>>>>>>>>>>>> ')
    return(theta)
  }

  ############################# function convert row to column###########################
  cbind.fill <- function(...) {
    df.tsps1 <- lapply(list(...),t)
    df.tsps2 <- lapply(df.tsps1, as.data.frame)
    return (data.frame(t(rbind.fill(df.tsps2))))
  }

  ############### Convert distance and theta to   plotly format ########################
  c2r <- function(theta_col, time_col,dist_col){

    df.row = data.frame()
    df.obj = data.frame()

    num_images <- length(unique(time_col))
    max.r = max(dist_col)        # max raduis
    min.r = min(dist_col)        # min raduis
    df.tb <- data.frame(time_col,dist_col,theta_col)

    for (k  in 1: num_images){

      df.t = df.tb[df.tb$time_col== k,]
      df.t = df.t[order(df.t$dist),]      ## sort distance

      #object
      objs <-  nrow(df.t)
      df.obj = rbind(df.obj, objs)

      #Angle
      raduis <- df.t$dist_col  #   collect Distance
      theta <-  df.t$theta_col   # collect angle
      df.com <- cbind(raduis,theta)
      df.row = cbind.fill(df.row,df.com)

    }

    df.obj= as.vector(t(df.obj))    # conver object data.frame to vector


    print(' Step2 : Data Converted to Plotly format >>>>>>>>>>>>>>>')

    #return (list(v1=df.row,v2=df.com, v3=raduis,v4=theta,v5= num_images,v6=df.t))
    return (list(v1=df.row,v2=df.obj,v3= num_images,v4= max.r,v5=min.r))

  }


  ########################## tronspose row to colunm ###########################
  p2p <- function (df.c2r,area_col,adjust,refLine,track,RLS){


    df.rt= df.c2r$v1       # raduis and theta
    df.obj= df.c2r$v2      # total object
    num_images= df.c2r$v3  # total image
    maxD= df.c2r$v4        # max distance
    mixD= df.c2r$v5        # min distance

    rls=c()


    if (mean(is.na(RLS))){
      df_rls = 0;
      print(' RLS data is not available !!!')
    }
    else {
      df_rls = 1;
    }


    print(' Step3: Preparing for Plot >>>>>>>>>>>>>>>>>>>>>>>>>>>>>')

    #####################  Generat Polar Plot ######################
    p <- plot_ly(
      type = 'scatterpolargl',
      mode = 'markers_line'
    )

    ######################## Zero-line Plot ######################
    p <- add_trace(
      p,
      mode= 'lines+markers',
      r = as.list(rep(refLine, 360)),
      theta = c(seq(0, 360, by=1)),
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
        c=   'rgb(255, 255, 255)'  #     no cell
        s=c
      }
      else if (df.obj[i] == 1){
        c=  'rgb(10, 10, 10)'     #     1 cells
        s=c
      }
      else if (df.obj[i] == 2){
        c=  'rgb(120, 150, 237)'   #    2 cells
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


      ####################### Adjust Cell Size ####################

      if (is.na(adjust) | adjust==0 ){
        cellSize = 5
      }
      else{
        cellSize= area_col[i] * adjust
      }


      #####################  apply  color tracking #################
      if( track != 'TRUE'){
        s=c
      }

      p <- add_trace(
        p,
        mode= 'lines+markers',
        r = df.rt[,j],
        theta = df.rt[,k],
        name = paste('t:',i,'\n','C:',df.obj[i]),  # tm  : Time Frame , Cell : Number of Cells
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

      ############################ Read RLS data #######################

      # collect RLS counting and convert time to degree for theta


      if(df_rls==1){

        if((RLS[i])==1){
          dg =  ( i * 359 ) / num_images
          rls=rbind(rls,dg)
        }
      }

      j <- j + 2
      k <- k + 2
    }

    ###################### RLS Plot (out layer) #######################
    if(df_rls==1){

      p <- add_trace(
        p,
        mode= 'markers',
        r = as.list(rep(maxD+5, 360)),
        theta = c(rls),
        name = paste('RLS:','\n'),
        marker = list(
          color = 'rgb(200,10,10)',
          symbol= "star",           # display as STAR
          size = 10,
          opacity = 10)
      )
    }


    print(' Step4: Preparing for layout >>>>>>>>>>>>>>>>>>>>>>>>>>> ')

    #################### Plot layout features  #####################
    p <- layout(
      p,
      title =  paste('Add your image number'),
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

  ############ Calling functions ############

  # time_col : time array
  # t2d <- function ( time_col)
  df.t2d = t2d(df$time_num)

  ##########################################

  # theta_col : angle array (theta)
  # time_col : time array
  # dist_col : distance array
  # c2r <- function(theta_col, time_col,dist_col)
  df.c2r <- c2r(df.t2d,df$time_num,df$dist)

  #########################################

  # raduis and theta arrays
  # area_col : object area array
  # adjust : cell size
  #  refLine :  reference line for analysis
  # track :  tacking cell by color-code , FALSE or TRUE
  #  RLS :  RLS  array  (format ( 0,0,1,0,0,1,0,0,...)  count 1 at each tim-point )

  # p2p <- function (df.c2r,area_col,adjust,refLine,track,RLS){
  p<- p2p(df.c2r,area_col,adjust,refLine,track,RLS)


  ########################################
  print(' Step5: Plot uPolar >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>')
  p

}

####################################################################################################
##################################### How to run the function from here ############################


# time_col : time array from  dataset
# dist_col : distance array from dataset
# area_col : object area array  from dataset   ( unavailable = NA)
# adjust : cell size adjustment  ( unavailable = NA)
#  refLine : reference line for analysis  ( unavailable = NA)
# track :  tacking cell by color-code , FALSE or TRUE
#  RLS :  RLS  array  from dataset (format ( 0,0,1,0,0,1,0,0,...)  count 1 at each time-point )
#( RLS division happen = 1, no division = 0 ), set to 'NA' if it is not available

########################################### Test Plot ##############################################

# dataset
#df= read.csv("../data/BC8_tp10.csv")

# RLS dataset
#rls.tp10 = read.csv("../data/BC8_RLS_tp10.csv")

#function
#uPolar(df$time_num,df$dist,df$area,0.05,34,FALSE,rls.tp10$com)

###################################################################################################
