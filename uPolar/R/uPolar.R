#' @title μPolar Visualization tool for time series of microfluidic microscopic images
#'
#' @description AN easy-to-use visualizing tool for Microfluidic based microscopy images to monitor cellular events in biomedical time-series data.
#'
#' @param dataset
#'
#' @param time number for each image
#'
#' @param total cells number at each image
#'
#' @param individual cell area (set it to zero (0) if area is unavailable)
#'
#' @param cell distance from reference point to calculate radius
#'
#' @param offset between maximum cell raduis and plolar plot outlayer line (better visualization)
#'
#' @param cell area adjustment for over size cells ( better visualization)
#'
#' @param total number of images
#'
#' @return Microfludics Time Series ploar plot (RLS optional)
#'
#' @format time_col,obj_col,area_col,dist_col,offset,c.Adjust,num_images)
#'
#' @examples uPolar(df,1,2,3,4,0,7,391)
#'
#' @note   install packages : dplyr , plyr , plotly
library(dplyr)
library(plotly)
library(plyr)
uPolar <- function(df,time_col,obj_col,area_col,dist_col,offset,c.Adjust,num_images){

  print(' Step1 : Loading data ')

  t2d <- function (df, time_col,num_images){

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
        dg= dg + (360/num_images)   # divid time to degree and add to next degree

      }

      else

        df$theta[i] = dg
    }

    print(' Step2 : Time converted to degree (theta) ')
    return(df)

  }

  cbind.fill <- function(...) {
    df.tsps1 <- lapply(list(...),t)
    df.tsps2 <- lapply(df.tsps1, as.data.frame)
    return (data.frame(t(rbind.fill(df.tsps2))))
  }


  ##### Data format (time_col,objs_col,area_col,dist_col, theta_col)######
  ##### Data format ( 1,        2,        3,        4,        5    )######


  # Convert distance and theta to  order of time for plotly format
  c2r <- function(df,time_col,objs_col,area_col,dist_col,theta_col,num_images ){

    df.row = data.frame()
    df.obj = data.frame()

    max.r = max(df[,dist_col])        # max raduis
    min.r = min(df[,dist_col])        # min raduis


    if( area_col==0){   # if area colunm is unavailable
      df.area = 0
    }
    else{
      df.area = df[,area_col]
    }


    for (k  in 1: num_images){

      df.t = df[df[,time_col]==k,]

      objs = mean(df.t[,objs_col])   # Objects
      objs = mean(df.t[,objs_col])   # Objects
      raduis <- df.t[,dist_col]  # Distance
      theta <-  df.t[,theta_col]       # Angle
      df1 <- cbind(raduis,theta)

      df.row = cbind.fill(df.row,df1)
      df.obj = rbind(df.obj, objs)

    }
    #df.row  <- df.row [,-c(1)]     # remove first colunm
    df.obj= as.vector(t(df.obj))   # conver object data.frame to vector
    print(' Step3 : Data Converted to Plotly format ')

    return (list(v1=df.row,v2=df.obj,v3=df.area, v4=max.r,v5=min.r))

  }

  ##### tronspose row to colunm ###################################################

  p2p <- function (df,off_set,c.Adjust ){

    df.1= df$v1  # raduis and theta
    df.2= df$v2  # total Object
    df.3= df$v3  # area
    max.area = median(df.3)
    rls=c()


    if ( missing(df)){
      print('missing "data" entery')
    }
    else if (missing(off_set)){
      off_set = 5;
      print('offset : 0 entery,  set to  5 (default) ')
    }
    else if (missing(c.Adjust) | c.Adjust==0 | mean(df.3==0) & c.Adjust==0){
      c.Adjust = 0;
      print(' Cell Size : 0 entery, set to  5 (default) ')
    }
    else{
      print(' Correct Entery')
    }

    print(' Step4: Preparing for Plot ')


    #####################  Generat Polar Plot #####################
    p <- plot_ly(
      type = 'scatterpolargl',
      r = c((df$v5)-30, df$v4+off_set),
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
        color=  'rgb(10,10,10)'  ),
      marker = list(
        color = 'rgb(10,10,10)',
        size = 2,
        line = list(
          color = 'black'
        ),
        opacity = 0.5
      )
    )

    ######################## Distance Plot ######################
    j = 1
    k = 2

    for (i in 1:(length(df.1)/2)){

      ################### Adjust Cell Size #########################

      if (c.Adjust==0){
        cellSize = 5
      }
      else{
        cellSize= df.3[i]/max.area+c.Adjust
      }

      ####################### Color Condition #####################
      if(df.2[i] == 0){
        c=   'rgb(255, 255, 255)'  #     no cell
      }
      else if (df.2[i] == 1){
        c=  'rgb(10, 10, 10)'      #     1 cells
      }
      else if (df.2[i] == 2){
        c=  'rgb(120, 150, 237)'   #    2 cells
      }
      else if (df.2[i] == 3){
        c = 'rgb(219,120,147)'     #     3 cells
      }
      else if (df.2[i] == 4 ){
        c=  'rgb(10, 140, 30)'     #     4 cells
      }
      else if (df.2[i] == 5 ){
        c=  'rgb(148, 10, 180)'    #     5 cells
      }
      else {
        c=  'rgb(220, 180, 10)'    #   above 5 cells
      }

      p <- add_trace(
        p,
        mode= 'lines+markers',
        r = df.1[,j],
        theta = df.1[,k],
        name = paste('Tm:',i,'\n','Ce:',df.2[i]),  # tm  : Time Frame , Cell : Number of Cells
        line = list(
          size = 0.2,
          color= c),
        marker = list(
          color =  c,
          size = cellSize,
          line = list(
            color = c
          ),
          opacity = 10,
          showlegend = F,
          line = c)
      )

      if(df.2[i]==1){
        rls=rbind(rls,i)
      }

      j <- j + 2
      k <- k + 2

    }

    ######################## RLS Plot (out layer) ######################
    # p <- add_trace(
    #     p,
    #     mode= 'markers',
    #     r = as.list(rep(df$v4+off_set, 360)),
    #     theta = c(rls),
    #     marker = list(
    #       color = 'rgb(10,10,10)',
    #        symbol= "star",
    #       size = 8,
    #       opacity = 10)
    #     )
    #
    print(' Step5: Preparing for layout ')

    # Plot layout features

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
  # 2 : num_images

  df.t2d = t2d(df,1,num_images)

  # 0 = data from t2d function
  # 1 : time col
  # 2 : obj_col
  # 3 : area_col
  # 4 : dist_col
  # 5 : theta_col
  # 6 : num_images

  df.c2r <- c2r(df.t2d,1,2,3,4,5,num_images)

  # 0 = data
  # 1 : offset raduis from outlayer ( Default = 0 )
  # 2 : Adjust cell size to be desplay with actuall zise or defult = 0

  p<- p2p(df.c2r,offset,c.Adjust)

  p
}
#############################################################################################
############################### How to run the function from here ###########################
#############################################################################################


################ Load data ##################

#df = read.csv("../data/BC8_Tp1.csv")
#df= select(df,"time_num","total_objs","Est_area","dist")  #arrange feature in order

#######  uPolar Function  Arguments ##########

##### Data format (df, time_col,objs_col,area_col,dist_col,offset, c.Adjust ,num_images)######
##### Data format (0 ,   1,        2,        3,      4,    offset, c.Adjust ,num_images)######


# 0 : df, dataset
# 1 : time col , time colunm number in dataset
# 2 : obj_col, total total cell colunm number in datasetzx
# 3 : area_col, area colunm number in dataset, if the area is unavailabl, set to zero (0 )
# 4 : dist_col  distance colunm number in dataset
# 5 : offset to ajdust max value of data from out-layer for better data visulization
# 6 : c.Ajust to adjust cell size if the  data area is available, otherwise set to zero (0)
# 7 : num_images, Number of images to visulize


#uPolar(df, 1,2,3,4,0,7,391)


