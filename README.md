# uPolar R Visualization Tool 



## Microfluidics Images :  
 
![ScreenShot](https://github.com/merang/uPolar/blob/master/microfluidic.png)


##  uPolar Visulization examples  :  

![ScreenShot](https://github.com/merang/uPolar/blob/master/compare.png)



##  uPolar Visualization for TrapNo.  10   : 

### No 1 to 8 represent  individual event onthe plot 

![ScreenShot](https://github.com/merang/uPolar/blob/master/explain.png)



##  uPolar Visualization with RLS for Trap No. 1   : 

### Red-Stars represent division point

![ScreenShot](https://github.com/merang/uPolar/blob/master/rlsTp1.png)



##  uPolar Visualization for migrating mouse fibroblasts  

### 37 time-lapse mouse cells microscopic images with color tag cell tracking inlcuding cells size:  

![ScreenShot](https://github.com/merang/uPolar/blob/master/microscopic.png)


## Installation : 

install.packages("devtools")

devtools::install.packages(c("dplyr,plotly,plyr"))

devtools::install_github("merang/uPolar/uPolar")


## How to run uPolar : 

library(dplyr)

library(plotly)

library(plyr)

### Load dead
df= read.csv("../data/BC8_tp10_TpTmXYAreaRLS.csv")

### uPolar function 

uPolar(df,refPoint=NULL,title=NULL,refLine=NULL,Area=FALSE,aAdjust=1,zeroAdjust= NULL,track = FALSE,RLS= FALSE)




