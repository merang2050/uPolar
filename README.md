# uPolar



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

### 37 time-lapse mouse cells microscopic images with color code cell tracking inlcuding cells size:  

![ScreenShot](https://github.com/merang/uPolar/blob/master/microscopic.png)



## Installation : 

### Option 1:  
install.packages("devtools")

devtools::install_github("merang/uPolar/uPolar")

### Option 2:
download zip file and run below command in console : 

install.packages("~/Downloads/uPolar_0.1.0.tar", repos = NULL,type = "source")

note : In install.packages("  file path " , repos = NULL , type =       ) , the "type" needs to be specified to "source"  


## Packages : 

1) library(dplyr) :    install.packages("dplyr")

2) library(plotly):    install.packages("plotly")   (Plotly )

3) library(RDocumentation): install.packages("RDocumentation")  ( open directory )

 #### Option :  Give directory name and path of dataset  

You can also use devtools to install the latest development version:

devtools::install_github("datacamp/RDocumentation")

https://github.com/datacamp/Rdocumentation


## How to run uPolar : 

library(dplyr)

library(plotly)

library(plyr)



df= read.csv("../data/BC8_tp10.csv")

rls.tp10 = read.csv("../data/BC8_RLS_tp10.csv")

uPolar(df$time_num,df$dist,df$area,0.05,34,FALSE,rls.tp10$com)




