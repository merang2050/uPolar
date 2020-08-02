# uPolar



## Microfluidics Images :  
 
![ScreenShot](https://github.com/merang/uPolar/blob/master/microfluidics.png)


##  uPolar Visulization for Trap 35  :  

![ScreenShot](https://github.com/merang/uPolar/blob/master/BC8_Tp_35.png)


##  uPolar Visulization for Trap 1  :  


![ScreenShot](https://github.com/merang/uPolar/blob/master/BC8_Tp_1.png)



##  uPolar Visulization for Trap 60  :  

![ScreenShot](https://github.com/merang/uPolar/blob/master/BC8_Tp_60.png)




##  uPolar Visulization with RLS for Trap 60  :  

![ScreenShot](https://github.com/merang/uPolar/blob/master/BC8_Tp_10.png)




##  uPolar Visulization for migrating mouse fibroblasts  

### 37 time-lapse mouse cells microscopic images with color code cell tracking inlcuding cells size:  

![ScreenShot](https://github.com/merang/uPolar/blob/master/im37_mouse.png)




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


df <- read.csv("../data/BC8_Tp10.csv")
  
uPolar(df, 1,2,NA,NA,NA,FALSE,NA) 



1 :  1st column of dataset for "time" 

2 :  2nd column of dataset for "distance"  

NA :  unavailable data e,g area or RLS

FALSE/TRUE : cell tracking

