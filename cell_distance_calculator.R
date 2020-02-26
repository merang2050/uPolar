---
title: "CELL distance calculation"
author: "Mehran Ghafari, Hong Qin"
date: "1/25/2019"
output: html_document
---


rm(list=ls())

###################  Coordinate and  REF point #############

ref_X = 2;
ref_Y = 60;

# Cell coordinate should be labeled as " X " and "Y"


###########################################################


#Load data
df.dist<- read.csv("cell.csv")

#Distance Functions
my_dist = function(x1, y1, x2, y2) {sqrt( (x1 - x2)^2 + (y1-y2)^2 ) };



# Calculate distance from ref-point for each cell in trap
df.dist$dist = 'NA';
for ( i in  1: nrow(df.dist)) {

  current  = df.n[i, ];
  d= my_dist(current$X, current$Y,ref_X,ref_Y);
  df.dist$dist[i] = d

}


#write.csv(df.dist, "df.dist.csv", quote=F)






















