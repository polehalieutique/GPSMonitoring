## ----setup, include=FALSE---------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,out.width="100%")
getwd()
rm (list=ls())
library(sf)
library(adehabitatLT)
library(sqldf)
library(dplyr)
library(ggplot2)
library(ggpubr)

library(RPostgreSQL)
library(tidyr)
library(units)
library(GPSMonitoring)
library(knitr)
library(kableExtra)
library(shiny)
library(leaflet)
library(ranger)
library(randomForest)
library(caret)


