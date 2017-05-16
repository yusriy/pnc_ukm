## This script runs Hysplit after downloading met data and looks for the data
## in certain folders specified within the function
## 
## Author: Yusri Yusup
## Date modified: 2017-05-15

# Load packages
library(openair)
library(mapdata) # To use hi-res maps
library(mapproj)

source('R/hysplit_run/procTraj.R')
source('R/hysplit_run/read.files.R')
source('R/hysplit_run/getMet.R')
source('R/hysplit_run/add.met.R')

# Download the data, commented out so that the data is not downloaded again
getMet(year=2016,month=11)

# Run Hysplit, this function changes the working directory.
# Need to change lat, long, name, hours, and height
for(i in 2016){
  procTraj(lat=3.168434,lon=101.701301,year=i,
           name='kl',hours=72,height=500,
           met='/Users/Yusri/Documents/Work/Data_analysis/pnc_ukm/data/TrajData/',
           out='/Users/Yusri/Documents/Work/Data_analysis/pnc_ukm/data/TrajProc/')
}
# Reset working directory
setwd('/Users/Yusri/Documents/Work/Data_analysis/pnc_ukm/')
# Import data
trajKL<-importTraj(site='kl',year=2016,local='data/TrajProc/')


trajLevel(trajKL, projection = 'mercator', orientation = c(90,90,0),
          parameters=NULL,map.res = 'hires',statistic = 'cwt')

# Trajectory plots
trajPlot(selectByDate(subset(trajKL,lat > 0 & lat < 10 & lon > 95 & lon < 105),
                      start='20/6/2012',end='20/6/2012'),
         orientation=c(90,90,0),projection='mercator',
         parameters=NULL,map.res = 'hires')

trajPlot(subset(usmt),orientation = c(90,90,0),
         plot.type='l',projection = 'mercator',
         parameters=NULL,pollutant = 'pm10',map.res='hires',type='monsoon')

trajLevel(s18t,orientation = c(90,90,0),projection = 'mercator',
          parameters=NULL,pollutant = 'Cd',map.res='hires')

trajLevel(trajPenang,statistic='frequency',col='increment',projection='mercator',
          parameters=NULL)

trajLevel(subset(trajPenang,lat > 0 & lat < 10 & lon > 95 & lon < 105),
          col='increment',orientation=c(90,90,0),method='hexbin',statistic='difference',
          projection='mercator',parameters=NULL,map.res='hires',grid.col='black')

trajLevel(s18t,pollutant='pm10',statistic='frequency',orientation = c(90,90,0))

trajLevel(s18t,pollutant='pm10',statistic='difference',orientation=c(90,90,0),
          map.res='hires')

# Trajectory plot with factors
#### S18 ####
# Factor 1: Mn-Fe-Cd
trajPlot(s18t,pollutant = 'f_metal1',
         map.res = 'hires', projection = 'mercator',parameters=NULL,
         grid.col = 'black',lwd=2)
# Factor 2: Pb-Ca
trajPlot(s18t,pollutant = 'f_metal2',
         map.res = 'hires', projection = 'mercator',parameters=NULL,
         grid.col = 'black',lwd=2)
# Factor 3: Ni-Zn
trajPlot(s18t,pollutant = 'f_metal3',
         map.res = 'hires', projection = 'mercator',parameters=NULL,
         grid.col = 'black',lwd=2)
# Factor 4: Mg-Cu
trajPlot(s18t,pollutant = 'f_metal4',
         map.res = 'hires', projection = 'mercator',parameters=NULL,
         grid.col = 'black',lwd=2)

#### USM ####
# Factor 1: Mn-Fe-Cd
trajPlot(usmt,pollutant = 'f_metal1',
         map.res = 'hires', projection = 'mercator',parameters=NULL,
         grid.col = 'black',lwd=2)
# Factor 2: Pb-Ca
trajPlot(usmt,pollutant = 'f_metal2',
         map.res = 'hires', projection = 'mercator',parameters=NULL,
         grid.col = 'black',lwd=2)
# Factor 3: Ni-Zn
trajPlot(usmt,pollutant = 'f_metal3',
         map.res = 'hires', projection = 'mercator',parameters=NULL,
         grid.col = 'black',lwd=2)
# Factor 4: Mg-Cu
trajPlot(usmt,pollutant = 'f_metal4',
         map.res = 'hires', projection = 'mercator',parameters=NULL,
         grid.col = 'black',lwd=2)