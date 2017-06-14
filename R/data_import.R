###############################################
# TITLE: PARTICLE NUMBER COUNT IN KL ANALYSIS
# AUTHOR: YUSRI YUSUP, PHD, USM
# DATE: 2017-06-14
# DATASET: MD FIROZ KHAN, UKM
#
###############################################

#### LOAD NEEDED PACKAGES ####
library(openair)

#### DATA IMPORT ####
pnc <- read.csv('data/PNC_UKMKL_Dr. Bari.csv', skip = 1, header = TRUE)
date <- paste(pnc$Date, pnc$Time)
date <- as.POSIXct(date, format = '%m/%d/%y %H:%M:%S', tz = 'Asia/Kuala_Lumpur')
pnc <- pnc[,-c(1,2)]
pnc <- cbind(date, pnc)
rm(date)

#### AVERAGING ####
# Average daily and hourly
pnc_hour <- timeAverage(pnc, avg.time = 'hour')
pnc_day <- timeAverage(pnc,avg.time = 'day')



#### MULTIVARIATE ANALYSIS ####
### K-MEANS CLUSTERING
# K-means clustering is an unsupervised learning algorithm
#set.seed(1) # To ensure reproducibility
#pnc_cluster <- kmeans(pnc[,2:17], 16, nstart = 10)
#pnc_cluster

# Determine number of clusters
#pnc_no <- pnc[,2:6]
#wss <- (nrow(pnc_no)-1)*sum(apply(pnc_no,2,var))
#for (i in 2:15) wss[i] <- sum(kmeans(pnc_no, 
#                                     centers=i)$withinss)
#plot(1:15, wss, type="b", xlab="Number of Clusters",
#     ylab="Within groups sum of squares")

### PCA 
matrix_pnc <- as.matrix(pnc[,-1])
cor_pnc <- cor(matrix_pnc)
pca_pnc <- prcomp(matrix_pnc, center = TRUE, scale. = TRUE)
print(pca_pnc)
summary(pca_pnc)
plot(pca_pnc, type = 'l')


#### PLOTS: POLAR PLOT ####

#### PLOTS: TIME SERIES ####
# Plot of hourly data 0.337 um
jpeg('figs/337um.jpeg', width = 16, height = 8, units = 'cm', res = 300)
par(mar = c(4.1,4.1,1.1,0.1))
plot(pnc_hour$date,pnc_hour$X0.337um, type = 'l', xlab = 'Date', 
     ylab = expression(
       paste('Particle number count /',' cm'^'3')),
     main = expression(paste('0.337 ',mu, 'm')))
points(pnc_day$date,pnc_day$X0.337um, pch = 19, col = 'black')
dev.off()

# Plot of hourly data 0.4195 um
jpeg('figs/4195um.jpeg', width = 16, height = 8, units = 'cm', res = 300)
par(mar = c(4.1,4.1,1.1,0.1))
plot(pnc_hour$date,pnc_hour$X0.4195um, type = 'l', xlab = 'Date', 
     ylab = expression(
       paste('Particle number count /',' cm'^'3')),
     main = expression(paste('0.4195 ',mu, 'm')))
points(pnc_day$date,pnc_day$X0.4195um, pch = 19, col = 'black')
dev.off()

# Plot of hourly data 0.522 um
jpeg('figs/522um.jpeg', width = 16, height = 8, units = 'cm', res = 300)
par(mar = c(4.1,4.1,1.1,0.1))
plot(pnc_hour$date,pnc_hour$X0.522um, type = 'l', xlab = 'Date', 
     ylab = expression(
       paste('Particle number count /',' cm'^'3')),
     main = expression(paste('0.522 ',mu, 'm')))
points(pnc_day$date,pnc_day$X0.522um, pch = 19, col = 'black')
dev.off()

# Plot of hourly data 0.65 um
jpeg('figs/65um.jpeg', width = 16, height = 8, units = 'cm', res = 300)
par(mar = c(4.1,4.1,1.1,0.1))
plot(pnc_hour$date,pnc_hour$X0.65um, type = 'l', xlab = 'Date', 
     ylab = expression(
       paste('Particle number count /',' cm'^'3')),
     main = expression(paste('0.65 ',mu, 'm')))
points(pnc_day$date,pnc_day$X0.65um, pch = 19, col = 'black')
dev.off()

# Plot of hourly data 0.809 um
jpeg('figs/809um.jpeg', width = 16, height = 8, units = 'cm', res = 300)
par(mar = c(4.1,4.1,1.1,0.1))
plot(pnc_hour$date,pnc_hour$X0.809um, type = 'l', xlab = 'Date', 
     ylab = expression(
       paste('Particle number count /',' cm'^'3')),
     main = expression(paste('0.809 ',mu, 'm')))
points(pnc_day$date,pnc_day$X0.809um, pch = 19, col = 'black')
dev.off()

# Plot of hourly data 1.007 um
jpeg('figs/1007um.jpeg', width = 16, height = 8, units = 'cm', res = 300)
par(mar = c(4.1,4.1,1.1,0.1))
plot(pnc_hour$date,pnc_hour$X1.007um, type = 'l', xlab = 'Date', 
     ylab = expression(
       paste('Particle number count /',' cm'^'3')),
     main = expression(paste('1.007 ',mu, 'm')))
points(pnc_day$date,pnc_day$X1.007um, pch = 19, col = 'black')
dev.off()

# Plot of hourly data 1.254 um
jpeg('figs/1254um.jpeg', width = 16, height = 8, units = 'cm', res = 300)
par(mar = c(4.1,4.1,1.1,0.1))
plot(pnc_hour$date,pnc_hour$X1.254um, type = 'l', xlab = 'Date', 
     ylab = expression(
       paste('Particle number count /',' cm'^'3')),
     main = expression(paste('1.254 ',mu, 'm')))
points(pnc_day$date,pnc_day$X1.254um, pch = 19, col = 'black')
dev.off()

# Plot of hourly data 1.5615 um
jpeg('figs/15615um.jpeg', width = 16, height = 8, units = 'cm', res = 300)
par(mar = c(4.1,4.1,1.1,0.1))
plot(pnc_hour$date,pnc_hour$X1.5615um, type = 'l', xlab = 'Date', 
     ylab = expression(
       paste('Particle number count /',' cm'^'3')),
     main = expression(paste('1.5615 ',mu, 'm')))
points(pnc_day$date,pnc_day$X1.5615um, pch = 19, col = 'black')
dev.off()

# Plot of hourly data 1.944 um
jpeg('figs/1944um.jpeg', width = 16, height = 8, units = 'cm', res = 300)
par(mar = c(4.1,4.1,1.1,0.1))
plot(pnc_hour$date,pnc_hour$X1.944um, type = 'l', xlab = 'Date', 
     ylab = expression(
       paste('Particle number count /',' cm'^'3')),
     main = expression(paste('1.944 ',mu, 'm')))
points(pnc_day$date,pnc_day$X1.944um, pch = 19, col = 'black')
dev.off()

# Plot of hourly data 2.4205 um
jpeg('figs/24205um.jpeg', width = 16, height = 8, units = 'cm', res = 300)
par(mar = c(4.1,4.1,1.1,0.1))
plot(pnc_hour$date,pnc_hour$X2.4205um, type = 'l', xlab = 'Date', 
     ylab = expression(
       paste('Particle number count /',' cm'^'3')),
     main = expression(paste('2.4205 ',mu, 'm')))
points(pnc_day$date,pnc_day$X2.4205um, pch = 19, col = 'black')
dev.off()

# Plot of hourly data 3.014 um
jpeg('figs/3014um.jpeg', width = 16, height = 8, units = 'cm', res = 300)
par(mar = c(4.1,4.1,1.1,0.1))
plot(pnc_hour$date,pnc_hour$X3.014um, type = 'l', xlab = 'Date', 
     ylab = expression(
       paste('Particle number count /',' cm'^'3')),
     main = expression(paste('3.014 ',mu, 'm')))
points(pnc_day$date,pnc_day$X3.014um, pch = 19, col = 'black')
dev.off()

# Plot of hourly data 3.7525 um
jpeg('figs/37525um.jpeg', width = 16, height = 8, units = 'cm', res = 300)
par(mar = c(4.1,4.1,1.1,0.1))
plot(pnc_hour$date,pnc_hour$X3.7525um, type = 'l', xlab = 'Date', 
     ylab = expression(
       paste('Particle number count /',' cm'^'3')),
     main = expression(paste('3.7525 ',mu, 'm')))
points(pnc_day$date,pnc_day$X3.7525um, pch = 19, col = 'black')
dev.off()

# Plot of hourly data 4.672 um
jpeg('figs/4672um.jpeg', width = 16, height = 8, units = 'cm', res = 300)
par(mar = c(4.1,4.1,1.1,0.1))
plot(pnc_hour$date,pnc_hour$X4.672um, type = 'l', xlab = 'Date', 
     ylab = expression(
       paste('Particle number count /',' cm'^'3')),
     main = expression(paste('4.672 ',mu, 'm')))
points(pnc_day$date,pnc_day$X4.672um, pch = 19, col = 'black')
dev.off()

# Plot of hourly data 5.8165 um
jpeg('figs/58165um.jpeg', width = 16, height = 8, units = 'cm', res = 300)
par(mar = c(4.1,4.1,1.1,0.1))
plot(pnc_hour$date,pnc_hour$X5.8165um,type = 'l', xlab = 'Date', 
     ylab = expression(
       paste('Particle number count /',' cm'^'3')),
     main = expression(paste('5.8165 ',mu, 'm')))
points(pnc_day$date,pnc_day$X5.8165um, pch = 19, col = 'black')
dev.off()

# Plot of hourly data 7.241 um
jpeg('figs/7241um.jpeg', width = 16, height = 8, units = 'cm', res = 300)
par(mar = c(4.1,4.1,1.1,0.1))
plot(pnc_hour$date,pnc_hour$X7.241um, type = 'l', xlab = 'Date', 
     ylab = expression(
       paste('Particle number count /',' cm'^'3')),
     main = expression(paste('7.241 ',mu, 'm')))
points(pnc_day$date,pnc_day$X7.241um, pch = 19, col = 'black')
dev.off()

# Plot of hourly data 9.0155 um
jpeg('figs/90155um.jpeg', width = 16, height = 8, units = 'cm', res = 300)
par(mar = c(4.1,4.1,1.1,0.1))
plot(pnc_hour$date,pnc_hour$X9.0155um, type = 'l', xlab = 'Date', 
     ylab = expression(
       paste('Particle number count /',' cm'^'3')),
     main = expression(paste('9.0155 ',mu, 'm')))
points(pnc_day$date,pnc_day$X9.0155um, pch = 19, col = 'black')
dev.off()


#### PLOTS: DIURNAL TRENDS ####
# Diurnal trend plots
x0337um <- timeVariation(pnc_hour,pollutant = c('X0.337um'), cols = 'black', 
                         xlab = c('Hour','Hour','Month','Weekday'),
                         ylab = expression(paste('Particle number count /',' cm'^'3')),
                         name.pol = c(expression(paste('0.337 ',mu, 'm'))))
jpeg('figs/day_0337um.jpeg', width = 16, height = 8, units = 'cm', res = 300)
#par(mar = c(3.1,3.1,1.1,0.01))
plot(x0337um, subset = 'day.hour')
dev.off()

# Diurnal trend plots
x04195um <- timeVariation(pnc_hour,pollutant = c('X0.4195um'), cols = 'black', 
                         xlab = c('Hour','Hour','Month','Weekday'),
                         ylab = expression(paste('Particle number count /',' cm'^'3')),
                         name.pol = c(expression(paste('0.4195 ',mu, 'm'))))
jpeg('figs/day_04195um.jpeg', width = 16, height = 8, units = 'cm', res = 300)
#par(mar = c(4.1,4.1,1.1,0.1))
plot(x04195um, subset = 'day.hour')
dev.off()

# Diurnal trend plots
x0522um <- timeVariation(pnc_hour,pollutant = c('X0.522um'), cols = 'black', 
                          xlab = c('Hour','Hour','Month','Weekday'),
                          ylab = expression(paste('Particle number count /',' cm'^'3')),
                          name.pol = c(expression(paste('0.522 ',mu, 'm'))))
jpeg('figs/day_0522um.jpeg', width = 16, height = 8, units = 'cm', res = 300)
#par(mar = c(4.1,4.1,1.1,0.1))
plot(x0522um, subset = 'day.hour')
dev.off()

# Diurnal trend plots
x065um <- timeVariation(pnc_hour,pollutant = c('X0.65um'), cols = 'black', 
                         xlab = c('Hour','Hour','Month','Weekday'),
                         ylab = expression(paste('Particle number count /',' cm'^'3')),
                         name.pol = c(expression(paste('0.65 ',mu, 'm'))))
jpeg('figs/day_065um.jpeg', width = 16, height = 8, units = 'cm', res = 300)
#par(mar = c(4.1,4.1,1.1,0.1))
plot(x065um, subset = 'day.hour')
dev.off()

# Diurnal trend plots
x0809um <- timeVariation(pnc_hour,pollutant = c('X0.809um'), cols = 'black', 
                        xlab = c('Hour','Hour','Month','Weekday'),
                        ylab = expression(paste('Particle number count /',' cm'^'3')),
                        name.pol = c(expression(paste('0.809 ',mu, 'm'))))
jpeg('figs/day_0809um.jpeg', width = 16, height = 8, units = 'cm', res = 300)
#par(mar = c(4.1,4.1,1.1,0.1))
plot(x0809um, subset = 'day.hour')
dev.off()

# Diurnal trend plots
x1007um <- timeVariation(pnc_hour,pollutant = c('X1.007um'), cols = 'black', 
                        xlab = c('Hour','Hour','Month','Weekday'),
                        ylab = expression(paste('Particle number count /',' cm'^'3')),
                        name.pol = c(expression(paste('1.007 ',mu, 'm'))))
jpeg('figs/day_1007um.jpeg', width = 16, height = 8, units = 'cm', res = 300)
#par(mar = c(4.1,4.1,1.1,0.1))
plot(x1007um, subset = 'day.hour')
dev.off()

# Diurnal trend plots
x1254um <- timeVariation(pnc_hour,pollutant = c('X1.254um'), cols = 'black', 
                        xlab = c('Hour','Hour','Month','Weekday'),
                        ylab = expression(paste('Particle number count /',' cm'^'3')),
                        name.pol = c(expression(paste('1.254 ',mu, 'm'))))
jpeg('figs/day_1254um.jpeg', width = 16, height = 8, units = 'cm', res = 300)
#par(mar = c(4.1,4.1,1.1,0.1))
plot(x1254um, subset = 'day.hour')
dev.off()

# Diurnal trend plots
x15615um <- timeVariation(pnc_hour,pollutant = c('X1.5615um'), cols = 'black', 
                        xlab = c('Hour','Hour','Month','Weekday'),
                        ylab = expression(paste('Particle number count /',' cm'^'3')),
                        name.pol = c(expression(paste('1.5615 ',mu, 'm'))))
jpeg('figs/day_15615um.jpeg', width = 16, height = 8, units = 'cm', res = 300)
#par(mar = c(4.1,4.1,1.1,0.1))
plot(x15615um, subset = 'day.hour')
dev.off()

# Diurnal trend plots
x3014um <- timeVariation(pnc_hour,pollutant = c('X3.014um'), cols = 'black', 
                        xlab = c('Hour','Hour','Month','Weekday'),
                        ylab = expression(paste('Particle number count /',' cm'^'3')),
                        name.pol = c(expression(paste('3.014 ',mu, 'm'))))
jpeg('figs/day_3014um.jpeg', width = 16, height = 8, units = 'cm', res = 300)
#par(mar = c(4.1,4.1,1.1,0.1))
plot(x3014um, subset = 'day.hour')
dev.off()

# Diurnal trend plots
x37525um <- timeVariation(pnc_hour,pollutant = c('X3.7525um'), cols = 'black', 
                        xlab = c('Hour','Hour','Month','Weekday'),
                        ylab = expression(paste('Particle number count /',' cm'^'3')),
                        name.pol = c(expression(paste('3.7525 ',mu, 'm'))))
jpeg('figs/day_3725um.jpeg', width = 16, height = 8, units = 'cm', res = 300)
#par(mar = c(4.1,4.1,1.1,0.1))
plot(x37525um, subset = 'day.hour')
dev.off()

# Diurnal trend plots
x4672um <- timeVariation(pnc_hour,pollutant = c('X4.672um'), cols = 'black', 
                        xlab = c('Hour','Hour','Month','Weekday'),
                        ylab = expression(paste('Particle number count /',' cm'^'3')),
                        name.pol = c(expression(paste('4.672 ',mu, 'm'))))
jpeg('figs/day_4672um.jpeg', width = 16, height = 8, units = 'cm', res = 300)
#par(mar = c(4.1,4.1,1.1,0.1))
plot(x4672um, subset = 'day.hour')
dev.off()

# Diurnal trend plots
x58165um <- timeVariation(pnc_hour,pollutant = c('X5.8165um'), cols = 'black', 
                        xlab = c('Hour','Hour','Month','Weekday'),
                        ylab = expression(paste('Particle number count /',' cm'^'3')),
                        name.pol = c(expression(paste('5.8165 ',mu, 'm'))))
jpeg('figs/day_58165um.jpeg', width = 16, height = 8, units = 'cm', res = 300)
#par(mar = c(4.1,4.1,1.1,0.1))
plot(x58165um, subset = 'day.hour')
dev.off()

# Diurnal trend plots
x7241um <- timeVariation(pnc_hour,pollutant = c('X7.241um'), cols = 'black', 
                        xlab = c('Hour','Hour','Month','Weekday'),
                        ylab = expression(paste('Particle number count /',' cm'^'3')),
                        name.pol = c(expression(paste('7.241 ',mu, 'm'))))
jpeg('figs/day_7241um.jpeg', width = 16, height = 8, units = 'cm', res = 300)
#par(mar = c(4.1,4.1,1.1,0.1))
plot(x7241um, subset = 'day.hour')
dev.off()

# Diurnal trend plots
x90155um <- timeVariation(pnc_hour,pollutant = c('X9.0155um'), cols = 'black', 
                        xlab = c('Hour','Hour','Month','Weekday'),
                        ylab = expression(paste('Particle number count /',' cm'^'3')),
                        name.pol = c(expression(paste('9.0155 ',mu, 'm'))))
jpeg('figs/day_90155um.jpeg', width = 16, height = 8, units = 'cm', res = 300)
#par(mar = c(4.1,4.1,1.1,0.1))
plot(x90155um, subset = 'day.hour')
dev.off()
