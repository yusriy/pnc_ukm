
pnc <- read.csv('data/PNC_UKMKL_Dr. Bari.csv', skip = 1, header = TRUE)

date <- paste(pnc$Date, pnc$Time)
date <- as.POSIXct(date, format = '%m/%d/%y %H:%M:%S', tz = 'Asia/Kuala_Lumpur')
pnc <- pnc[,-c(1,2)]
pnc <- cbind(date, pnc)
rm(date)

plot(pnc$date, pnc$X0.337um, type = 'l')
plot(pnc$date, pnc$X0.4195um, type = 'l')
plot(pnc$date, pnc$X0.522um, type = 'l')


matrix_pnc <- as.matrix(pnc[,-1])
cor_pnc <- cor(matrix_pnc)

pca_pnc <- prcomp(matrix_pnc, center = TRUE, scale. = TRUE)
print(pca_pnc)
plot(pca_pnc, type = 'l')
