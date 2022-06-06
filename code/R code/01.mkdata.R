# LOAD THE PACKAGE
library(dlnm) ; library(splines) ; library(xtable)

# CHECK VERSION OF THE PACKAGE
  if(packageVersion("dlnm")<"2.2.0")
    stop("update dlnm package to version >= 2.2.0")

# LOAD THE DATA
data <- read.csv("~/School/KU Leuven/Modern Data Analytics/mda_project/code/out/meta_analysis_data.csv",row.names=1)
# Excluding region La Louvière, as the mortality data was unavailable
data <- data[data$Nom_arrondissement != "La Louvière",]
  
####################################################################
# REGIONS

regions <- as.character(unique(data$Nom_arrondissement))

####################################################################
# LIST OF DATAFRAMES FOR OUR REGIONS

datalist <- lapply(regions, function(region) data[data$Nom_arrondissement==region,])
names(datalist) <- regions

####################################################################
# CITY-LEVEL META-PREDICTORS

lat <- c(50.9407, 51.2213, 49.6855, 50.6340, 50.8459, 50.0006, 51.2092, 50.8260,
         50.4096, 50.8268, 51.0255, 51.0317, 50.2607, 51.1852, 51.0500, 50.8135,
         50.9326, 50.5187, 50.8823, 50.6330, 51.0963, 50.2297, 51.0259, 50.4547,
         50.7431, 50.4649, 49.8417, 50.5992, 51.2247, 50.1960, 50.9499, 51.1653,
         50.5793, 50.3406, 51.0000, 50.7800, 50.6055, 51.3217, 50.5911, 51.0732,
         49.5673, 50.6981, 50.8492)
perclat <- round(quantile(lat,c(1,3)*0.25),1)

####################################################################
# ADDITIONAL INFO

m <- length(datalist)

# MOVING AVERAGE OF TMEAN OVER LAG 0-6
for(i in seq(datalist)) datalist[[i]]$tmean05 <- 
  filter(datalist[[i]]$tmean,rep(1,6)/6,side=1)

# TEMPERATURE RANGES (FOR LAG 0-5)
ranges <- t(sapply(datalist,function(x) range(x$tmean05,na.rm=T)))

# COMPUTE 25TH-75TH PERCENTILES OF META-VARIABLES

# DEFINE THE AVERAGE RANGE, CENTERING POINT, DEGREE AND TYPE OF THE SPLINE
# (THESE PARAMETERS CAN BE CHANGED BY THE USER FOR ADDITIONAL ANALYSES)
cen <- 17
bound <- colMeans(ranges)
degree <- 2
type <- "bs"
df <- 6

# DEFINE THE KNOTS AT TEMPERATURE CORRESPONDING TO AVERAGE PERCENTILES
knotperc <- c(5,35,65,95)
knots <- rowMeans(sapply(datalist,function(x) 
  quantile(x$tmean05,knotperc/100,na.rm=T)))

# SAVE
#save.image("data.RData")
	
#
