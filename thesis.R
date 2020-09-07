#------------------------------------------------------------------------------

# Empty the memory
remove(list=ls())

# Clear Console
cat("\f")

#-------------------------------------------------------------------------
# Import the data
#-------------------------------------------------------------------------

dir      <- ""
dirResults <- ""

dsCalendar_before<- read.csv(paste0(dir, "calendar.csv"), stringsAsFactors = FALSE)
dsListings_before<- read.csv(paste0(dir, "listings.csv"), stringsAsFactors = FALSE)

# Check contents

head(dsCalendar_before)
head(dsListings_before)

#-------------------------------------------------------------------------
# Change column names for various columns.
#-------------------------------------------------------------------------

colnames(dsCalendar_before)
colnames(dsListings_before)

names(dsCalendar_before)[1]<-paste("ID")
names(dsCalendar_before)[2]<-paste("Date")
names(dsCalendar_before)[3]<-paste("Available")
names(dsCalendar_before)[4]<-paste("Price")

names(dsListings_before)[1]<-paste("ID")
names(dsListings_before)[29]<-paste("Host.is.Superhost")
names(dsListings_before)[37]<-paste("Host.is.Verified")
names(dsListings_before)[39]<-paste("Neighborhood")
names(dsListings_before)[49]<-paste("Latitude")
names(dsListings_before)[50]<-paste("Longitude")
names(dsListings_before)[52]<-paste("Property.Type")
names(dsListings_before)[54]<-paste("Capacity")
names(dsListings_before)[83]<-paste("Number.of.Reviews")
names(dsListings_before)[87]<-paste("Review.Scores.Ratings")

#-------------------------------------------------------------------------
# Select columns and create data frames
#-------------------------------------------------------------------------

dsCalendar <- dsCalendar_before[c(1:4)]

dsListings <- dsListings_before[c(1,29,37,39,49,50,52,54,83,87)]


#-------------------------------------------------------------------------
# Check missing values
#-------------------------------------------------------------------------

sum(is.na(dsCalendar))

sum(is.na(dsListings))
# Overview valid and missing cases
sum(complete.cases(dsListings))

dsListings <- na.omit(dsListings)

#-------------------------------------------------------------------------
# Log Listings
#-------------------------------------------------------------------------

dsListings$Capacity <- log(dsListings$Capacity, base = exp(1))
dsListings$Capacity <- round(dsListings$Capacity,3)

dsListings$Number.of.Reviews <- log(dsListings$Number.of.Reviews, base = exp(1))
dsListings$Number.of.Reviews <-round(dsListings$Number.of.Reviews,3)

dsListings$Review.Scores.Ratings <- log(max(dsListings$Review.Scores.Ratings) - dsListings$Review.Scores.Ratings +1, base = exp(1))
dsListings$Review.Scores.Ratings <- max(dsListings$Review.Scores.Ratings) - dsListings$Review.Scores.Ratings
dsListings$Review.Scores.Ratings <- round(dsListings$Review.Scores.Ratings,3)


#-------------------------------------------------------------------------
# Convert "Price" variable to numeric
#-------------------------------------------------------------------------

dsCalendar$Price<- substring(dsCalendar$Price, 2)
library(stringr)
dsCalendar$Price<- str_sub(dsCalendar$Price, end=-4)

dsCalendar$Price <- as.numeric(as.factor(dsCalendar$Price))

#-------------------------------------------------------------------------
# Convert "Date" variable to date
#-------------------------------------------------------------------------

dsCalendar$Date <- as.Date(dsCalendar$Date)

#-------------------------------------------------------------------------
# Subset for off peak months (Oct,Nov,Dec) and find standar deviation per ID
#-------------------------------------------------------------------------

library(lubridate)

dsCalendar <- subset(dsCalendar, (month(dsCalendar$Date) == 10 | month(dsCalendar$Date) == 11 | month(dsCalendar$Date) == 12) & dsCalendar$Available == "t")
#dsCalendar <- cbind(ID = dsCalendar$ID, Price = dsCalendar$Price)

library(data.table)
setDT(dsCalendar)[, sd(Price), by = ID]
dsCalendar1<- aggregate(Price ~ ID, dsCalendar, sd)
names(dsCalendar1)[2]<-paste("Price.StDev")
dsCalendar2<- aggregate(Price ~ ID, dsCalendar, mean)
names(dsCalendar2)[2]<-paste("Price.Mean")

library(dplyr)
dsCalendar <- left_join(dsCalendar1,dsCalendar2, by = 'ID')
dsCalendar$Price.Dispersion <- dsCalendar$Price.StDev/dsCalendar$Price.Mean


# log
dsCalendar$log.Price.Dispersion <- log(dsCalendar$Price.Dispersion+1, base = exp(1))

dsCalendar <- round(dsCalendar,3)

dsCalendar <- na.omit(dsCalendar)

#-------------------------------------------------------------------------
# Summary tables
#-------------------------------------------------------------------------
library(stargazer)
#stargazer(dsCalendar, type = "html",out=paste0(dirResults,"Descriptive Summary Calendar.doc"))
#stargazer(dsListings, type = "html",out=paste0(dirResults,"Descriptive Summary Listings.doc"))

#-------------------------------------------------------------------------
# Merge data sets using left join
#-------------------------------------------------------------------------

dsAirbnb <- left_join(dsCalendar,dsListings, by = 'ID')

#-------------------------------------------------------------------------
# Check missing values on new data set
#-------------------------------------------------------------------------

sum(is.na(dsAirbnb))
sum(complete.cases(dsAirbnb))
dsAirbnb <- na.omit(dsAirbnb)

# remove rows with blank spaces - especially for Host.is.Superhost and Host.is.Verified
dsAirbnb <- dsAirbnb[!(dsAirbnb$Host.is.Superhost == ""), ]
#-------------------------------------------------------------------------
# Generalise Property type
#-------------------------------------------------------------------------

round(table(dsAirbnb$Property.Type)/nrow(dsAirbnb)*100,1)

dsAirbnb$Property.Type[!(dsAirbnb$Property.Type == "Condominium"|dsAirbnb$Property.Type == "Apartment" |dsAirbnb$Property.Type == "House"|dsAirbnb$Property.Type == "Loft")] <- "Other"

#-------------------------------------------------------------------------
# Write the results to csv
#-------------------------------------------------------------------------

summary (dsAirbnb)

write.csv(dsAirbnb, paste0(dir, "Airbnb_Final_Data_stdev.csv"), row.names = FALSE)

dsAirbnb<- read.csv(paste0(dir, "Airbnb_Final_Data_stdev.csv"), stringsAsFactors = FALSE)

#-------------------------------------------------------------------------
# Table with summary statistics
#-------------------------------------------------------------------------

summary(dsAirbnb)

library("stargazer")

stargazer(dsAirbnb, title="Summary statistics Airbnb data")

#-------------------------------------------------------------------------
# Graphical analysis
#-------------------------------------------------------------------------

library(ggplot2)

library(dplyr)


# Qualitative variables: Property type | Host is Superhost | Host is Verified

library(ggpubr)
theme_set(theme_pubr())

df <- dsAirbnb %>%
  group_by(Property.Type) %>%
  summarise(Counts = n())

ggplot(data=df, aes(x=Property.Type, y=Counts, fill=Property.Type)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=Counts), vjust=-0.3, size=3.5)+
  theme_minimal() + 
  theme(legend.position="bottom") + 
  scale_fill_brewer(palette="Dark2") + 
  guides(fill=FALSE) +
  labs(x ="Property Type", y = "Count")

#ggsave(paste0(dirResults, "BarchartProperty.Type.png"))

df <- dsAirbnb %>%
  group_by(Host.is.Verified) %>%
  summarise(Counts = n())

ggplot(data=df, aes(x=Host.is.Verified, y=Counts, fill=Host.is.Verified)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=Counts), vjust=-0.3, size=3.5)+
  theme_minimal() + 
  scale_x_discrete(labels=c("False", "True"))  + 
  scale_fill_brewer(palette="Dark2") + 
  guides(fill=FALSE) +
  labs(x ="Host is Verified", y = "Count")


#ggsave(paste0(dirResults, "BarchartVerified.Type.png"))

df <- dsAirbnb %>%
  group_by(Host.is.Superhost) %>%
  summarise(Counts = n())

ggplot(data=df, aes(x=Host.is.Superhost, y=Counts, fill=Host.is.Superhost)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=Counts), vjust=-0.3, size=3.5)+
  theme_minimal() + 
  scale_x_discrete(labels=c("False", "True"))  + 
  theme(legend.position="bottom") + 
  scale_fill_brewer(palette="Dark2") + 
  guides(fill=FALSE)+
  labs(x ="Host is Superhost", y = "Count")

#ggsave(paste0(dirResults, "BarchartSuperhost.Type.png"))

# Quantitative variables: Price | Capacity | Review Scores Ratings | Number of Reviews

# Histograms

ggplot(dsAirbnb, aes(x = log.Price.Dispersion)) +
  geom_histogram(bins = 20, fill = "lightblue", col = "darkblue") +
  xlab("Price Dispersion") + scale_x_continuous(breaks = seq(0, 260, by = 0.5)) + 
  geom_vline(aes(xintercept=mean(log.Price.Dispersion)),color="blue", linetype="dashed", size=1)

ggplot(dsAirbnb, aes(x = Price.Dispersion)) +
  geom_histogram(bins = 26, fill = "lightblue", col = "darkblue") +
  xlab("Price Dispersion") + scale_x_continuous(breaks = seq(0, 26, by = 1)) +
  geom_vline(aes(xintercept=mean(Price.Dispersion)),color="blue", linetype="dashed", size=1)
#ggsave(paste0(dirResults, "HistPriceNorm.StDev.png"))

ggplot(dsAirbnb, aes(x = Capacity)) +
  geom_histogram(bins = 26, fill = "lightblue", col = "darkblue") +
  xlab("Capacity of property") + scale_x_continuous(breaks = seq(0, 26, by = 1)) +
  geom_vline(aes(xintercept=mean(Capacity)),color="blue", linetype="dashed", size=1)

#ggsave(paste0(dirResults, "HistCapacity.png"))

ggplot(dsAirbnb, aes(x = Number.of.Reviews)) +
  geom_histogram(bins = 10, fill = "lightblue", col = "darkblue") +
  xlab("Number of Reviews") + 
  geom_vline(aes(xintercept=mean(Number.of.Reviews)),color="blue", linetype="dashed", size=1)

#ggsave(paste0(dirResults, "HistNumber.Reviews.png"))

ggplot(dsAirbnb, aes(x = Review.Scores.Ratings)) +
  geom_histogram(bins = 10, fill = "lightblue", col = "darkblue") +
  xlab("Review Scores Ratings") +
  geom_vline(aes(xintercept=mean(Review.Scores.Ratings)),color="blue", linetype="dashed", size=1)

#ggsave(paste0(dirResults, "HistRatings.png"))

# Scatter Plots

dsOutliers <- dsAirbnb[dsAirbnb$Capacity>18, c("Capacity","Norm.StDev")]

ggplot(dsAirbnb, aes(x = Capacity, y = Norm.StDev)) +
  geom_point(size = 1) +
  geom_hline(yintercept = mean(dsAirbnb$Norm.StDev), lty=2, col = "red") +
  geom_vline(xintercept = mean(dsAirbnb$Capacity),   lty=2, col = "red") +
  geom_point(aes(x = mean(dsAirbnb$Capacity), y = mean(dsAirbnb$Norm.StDev)), 
             colour = "red", size = 5) +
  geom_smooth(method = "lm", col = "black", lty = 2, 
              lwd = 1) +
  scale_color_brewer("School", palette = "Set1", 
                     labels=c("Public", "Private")) +
  geom_point(aes(x=Capacity, y=Norm.StDev), dsOutliers,
             shape = 1, stroke = 1.5, size = 6, 
             col = "chocolate3") +
  xlab("Capacity") +
  ylab("Price Standard Deviation")
#ggsave(paste0(dirResults, "ScatterplotCapacity.png"))

dsOutliers <- dsAirbnb[dsAirbnb$Number.of.Reviews>400, c("Number.of.Reviews","Norm.StDev")]

ggplot(dsAirbnb, aes(x = Number.of.Reviews, y = Norm.StDev)) +
  geom_point(size = 1) +
  geom_hline(yintercept = mean(dsAirbnb$Norm.StDev), lty=2, col = "red") +
  geom_vline(xintercept = mean(dsAirbnb$Number.of.Reviews),   lty=2, col = "red") +
  geom_point(aes(x = mean(dsAirbnb$Number.of.Reviews), y = mean(dsAirbnb$Norm.StDev)), 
             colour = "red", size = 5) +
  geom_smooth(method = "lm", col = "black", lty = 2, 
              lwd = 1) +
  scale_color_brewer("School", palette = "Set1", 
                     labels=c("Public", "Private")) +
  geom_point(aes(x=Number.of.Reviews, y=Norm.StDev), dsOutliers,
             shape = 1, stroke = 1.5, size = 6, 
             col = "chocolate3") +
  xlab("Number of Reviews") +
  ylab("Price Standard Deviation")
#ggsave(paste0(dirResults, "ScatterplotNumber.Reviews.png"))

dsOutliers <- dsAirbnb[dsAirbnb$Review.Scores.Ratings<59, c("Review.Scores.Ratings","Norm.StDev")]

ggplot(dsAirbnb, aes(x = Review.Scores.Ratings, y = Norm.StDev)) +
  geom_point(size = 1) +
  geom_hline(yintercept = mean(dsAirbnb$Norm.StDev), lty=2, col = "red") +
  geom_vline(xintercept = mean(dsAirbnb$Review.Scores.Ratings),   lty=2, col = "red") +
  geom_point(aes(x = mean(dsAirbnb$Review.Scores.Ratings), y = mean(dsAirbnb$Norm.StDev)), 
             colour = "red", size = 5) +
  geom_smooth(method = "lm", col = "black", lty = 2, 
              lwd = 1) +
  scale_color_brewer("School", palette = "Set1", 
                     labels=c("Public", "Private")) +
  geom_point(aes(x=Review.Scores.Ratings, y=Norm.StDev), dsOutliers,
             shape = 1, stroke = 1.5, size = 6, 
             col = "chocolate3") +
  xlab("Review Scores Ratings") +
  ylab("Price Standard Deviatopm")
#ggsave(paste0(dirResults, "ScatterplotRatings.png"))


#-------------------------------------------------------------------------
# Correlation analysis
#-------------------------------------------------------------------------
library(Hmisc)
sub <- c("Price.Dispersion","log.Price.Dispersion","Capacity", "Number.of.Reviews","Review.Scores.Ratings")
rcorr(as.matrix(dsAirbnb[sub]), type="pearson")
library(xtable)
library(corrplot)

#mcor <-round(cor(dsAirbnb[sub]),3)
#upper<- mcor
#upper[upper.tri(mcor)]<- ""
#upper<-as.data.frame(upper)
#upper
#stargazer(mcor)
#stargazer(upper, title="Correlation Analysis", type = "latex", out=paste0(dirResults,"Correltion Analysis.tex"))

corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
} 

corstars(dsAirbnb[sub], result="latex")

#-------------------------------------------------------------------------
# K-nearest neighbors k=5
#-------------------------------------------------------------------------

library(spdep)

nc.coords <- cbind(dsAirbnb$Longitude,dsAirbnb$Latitude)

nc.5nn <- knearneigh(nc.coords, k=5, longlat=TRUE)

nc.10nn <- knearneigh(nc.coords, k=10, longlat=TRUE)

nc.dist <-dnearneigh(nc.coords, 0, 0.5,longlat=TRUE)

nc.dist2 <-dnearneigh(nc.coords, 0, 0.7,longlat=TRUE)

#library("fossil")
#summary(earth.dist(nc.coords, dist = TRUE))

#test <-earth.dist(nc.coords, dist = TRUE)

#distM <- as.matrix( fossil::earth.dist(nc.coords))
#summary(unlist(lapply(1:nrow(distM), function(x) mean(distM[x, order(distM[x, ])[2:11]]))))

#summary(nc.dist, nc.coords)

# convert knn to neighbor objects
nc.5nn.nb <- knn2nb(nc.5nn)
nc.10nn.nb <- knn2nb(nc.10nn)

#plot(nc.5nn.nb,nc.coords, col="red")

# convert neighbor object to matrix
nc.dist.mat<- nb2mat(nc.dist2)

write.csv(nc.distn.mat,paste0(dirResults, "nc.dist6.mat.csv"), row.names = FALSE)

#-------------------------------------------------------------------------
# Load shape file
#-------------------------------------------------------------------------

library(rgdal)
spat.data <- readOGR(dsn = "", layer="shapenorm")
names(spat.data)
summary(spat.data)

names(spat.data)[2]<-paste("Price.StDev")
names(spat.data)[3]<-paste("Price.Mean")
names(spat.data)[4]<-paste("Price.Dispersion")
names(spat.data)[5]<-paste("Log.Price.Dispersion")
names(spat.data)[6]<-paste("Host.is.Superhost")
names(spat.data)[7]<-paste("Host.is.Verified")
names(spat.data)[11]<-paste("Property.Type")
names(spat.data)[12]<-paste("Log.Capacity")
names(spat.data)[13]<-paste("Log.Number.of.Reviews")
names(spat.data)[14]<-paste("Log.Review.Scores.Ratings")

spat.data$Log.Capacity <- as.numeric(levels(spat.data$Log.Capacity))[spat.data$Log.Capacity]
spat.data$Log.Number.of.Reviews <- as.numeric(levels(spat.data$Log.Number.of.Reviews))[spat.data$Log.Number.of.Reviews]
spat.data$Log.Review.Scores.Ratings <- as.numeric(levels(spat.data$Log.Review.Scores.Ratings))[spat.data$Log.Review.Scores.Ratings]


spplot(spat.data, "Price.StDev", main="Price Standard Deviation")
#dev.off()

#-------------------------------------------------------------------------
# Lists
#-------------------------------------------------------------------------

# convert nb to list type
listw1 <-  nb2listw(nc.5nn.nb)

listw1b <-  nb2listw(nc.10nn.nb)
#listw.dist <- nb2listw(nc.dist, style="W")

listw2 <- nb2listw(nc.dist, style="W", zero.policy=TRUE)
listw2b <- nb2listw(nc.dist2, style="W", zero.policy=TRUE)
#-------------------------------------------------------------------------
# Regression
#-------------------------------------------------------------------------

# library(rgeos)

spat.data$Host.is.Superhost <- factor(spat.data$Host.is.Superhost)
spat.data$Host.is.Verified <- factor(spat.data$Host.is.Verified)
spat.data$Property.Type <- factor(spat.data$Property.Type)
levels(spat.data$Property.Type)
spat.data$Property.Type <- relevel(spat.data$Property.Type, ref = 4)

# Define regression equation
reg.eq.a = Price.Dispersion ~ Host.is.Superhost + Host.is.Verified + Property.Type + Log.Capacity + Log.Number.of.Reviews + Log.Review.Scores.Ratings
reg.eq.b = Log.Price.Dispersion ~ Host.is.Superhost + Host.is.Verified + Property.Type + Log.Capacity + Log.Number.of.Reviews + Log.Review.Scores.Ratings

#multicollinearity
library(car)
vif(reg1)
#Tolerance
1/vif(reg1)

# OLS
reg1a = lm(reg.eq.a, data=spat.data)
reg1b = lm(reg.eq.b, data=spat.data)
summary(reg1a)
summary(reg1b)

stargazer(reg1a, reg1b,
          align = TRUE, df = TRUE, intercept.bottom = FALSE, 
          no.space = TRUE,
          out = paste0(dirResults, "RegressionOLS.tex"))

# Moran's correlation test for the residuals
lm.morantest(reg1a,listw1)
lm.morantest(reg1b,listw1)
lm.morantest(reg1a,listw2, zero.policy=TRUE) 
lm.morantest(reg1b,listw2, zero.policy=TRUE) 

lm.morantest(reg1a,listw1b)
lm.morantest(reg1b,listw1b)
lm.morantest(reg1a,listw2b, zero.policy=TRUE)
lm.morantest(reg1b,listw2b, zero.policy=TRUE)

# Lagrange multiplier test 

lm.LMtests(reg1a,listw1, test="all") 
lm.LMtests(reg1a,listw1b, test="all")
lm.LMtests(reg1a,listw2, test="all")
lm.LMtests(reg1a,listw2b, test="all")

lm.LMtests(reg1b,listw1, test="all") 
lm.LMtests(reg1b,listw1b, test="all")
lm.LMtests(reg1b,listw2, test="all")
lm.LMtests(reg1b,listw2b, test="all")

# SLX Spatially Lagged X

library(spatialreg, warn.conflicts = FALSE)

reg2.knn = lmSLX(reg.eq, data=spat.data, listw1)
reg2.knn = lmSLX(reg.eq, data=spat.data, listw1b)
reg2.dist = lmSLX(reg.eq, data=spat.data, listw2, zero.policy=TRUE)
reg2.dist = lmSLX(reg.eq, data=spat.data, listw2b, zero.policy=TRUE)

summary(reg2)

stargazer(reg2, 
          align = TRUE, df = TRUE, intercept.bottom = FALSE, 
          no.space = TRUE,
          out = paste0(dirResults, "SLX.tex"))

stargazer(reg2, 
          align = TRUE, df = TRUE, intercept.bottom = FALSE, 
          no.space = TRUE,
          type = "html",
          out = paste0(dirResults, "SLX.doc"))


impacts(reg2, listw=listw1)
summary(impacts(reg2, listw=listw1, R=500,zstats= TRUE))

#SAR Spatial Lag Y Model 

reg3.knna = spatialreg::lagsarlm(reg.eq.a, data=spat.data, listw1)
reg3.knnb = spatialreg::lagsarlm(reg.eq.b, data=spat.data, listw1)

reg3.10knna = spatialreg::lagsarlm(reg.eq.a, data=spat.data, listw1b)
reg3.10knnb = spatialreg::lagsarlm(reg.eq.b, data=spat.data, listw1b)

reg3.dista = spatialreg::lagsarlm(reg.eq.a, data=spat.data, listw2, zero.policy=TRUE)
reg3.distb = spatialreg::lagsarlm(reg.eq.b, data=spat.data, listw2, zero.policy=TRUE)

reg3.7dista = spatialreg::lagsarlm(reg.eq.a, data=spat.data, listw2b, zero.policy=TRUE)
reg3.7distb = spatialreg::lagsarlm(reg.eq.b, data=spat.data, listw2b, zero.policy=TRUE)

summary(reg3.knna)
summary(reg3.10knna)
summary(reg3.dista)
summary(reg3.7dista)

summary(reg3.knnb)
summary(reg3.10knnb)
summary(reg3.distb)
summary(reg3.7distb)

# Calculate direct and indirect impacts for 5nn
W1 <- as(listw1, "CsparseMatrix")
trMatc1 <- trW(W1, type="mult")
trMC1 <- trW(W1, type="MC")
imp1 <- impacts(reg3.knn, tr=trMC1, R=100) 
sums1 <- summary(imp1, zstats=T)

data.frame(sums1$res)

# Calculate direct and indirect impacts for 10nn
W1b <- as(listw1b, "CsparseMatrix")
trMatc1b <- trW(W1b, type="mult")
trMC1b <- trW(W1b, type="MC")
imp1b <- impacts(reg3.10knn, tr=trMC1b, R=100) 
sums1b <- summary(imp1b, zstats=T)

round(data.frame(sums1$res, sums1b$res),3)

# Calculate direct and indirect impacts for 0.5 distance
W2 <- as(listw2, "CsparseMatrix")
trMatc2 <- trW(W2, type="mult")
trMC2 <- trW(W2, type="MC")
imp2 <- impacts(reg3.dista, tr=trMC2, R=100) 
sums2 <- summary(imp2, zstats=T)

# Calculate direct and indirect impacts for 0.7 distance
W2b <- as(listw2b, "CsparseMatrix")
trMatc2b <- trW(W2b, type="mult")
trMC2b <- trW(W2b, type="MC")
imp2b <- impacts(reg3.7dista, tr=trMC2b, R=100) 
sums2b <- summary(imp2b, zstats=T)

round(data.frame(sums2$res, sums2b$res),3)

# --------------------------------------------------------------- Log Price

# Calculate direct and indirect impacts for 5nn
W1 <- as(listw1, "CsparseMatrix")
trMatc1 <- trW(W1, type="mult")
trMC1 <- trW(W1, type="MC")
imp1 <- impacts(reg3.knn, tr=trMC1, R=100) 
sums1 <- summary(imp1, zstats=T)

data.frame(sums1$res)

# Calculate direct and indirect impacts for 10nn
W1b <- as(listw1b, "CsparseMatrix")
trMatc1b <- trW(W1b, type="mult")
trMC1b <- trW(W1b, type="MC")
imp1b <- impacts(reg3.10knn, tr=trMC1b, R=100) 
sums1b <- summary(imp1b, zstats=T)

round(data.frame(sums1$res, sums1b$res),3)

# Calculate direct and indirect impacts for 0.5 distance
W2 <- as(listw2, "CsparseMatrix")
trMatc2 <- trW(W2, type="mult")
trMC2 <- trW(W2, type="MC")
imp2 <- impacts(reg3.distb, tr=trMC2, R=100) 
sums2 <- summary(imp2, zstats=T)

# Calculate direct and indirect impacts for 0.7 distance
W2b <- as(listw2b, "CsparseMatrix")
trMatc2b <- trW(W2b, type="mult")
trMC2b <- trW(W2b, type="MC")
imp2b <- impacts(reg3.7distb, tr=trMC2b, R=100) 
sums2b <- summary(imp2b, zstats=T)

round(data.frame(sums2$res, sums2b$res),3)

spatialreg::impacts(reg3,listw=listw1)
summary(impacts(reg3.knn,listw=listw1,R=500),zstats=TRUE) #Add zstats,pvals

SEM Spatial Error Model  y=XB+u,   u=LWu+e
reg4.knn=errorsarlm(reg.eq,data=spat.data, listw1)
reg4.dist=errorsarlm(reg.eq,data=spat.data, listw2, zero.policy=TRUE)
summary(reg4)

#Spatial Hausman Test

#Hausman.test(reg4)

#SDM Spatial Durbin Model
reg6.knn=lagsarlm(reg.eq.a, data=spat.data,listw1, type="mixed")
reg6.10knn=lagsarlm(reg.eq.a, data=spat.data,listw1b, type="mixed")
reg6.dist=lagsarlm(reg.eq.a, data=spat.data,listw2, type="mixed", zero.policy=TRUE)
reg6.7dist=lagsarlm(reg.eq.a, data=spat.data,listw2b, type="mixed", zero.policy=TRUE)

reg6.knn=lagsarlm(reg.eq.b, data=spat.data,listw1, type="mixed")
reg6.10knn=lagsarlm(reg.eq.b, data=spat.data,listw1b, type="mixed")
reg6.dist=lagsarlm(reg.eq.b, data=spat.data,listw2, type="mixed", zero.policy=TRUE)
reg6.7dist=lagsarlm(reg.eq.b, data=spat.data,listw2b, type="mixed", zero.policy=TRUE)


summary(reg6.knn)
summary(reg6.10knn)
summary(reg6)
summary(reg6.7dist)

# Calculate direct and indirect impacts for knn
imp3 <- impacts(reg6.knn, tr=trMC1, R=100) 
sums3 <- summary(imp3, zstats=T)

round(data.frame(sums3$res),3)

# Calculate direct and indirect impacts for knn
imp3b <- impacts(reg6.10knn, tr=trMC1b, R=100) 
sums3b <- summary(imp3b, zstats=T)

round(data.frame(sums3$res,sums3b$res),3)

# Calculate direct and indirect impacts for distance
imp4 <- impacts(reg6.dist, tr=trMC2, R=100) 
sums4 <- summary(imp4, zstats=T)

round(data.frame(sums4$res),3)

# Calculate direct and indirect impacts for distance
imp4b <- impacts(reg6.7dist, tr=trMC2b, R=100) 
sums4b <- summary(imp4b, zstats=T)

round(data.frame(sums4$res, sums4b$res),3)

#Manski All-inclusive Model
reg7=sacsarlm(reg.eq,data=spat.data, listw1, type="sacmixed") 

#SARAR
reg8=sacsarlm(reg.eq,data=spat.data,listw1, type="sac")

# Summaries
s=summary

s(reg1)#OLS
s(reg2)#SLX
s(reg3)#Lag Y (SAR)
s(reg4)#Lag Error (SEM)
s(reg6)#Durbin (SDM)

#LR Tests: Test Model Restrictions Global model:
LR.sarlm(reg6.knn, reg3.knn)
LR.sarlm(reg6.10knn, reg3.10knn) 
LR.sarlm(reg6, reg3.dist)
LR.sarlm(reg6.7dist,  reg3.7dist)

#Pseudo R^2
1-(reg6$SSE/(var(spat.data$Price.Norm.StDev)*(length(spat.data$Price.Norm.StDev)-1)))

1-(reg3.knn$SSE/(var(spat.data$Price.Norm.StDev)*(length(spat.data$Price.Norm.StDev)-1)))
1-(reg3.10knn$SSE/(var(spat.data$Price.Norm.StDev)*(length(spat.data$Price.Norm.StDev)-1)))

1-(reg3.dist$SSE/(var(spat.data$Price.Norm.StDev)*(length(spat.data$Price.Norm.StDev)-1)))
1-(reg3.7dist$SSE/(var(spat.data$Price.Norm.StDev)*(length(spat.data$Price.Norm.StDev)-1)))

1-(reg6.knn$SSE/(var(spat.data$Price.Norm.StDev)*(length(spat.data$Price.Norm.StDev)-1)))
1-(reg6.10knn$SSE/(var(spat.data$Price.Norm.StDev)*(length(spat.data$Price.Norm.StDev)-1)))

1-(reg6$SSE/(var(spat.data$Price.Norm.StDev)*(length(spat.data$Price.Norm.StDev)-1)))
1-(reg6.7dist$SSE/(var(spat.data$Price.Norm.StDev)*(length(spat.data$Price.Norm.StDev)-1)))

# Breusch-Pagan Test for Heteroskedasticity
bptest.sarlm(reg5,studentize=TRUE)
# heteroskedasticity affects standard errors and p-values -> i shouldn't be worried though if p-value is very small
bptest.sarlm(reg6.7dist,studentize=TRUE)