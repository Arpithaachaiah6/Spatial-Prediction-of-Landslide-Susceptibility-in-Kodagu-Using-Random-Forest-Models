path=readClipboard()
setwd(path)
getwd() # for checking
.libPaths("D:/RF/Library")

install.packages("terra")
install.packages("pROC", dependencies = TRUE, repos = "http://cran.us.r-project.org")
install.packages("dplyr", dependencies = TRUE, repos = "http://cran.us.r-project.org")
install.packages("plyr", dependencies = TRUE, repos = "http://cran.us.r-project.org")
install.packages("utf8", dependencies = TRUE, repos = "http://cran.us.r-project.org")
install.packages("caret", dependencies = TRUE, repos = "http://cran.us.r-project.org")
install.packages("doParallel", dependencies = TRUE, repos = "http://cran.us.r-project.org")
install.packages("e1071", dependencies = TRUE, repos = "http://cran.us.r-project.org")
install.packages("timeDate", dependencies = TRUE, repos = "http://cran.us.r-project.org")
install.packages("labeling")
install.packages("sf")
install.packages("tidyverse")
install.packages("farver")
install.packages("gower")
install.packages("randomForest", dependencies = TRUE, repos = "http://cran.us.r-project.org")
install.packages("lattice", dependencies = TRUE, repos = "http://cran.us.r-project.org")
install.packages("ggplot2", dependencies = TRUE, repos = "http://cran.us.r-project.org")

library(farver)
library(ggplot2)
library(sp)            # spatial data
library(raster)        # raster processing 
library(plyr)         # data manipulation 
library(dplyr)        # data manipulation  
library(RColorBrewer) # color
library(caret)        # machine lreaning
library(doParallel)   # Parallel processing
library(e1071)        # Naive Bayes
library(labeling)
library(gower)
library(randomForest)
library(party)

list.files( pattern = "csv$", full.names = TRUE) # Import training and testing data

data_train <-  read.csv("D:/RF/Excel/TRAIN.csv", header = T)
data_train <-(na.omit(data_train))
summary(data_train)

ASPECTr<-cut(data_train$Aspect, seq(0,361,45), right=FALSE, labels=c("a","b","c","d","e","f","g","h"))
table(ASPECTr) 
class(ASPECTr) # double check if not a factor
ASPECTr <- factor(ASPECTr)   #continuous-to-categorical

flags = data.frame(Reduce(cbind,lapply(levels(ASPECTr),function(x){(ASPECTr == x)*1})
))
names(flags) = levels(ASPECTr)
data_train = cbind(data_train, flags)

data_train <- data_train[,-4]

as.data.frame(table(data_train$Training))

# Normalization
maxs <- apply(data_train[,2:6], 2, max) 
x <- maxs
print(x)

mins <- apply(data_train[,2:6], 2, min)
y <- mins
print(y)

scaled_train <- as.data.frame(scale(data_train[,2:6], center = mins, scale = maxs - mins))
scaled_tr<-as.data.frame(cbind(scaled_train[,c(1:5)],data_train[,c(1,7:14)]))
scaled_tr<-scaled_tr[,c(6,1:5,7:14)] # arrange the column data
scaled_tr$Training <- ifelse(scaled_tr$Training == 1, "yes","no")

summary(scaled_tr) #find out whether NA is present

scaled_t<- na.omit(scaled_tr) # omit NA and save the data in scaled_t
summary(scaled_t)


# 2-2 Testing Data --------------------------------------------------------


data_test <-  read.csv("D:/RF/Excel/TEST.csv", header = T)
data_test <-na.omit(data_test)
data_test <-data.frame(data_test)
str(data_test)
as.data.frame(table(data_test$Testing))


# Fix the categorial factor
ASPECTe<-cut(data_test$Aspect, seq(0,361,45), right=FALSE, labels=c("a","b","c","d","e","f","g","h"))
table(ASPECTe)


# Dealing with Categorial data
ASPECTe <- factor(ASPECTe)
flagse = data.frame(Reduce(cbind, 
                           lapply(levels(ASPECTe), function(x){(ASPECTe == x)*1})
))
names(flagse) = levels(ASPECTe)
data_test = cbind(data_test, flagse) # combine the ASPECTS with original data
data_test <- data_test[,-4] # remove original Aspect


#Normalization
maxs <- apply(data_test[,2:6], 2, max) 
x1 <- maxs
print(x1)

mins <- apply(data_test[,2:6], 2, min)
y1 <- mins
print(y1)


scaled_test <- as.data.frame(scale(data_test[,2:6], center = mins, scale = maxs - mins))
scaled_ts<-as.data.frame(cbind(scaled_test[,c(1:5)],data_test[,c(1,7:14)]))
scaled_ts<-scaled_ts[,c(6,1:5,7:14)] # arrange the column data

summary(scaled_ts) #find out whether NA is present

scaled_tst<- na.omit(scaled_ts) # omit NA and save the data in scaled_tst
summary(scaled_tst)

scaled_tst$Testing <- ifelse(scaled_tst$Testing == 1, "yes","no")

names(scaled_tst)
scaled_tst$Slides=scaled_tst$Testing
names(scaled_t)
scaled_t$Slides=scaled_t$Training

mytree <- Training ~.
slides_mytree <- ctree(mytree , data = data_train)
table(predict(slides_mytree),data_train$Training)
plot(slides_mytree)
jpeg("Elevation", width = 800, height = 500)
plot(slides_mytree,main="Elevation")
dev.off()

All_incidents <- merge(scaled_tst[,-1], scaled_t[,-1], all=TRUE)  
str(All_incidents)
All_incidents <- All_incidents[,c(14,1:13)] # re-order columns
summary(All_incidents)#check NA is present

scaled_tst$Slides= NULL  # remove Slide column
scaled_t$Slides=NULL  # remove Slide column

#Creating seperate dataframe for '"LevelsAve" features which is our target.
number.perfect.splits <- apply(X=All_incidents[,-1], MARGIN = 2, FUN = function(col){
  t <- table(All_incidents$Slides,col)
  sum(t == 0)})

# Descending order of perfect splits
order <- order(number.perfect.splits,decreasing = TRUE)
number.perfect.splits <- number.perfect.splits[order]

# Plot graph
par(mar=c(10,2,2,2))
barplot(number.perfect.splits,main="Number of perfect splits vs Landslide Conditional Factors",xlab="",ylab="Counts",las=3,col="grey") 

#Step 2: Data Visualization

data_train$Training <- ifelse(data_train$Training == 1, "yes","no")
data_test$Testing <- ifelse(data_test$Testing == 0, "no","yes")
summary(data_train)
summary(data_test)
data_train<-(na.omit(data_train)) #no NA data
summary(data_train)

# Create one file contain all data
data_test$Slides=data_test$Testing
data_train$Slides=data_train$Training

All_incidents_orginal <- merge(data_train[,-1], data_test[,-1], all=TRUE) 
str(All_incidents_orginal)
summary(All_incidents_orginal)
#All_incidents_orginal <-(na.omit(All_incidents_orginal))

#Visual Elevation
ggplot(All_incidents_orginal, aes(Elevation, colour = Slides)) +
  geom_freqpoly(binwidth = 1) + labs(title="Elevation Distribution by Landslides occurances")

ggplot(All_incidents_orginal, aes(Slope, colour = Slides)) +
  geom_freqpoly(binwidth = 1) + labs(title="Slope Distribution by Landslides occurances")

ggplot(All_incidents_orginal, aes(Curvature, colour = Slides)) +
  geom_freqpoly(binwidth = 1) + labs(title="Curvature Distribution by Landslides occurances")

ggplot(All_incidents_orginal, aes(SPI, colour = Slides)) +
  geom_freqpoly(binwidth = 1) + labs(title="SPI Distribution by Landslides occurances")

ggplot(All_incidents_orginal, aes(TWI, colour = Slides)) +
  geom_freqpoly(binwidth = 1) + labs(title="TWI Distribution by Landslides occurances")

ggplot(All_incidents_orginal, aes(a, colour = Slides)) +
  geom_freqpoly(binwidth = 1) + labs(title="a Distribution by Landslides occurances")
ggplot(All_incidents_orginal, aes(b, colour = Slides)) +
  geom_freqpoly(binwidth = 1) + labs(title="b Distribution by Landslides occurances")
ggplot(All_incidents_orginal, aes(c, colour = Slides)) +
  geom_freqpoly(binwidth = 1) + labs(title="c Distribution by Landslides occurances")
ggplot(All_incidents_orginal, aes(d, colour = Slides)) +
  geom_freqpoly(binwidth = 1) + labs(title="d Distribution by Landslides occurances")
ggplot(All_incidents_orginal, aes(e, colour = Slides)) +
  geom_freqpoly(binwidth = 1) + labs(title="e Distribution by Landslides occurances")
ggplot(All_incidents_orginal, aes(f, colour = Slides)) +
  geom_freqpoly(binwidth = 1) + labs(title="f Distribution by Landslides occurances")
ggplot(All_incidents_orginal, aes(g, colour = Slides)) +
  geom_freqpoly(binwidth = 1) + labs(title="g Distribution by Landslides occurances")
#gplot(All_incidents_orginal, aes(h, colour = Slides)) +
  #geom_freqpoly(binwidth = 1) + labs(title="h Distribution by Landslides occurances")



######################
#### 2 Modeling ---------------------------------------------------------
######################

# Define the control
trControl <- trainControl(method='repeatedcv', 
                          repeats=3,
                          number = 10,
                          search = "grid")

set.seed(1234)
# Run the model
#Trainclasses<- as.factor(scaled_t[[1]])
#Trainclasses <- factor(Trainclasses)
#class(Trainclasses)


rf_defaultN <- train(Training~.,
                     data=scaled_t,
                     method = "rf",
                     metric = "Accuracy",
                     trControl = trControl ) 
summary(scaled_t)
# Print the results
print(rf_defaultN)     
plot(rf_defaultN)
rf_defaultN$finalModel         # Results mtry=11 Number of trees: 500
rf_defaultN$results 




# Step 2) Search best mtry

set.seed(1234)
tuneGrid <- expand.grid(.mtry = c(1: 22))
rf_mtry <- train(Training~., 
                 data=scaled_t,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 nodesize = 14,
                 ntree = 500)
print(rf_mtry)
rf_mtry$bestTune$mtry

#You can store it and use it when you need to tune the other parameters.
max(rf_mtry$results$Accuracy)
best_mtry <- rf_mtry$bestTune$mtry 
best_mtry


# Step 3) Search the best maxnodes SKIP

store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in c(5: 30)) {
  set.seed(1234)
  rf_maxnode <- train(Training~., 
                      data=scaled_t,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE,
                      nodesize = 14,
                      maxnodes = maxnodes,
                      ntree = 500)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry)



#final model


fit_rf_final <- train(Training~., 
                      data=scaled_t,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE
)

fit_rf_final
print(fit_rf_final)

varImp(fit_rf_final)
plot(varImp(fit_rf_final), main="Final RandomForest tuned model")



# Evaluate the model
p1_final<-predict(fit_rf_final, scaled_tst[,c(-1)], type = "raw")
confusionMatrix(p1_final, as.factor(scaled_tst$Testing))

#Random search#####
#Caret can provide for you random parameter if you do not declare for them. 
control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3,
                        search = 'random')    

#Random generate 15 mtry values with tuneLength = 15
set.seed(1)
rf_random <- train(Training~., 
                   data=scaled_t,
                   method = 'rf',
                   metric = 'Accuracy',
                   trControl = control,
                   importance = TRUE)
print(rf_random)
varImp(rf_random)
plot(varImp(rf_random))
plot(rf_random)

# Evaluate the model
p1_random<-predict(rf_random, scaled_tst[,c(-1)], type = "raw")
cf <- confusionMatrix(p1_random, as.factor(scaled_tst$Testing))  

print(cf)
fourfoldplot(as.table(cf),color=c("yellow","pink"),main="Confusion Matrix")
#  Plot ROC curves

library(pROC)
predictions1 <- as.data.frame(predict(rf_random, scaled_tst, type = "prob")) # the model is used to predict the test data
str(predictions1)
predictions1$predict <- names(predictions1)[1:2][apply(predictions1[,1:2], 1, which.max)]
predictions1$observed <- as.factor(scaled_tst$Testing)
head(predictions1)


# plot the ROC curves. For each class, convert the multi-class problem into a binary problem. Also, 
#    call the roc() function specifying 2 arguments: i) observed classes and ii) class probability (instead of predicted class).

roc.yes <- roc(ifelse(predictions1$observed=="yes","no-yes","yes"), as.numeric(predictions1$yes))
roc.no <- roc(ifelse(predictions1$observed=="no","no-no", "no"), as.numeric(predictions1$no))

plot(roc.no, col = "green", main="RF best tune prediction ROC plot using testing data", xlim=c(0.44,0.1))
lines(roc.yes, col = "red")

# calculating the values of AUC for ROC curve
results= c("Yes AUC" = roc.yes$auc) #,"No AUC" = roc.no$auc)
print(results)
legend("topleft",c("AUC = 0.8453906 "),fill=c("red"),inset = (0.42))

#--------------------------------
All_incidents <-(na.omit(All_incidents))
set.seed(849)
fit.rfAll<- train(Slides~., 
                  data=All_incidents,
                  method = "rf",
                  metric = "Accuracy",
                  trControl = control,
                  importance = TRUE)

X.rfAll = varImp(fit.rfAll)
plot(X.rfAll)

# Plot graph

jpeg("varImportance All RF.jpg", width = 800, height = 500)
plot(X.rfAll,main="varImportanceAll RF" )
dev.off()

# Produce prediction map using Raster data ---------------------------
# Import and process thematic maps ------------------------------------
#Produce LSM map using Training model results and Raster layers data

library(raster)

Elevation = raster("D:/RF/Original Layers/Elevation.Tif")  
Slope= raster("D:/RF/Original Layers/Slope.Tif") 
Curvature= raster("D:/RF/Original Layers/Curvature.Tif")  
Aspect=raster("D:/RF/Original Layers/Aspect.Tif")  
SPI=raster("D:/RF/Original Layers/SPI.Tif")  
TWI= raster("D:/RF/Original Layers/TWI.Tif")  



extent(Elevation)
extent(Slope)
extent(Curvature)
extent(Aspect)
extent(SPI)
extent(TWI)

#  diffrent extent, then try to Resample them using the smallest area
#ELEVATION_r <- resample(Elevation,Slope, resample='bilinear') 
#SLOPE_r <- resample(SLOPE,LANDCOVER, resample='bilinear') 
#TWI_r <- resample(TWI,Slope, resample='bilinear') 
#CURVATURE_r <- resample(Curvature,Slope, resample='bilinear') 
#SPI_r <- resample(SPI,Slope, resample='bilinear') 
#ASPECT_r <- resample(Aspect,Slope, resample='bilinear') 

#extent(ELEVATION_r)
#extent(Slope)
#extent(CURVATURE_r)
#extent(ASPECT_r)
#extent(SPI_r)
#extent(TWI_r)

# write to a new geotiff file

writeRaster(Elevation,filename="D:/RF/resampled/Elevation.tif", format="GTiff", overwrite=TRUE) 
writeRaster(Slope,filename="D:/RF/resampled/Slope.tif", format="GTiff", overwrite=TRUE)
writeRaster(Curvature,filename="D:/RF/resampled/Curvature.tif", format="GTiff", overwrite=TRUE)
writeRaster(Aspect,filename="D:/RF/resampled/Aspect.tif", format="GTiff", overwrite=TRUE)
writeRaster(SPI,filename="D:/RF/resampled/SPI.tif", format="GTiff", overwrite=TRUE)
writeRaster(TWI,filename="D:/RF/resampled/TWI.tif", format="GTiff", overwrite=TRUE)

## stack multiple raster files
Stack_List= list.files(path = "D:/RF/resampled/",pattern = "tif$", full.names = TRUE)
Rasters=stack(Stack_List)

names(Rasters)
#rm(SPI)

#Convert rasters to dataframe with Long-Lat -----------------------

Rasters.df = as.data.frame(Rasters, xy = TRUE, na.rm = TRUE)
head(Rasters.df,1)
Rasters.df <-(na.omit(Rasters.df))



# :Prediction using imported Rasters
Rasters.df_N <- Rasters.df[,c(-1,-2)] # remove x, y
head(Rasters.df_N,1)



# ASPECT
ASPECTras<-cut(Rasters.df_N$Aspect, seq(0,361,45), right=FALSE, labels=c("a","b","c","d","e","f","g","h"))
table(ASPECTras)


ASPECTras <- factor(ASPECTras)
flagsras = data.frame(Reduce(cbind, 
                             lapply(levels(ASPECTras), function(x){(ASPECTras == x)*1})
))
names(flagsras) = levels(ASPECTras)
Rasters.df_N = cbind(Rasters.df_N, flagsras) # combine the ASPECTS with original data

# Remove the original aspect data
Rasters.df_N<- Rasters.df_N[,-1]


str(Rasters.df_N)
Rasters.df_N[is.na(Rasters.df_N)] = 0 #convert null into zero so the observations of raster.df.N matches with raster.df_N_scaled

str(Rasters.df_N)


# Scale the numeric variables --------------------------------------

# Check the relationship between the numeric varaibles, Scale the numeric var first!


maxss <- apply(Rasters.df_N, 2, max) 
minss <- apply(Rasters.df_N, 2, min)
Rasters.df_N_scaled <- as.data.frame(scale(Rasters.df_N, center = minss, scale = maxss - minss)) 
Rasters.df_N_scaled <- na.omit(Rasters.df_N_scaled)
#Rasters.df_N_scaled <- cbind(Rasters.df_N_scaled[,1:5],Rasters.df_N[,6:13])

# PRODUCE PROBABILITY MAP
p3<-as.data.frame(predict(fit.rfAll, Rasters.df_N_scaled, type = "prob"))
summary(p3)


Rasters.df$Levels_yes<-p3$yes #add levels to raster.df
Rasters.df$Levels_no<-p3$no

projection(Elevation)

x<-SpatialPointsDataFrame(as.data.frame(Rasters.df)[, c("x", "y")], data = Rasters.df)
str(x)


r_ave_yes <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_yes")])
proj4string(r_ave_yes)=CRS(projection(Elevation))

r_ave_no <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_no")])
proj4string(r_ave_no)=CRS(projection(Elevation))


# Plot Maps
spplot(r_ave_yes, main="Landslide Prone Areas")
writeRaster(r_ave_yes,filename="D:/RF/Output/Prediction_RF Tunned_Landslides SM.tif", format="GTiff", overwrite=TRUE) 


spplot(r_ave_no, main="Non-Landslide Prone Areas")
writeRaster(r_ave_no,filename="D:/RF/Output/Prediction_RF Tunned_Non Slide.tif", format="GTiff", overwrite=TRUE) 


# PRODUCE CLASSIFICATION MAP
#Prediction at grid location
p3<-as.data.frame(predict(fit.rfAll, Rasters.df_N_scaled, type = "raw"))
summary(p3)
# Extract predicted levels class

Rasters.df$Levels_Slide_No_slide<-p3$`predict(fit.rfAll, Rasters.df_N_scaled, type = "raw")`
head(Rasters.df, n=2)

# Import levels ID file 
ID<-read.csv("D:/RF/Excel/Levels_key.csv", header = T)
head(ID)
# Join landuse ID
grid.new<-join(Rasters.df, ID, by="Levels_Slide_No_slide", type="inner") 
# Omit missing values
#head(grid.new,n=2)
#summary(grid.new)
grid.new.na<-na.omit(grid.new)  
head(grid.new.na, n=2)

#Convert to raster
x<-SpatialPointsDataFrame(as.data.frame(grid.new.na)[, c("x", "y")], data = grid.new.na)
r_ave_Slide_No_slide <- rasterFromXYZ(as.data.frame(x)[, c("x","y", "Level_ID")])
str(r_ave_Slide_No_slide)
# coord. ref. : NA 
# Add coord. ref. system by using the original data info (Copy n Paste).
# borrow the projection from Raster data
proj4string(r_ave_Slide_No_slide)=CRS(projection(Elevation)) # set it to lat-long

# Export final prediction map as raster TIF ---------------------------
# write to a new geotiff file
spplot(r_ave_Slide_No_slide, main="LSM using RF")
writeRaster(r_ave_Slide_No_slide,filename="D:/RF/Output/Classification_Map RF.tif", format="GTiff", overwrite=TRUE) 


#Plot Landuse Map:
# Color Palette follow Air index color style
#https://bookdown.org/rdpeng/exdata/plotting-and-color-in-r.html

myPalette <- colorRampPalette(c("light green","red" ))

# Plot Map
LU_ave<-spplot(r_ave_Slide_No_slide,"Level_ID", main="Landslide prediction: RF tunned", 
               colorkey = list(space="right",tick.number=1,height=1, width=1.5,
                               labels = list(at = seq(1,4.8,length=5),cex=1.0,
                                             at = c("Yes" ,"No"))),
               col.regions=myPalette,cut=5)
LU_ave
jpeg("Prediction_Map RF_Landslide .jpg", width = 1000, height = 700)
LU_ave
dev.off()




