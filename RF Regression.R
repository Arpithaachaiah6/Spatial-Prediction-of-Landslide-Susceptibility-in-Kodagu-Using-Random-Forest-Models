getwd()
setwd("D:/RF_REGRESSION")
.libPaths("D:/RF/Library")
install.packages("raster", dependencies = TRUE)
install.packages("caret", dependencies = TRUE)
install.packages("pROC", dependencies = TRUE)
install.packages("terra", dependencies = TRUE)
#install.packages("tidyverse")
library(raster)
library(caret)
library(pROC)
library(terra)
#library(tidyverse)

tb<-read.csv("Data.csv", header= TRUE)
summary(tb)
head(tb)
tail(tb)

#training and testing division
trainIndex=createDataPartition(tb$Y, p = .75, list = FALSE, times = 1)
training = tb [trainIndex,]
testing = tb[-trainIndex,]

Y=as.factor(tb$Y)
head(Y)


#------------------ RANDOM SEARCH MTRY-------------
#Random search#####
#Caret can provide for you random parameter if you do not declare for them. 
train_controlR <- trainControl(method='repeatedcv', 
                               number=10, 
                               repeats=3,
                               search = 'random')    

set.seed(1)
mod_fit_random <- train(Y~.,data = training, trControl=train_controlR, method="rf", importance=TRUE, metric = 'RMSE')
system.time({mod_fit_random <- train(Y~.,data = training, trControl=train_controlR, method="rf", importance=TRUE, metric = 'RMSE')})

print(mod_fit_random )
plot(mod_fit_random )
mod_fit_random$finalModel        
mod_fit_random$results 

p1_random=predict(mod_fit_random, newdata=testing)

roc.glmModel = pROC::roc(testing[,"Y"],p1_random)  
auc_random=pROC::auc(roc.glmModel)
auc_random 
plot(roc.glmModel)  

text(0.5,0.5, paste("AUC =", format(auc_random, digits = 1, scientific = FALSE))) 
#-----------------Final AUC------

print(auc_random)



#--------- predict on raster layer---------
datafiles=Sys.glob("*.tif")
datafiles

stck = stack()
for(i in 1:NROW(datafiles))
{
  tempraster = raster(datafiles[i])
  stck = stack(stck, tempraster)
}
names(stck)[names(stck) == "Band_1"] <- "Elevation"

plot(stck$Elevation)
plot(stck$Slope)
plot(stck$Aspect)
plot(stck$SPI)
plot(stck$TWI)

#---------- test model------------
p2=predict(stck, mod_fit_random)


#---------- Tiff image-----------
plot(p2,main="RF Predictive Map")

writeRaster(p2,filename="Classification_Map_Reg_random.tif", format="GTiff", overwrite=TRUE)

plot(p2, main="RF_LSI")


#------------ Pearson correlation coefficient---------------
# Calculate Pearson correlation coefficient for model with random search
cor_random <- cor(p1_random, testing$Y, method = "pearson")
print("Pearson correlation coefficient with random search:")
print(cor_random)

