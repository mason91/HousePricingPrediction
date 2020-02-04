library(readr)
library(mice)
library(data.table)

dt <- read.csv('./train.csv')
#dt <- data.table(df)
#rm(df)

colnames(dt) <- c("Id","MSSubClass","MSZoning","LotFrontage","LotArea","Street","Alley","LotShape","LandContour","Utilities","LotConfig","LandSlope","Neighborhood","Condition1","Condition2","BldgType","HouseStyle","OverallQual","OverallCond","YearBuilt","YearRemodAdd","RoofStyle","RoofMatl","Exterior1st","Exterior2nd","MasVnrType","MasVnrArea","ExterQual","ExterCond","Foundation","BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinSF1","BsmtFinType2","BsmtFinSF2","BsmtUnfSF","TotalBsmtSF","Heating","HeatingQC","CentralAir","Electrical","FirstFlrSF","SecFlrSF","LowQualFinSF","GrLivArea","BsmtFullBath","BsmtHalfBath","FullBath","HalfBath","BedroomAbvGr","KitchenAbvGr","KitchenQual","TotRmsAbvGrd","Functional","Fireplaces","FireplaceQu","GarageType","GarageYrBlt","GarageFinish","GarageCars","GarageArea","GarageQual","GarageCond","PavedDrive","WoodDeckSF","OpenPorchSF","EnclosedPorch","ThreeSsnPorch","ScreenPorch","PoolArea","PoolQC","Fence","MiscFeature","MiscVal","MoSold","YrSold","SaleType","SaleCondition","SalePrice")

dt$PoolQC <- as.character(dt$PoolQC)
dt$PoolQC[is.na(dt$PoolQC)] <- "NA" 
dt$PoolQC <- factor(dt$PoolQC)
dt$Fence <- as.character(dt$Fence)
dt$Fence[is.na(dt$Fence)] <- "NA"
dt$Fence <- factor(dt$Fence)
dt$MiscFeature <- as.character(dt$MiscFeature)
dt$MiscFeature[is.na(dt$MiscFeature)] <- "NA"
dt$MiscFeature <- factor(dt$MiscFeature)
dt$FireplaceQu <- as.character(dt$FireplaceQu)
dt$FireplaceQu[is.na(dt$FireplaceQu)] <- "NA"
dt$FireplaceQu <- factor(dt$FireplaceQu)
dt$Alley <- as.character(dt$Alley)
dt$Alley[is.na(dt$Alley)] <- "NA"
dt$Alley <- factor(dt$Alley)
dt$BsmtQual <- as.character(dt$BsmtQual)
dt$BsmtQual[is.na(dt$BsmtQual) & dt$TotalBsmtSF == 0] <- "NA"
dt$BsmtQual <- factor(dt$BsmtQual)
dt$BsmtCond <- as.character(dt$BsmtCond)
dt$BsmtCond[is.na(dt$BsmtCond) & dt$TotalBsmtSF == 0] <- "NA"
dt$BsmtCond <- factor(dt$BsmtCond)
dt$BsmtExposure <- as.character(dt$BsmtExposure)
dt$BsmtExposure[is.na(dt$BsmtExposure) & dt$TotalBsmtSF == 0] <- "NA"
dt$BsmtExposure <- factor(dt$BsmtExposure)
dt$BsmtFinType1 <- as.character(dt$BsmtFinType1)
dt$BsmtFinType1[is.na(dt$BsmtFinType1) & dt$TotalBsmtSF == 0] <- "NA"
dt$BsmtFinType1 <- factor(dt$BsmtFinType1)
dt$BsmtFinType2 <- as.character(dt$BsmtFinType2)
dt$BsmtFinType2[is.na(dt$BsmtFinType2) & dt$TotalBsmtSF == 0] <- "NA"
dt$BsmtFinType2 <- factor(dt$BsmtFinType2)
dt$FireplaceQu[is.na(dt$FireplaceQu) & dt$Fireplaces == 0] <- "NA"
dt$GarageType <- as.character(dt$GarageType)
dt$GarageType[is.na(dt$GarageType) & dt$GarageArea == 0] <- "NA"
dt$GarageType <- factor(dt$GarageType)
dt$GarageFinish <- as.character(dt$GarageFinish)
dt$GarageFinish[is.na(dt$GarageFinish) & dt$GarageArea == 0] <- "NA"
dt$GarageFinish <- factor(dt$GarageFinish)
dt$GarageQual <- as.character(dt$GarageQual)
dt$GarageQual[is.na(dt$GarageQual) & dt$GarageArea == 0] <- "NA"
dt$GarageQual <- factor(dt$GarageQual)
dt$GarageCond <- as.character(dt$GarageCond)
dt$GarageCond[is.na(dt$GarageCond) & dt$GarageArea == 0] <- "NA"
dt$GarageCond <- factor(dt$GarageCond)

dt$MSSubClass <- as.factor(as.character(dt$MSSubClass))
#dt$OverallCond <- factor(dt$OverallCond)
#dt$OverallQual <- factor(dt$OverallQual)
dt$OverallCond <- factor(as.character(dt$OverallCond), levels=c("1","2","3","4","5","6","7","8","9","10"), ordered=TRUE)
dt$OverallQual <- factor(as.character(dt$OverallQual), levels=c("1","2","3","4","5","6","7","8","9","10"), ordered=TRUE)
dt$ExterQual <- factor(dt$ExterQual, levels=c("Po","Fa","TA","Gd","Ex"), ordered=TRUE)
dt$ExterCond <- factor(dt$ExterCond, levels=c("Po","Fa","TA","Gd","Ex"), ordered=TRUE)
dt$BsmtQual <- factor(dt$BsmtQual, levels=c("NA","Po","Fa","TA","Gd","Ex"), ordered=TRUE)
dt$BsmtCond <- factor(dt$BsmtCond, levels=c("NA","Po","Fa","TA","Gd","Ex"), ordered=TRUE)
dt$BsmtExposure <- factor(dt$BsmtExposure, levels=c("NA","No","Mn","Av","Gd"), ordered=TRUE)
dt$BsmtFinType1 <- factor(dt$BsmtFinType1, levels=c("NA","Unf","LwQ","Rec","BLQ","ALQ","GLQ"), ordered=TRUE)
dt$BsmtFinType2 <- factor(dt$BsmtFinType2, levels=c("NA","Unf","LwQ","Rec","BLQ","ALQ","GLQ"), ordered=TRUE)
dt$HeatingQC <- factor(dt$HeatingQC, levels=c("Po","Fa","TA","Gd","Ex"), ordered=TRUE)
dt$KitchenQual <- factor(dt$KitchenQual, levels=c("Po","Fa","TA","Gd","Ex"), ordered=TRUE)
dt$Functional <- factor(dt$Functional, levels=c("Sal","Sev","Maj2","Maj1","Mod","Min2","Min1","Typ"), ordered=TRUE)
dt$FireplaceQu <- factor(dt$FireplaceQu, levels=c("NA","Po","Fa","TA","Gd","Ex"), ordered=TRUE)
dt$GarageFinish <- factor(dt$GarageFinish, levels=c("NA","Unf","RFn","Fin"), ordered=TRUE)
dt$GarageQual <- factor(dt$GarageQual, levels=c("NA","Po","Fa","TA","Gd","Ex"), ordered=TRUE)
dt$GarageCond <- factor(dt$GarageCond, levels=c("NA","Po","Fa","TA","Gd","Ex"), ordered=TRUE)
dt$PavedDrive <- factor(dt$PavedDrive, levels=c("N","P","Y"), ordered=TRUE)
dt$PoolQC <- factor(dt$PoolQC, levels=c("NA","Fa","TA","Gd","Ex"), ordered=TRUE)
dt$Fence <- factor(dt$Fence, levels=c("NA","MnWw","GdWo","MnPrv","GdPrv"), ordered=TRUE)



miceMod <- mice(dt[,!names(dt) %in% "SalePrice"],method="rf")
dt.complete <- complete(miceMod)
anyNA(dt.complete)

getNACol <- function(dt, index) {
  if(any(is.na(dt[index]))) 
  {
    return(index)
  }
}
naCol <- sapply(dt.complete, getNACol, index=seq_len(length(dt.complete)))
which(is.null(naCol))
dt.complete <- cbind(dt.complete, dt$SalePrice)
dt.complete <- dt.complete[,!colnames(dt.complete) %in% "Utilities"]
names(dt.complete)[80] <- "SalePrice"

rm(dt)

dt.test <- read.csv("./test.csv")

colnames(dt.test) <- c("Id","MSSubClass","MSZoning","LotFrontage","LotArea","Street","Alley","LotShape","LandContour","Utilities","LotConfig","LandSlope","Neighborhood","Condition1","Condition2","BldgType","HouseStyle","OverallQual","OverallCond","YearBuilt","YearRemodAdd","RoofStyle","RoofMatl","Exterior1st","Exterior2nd","MasVnrType","MasVnrArea","ExterQual","ExterCond","Foundation","BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinSF1","BsmtFinType2","BsmtFinSF2","BsmtUnfSF","TotalBsmtSF","Heating","HeatingQC","CentralAir","Electrical","FirstFlrSF","SecFlrSF","LowQualFinSF","GrLivArea","BsmtFullBath","BsmtHalfBath","FullBath","HalfBath","BedroomAbvGr","KitchenAbvGr","KitchenQual","TotRmsAbvGrd","Functional","Fireplaces","FireplaceQu","GarageType","GarageYrBlt","GarageFinish","GarageCars","GarageArea","GarageQual","GarageCond","PavedDrive","WoodDeckSF","OpenPorchSF","EnclosedPorch","ThreeSsnPorch","ScreenPorch","PoolArea","PoolQC","Fence","MiscFeature","MiscVal","MoSold","YrSold","SaleType","SaleCondition")

dt.test$PoolQC <- as.character(dt.test$PoolQC)
dt.test$PoolQC[is.na(dt.test$PoolQC)] <- "NA"
dt.test$Fence <- as.character(dt.test$Fence)
dt.test$Fence[is.na(dt.test$Fence)] <- "NA"
dt.test$MiscFeature <- as.character(dt.test$MiscFeature)
dt.test$MiscFeature[is.na(dt.test$MiscFeature)] <- "NA"
dt.test$MiscFeature <- factor(dt.test$MiscFeature)
dt.test$FireplaceQu <- as.character(dt.test$FireplaceQu)
dt.test$FireplaceQu[is.na(dt.test$FireplaceQu)] <- "NA"
dt.test$Alley <- as.character(dt.test$Alley)
dt.test$Alley[is.na(dt.test$Alley)] <- "NA"
dt.test$Alley <- factor(dt.test$Alley)
dt.test$BsmtQual <- as.character(dt.test$BsmtQual)
dt.test$BsmtQual[is.na(dt.test$BsmtQual) & dt.test$TotalBsmtSF == 0] <- "NA"
dt.test$BsmtCond <- as.character(dt.test$BsmtCond)
dt.test$BsmtCond[is.na(dt.test$BsmtCond) & dt.test$TotalBsmtSF == 0] <- "NA"
dt.test$BsmtExposure <- as.character(dt.test$BsmtExposure)
dt.test$BsmtExposure[is.na(dt.test$BsmtExposure) & dt.test$TotalBsmtSF == 0] <- "NA"
dt.test$BsmtFinType1 <- as.character(dt.test$BsmtFinType1)
dt.test$BsmtFinType1[is.na(dt.test$BsmtFinType1) & dt.test$TotalBsmtSF == 0] <- "NA"
dt.test$BsmtFinType2 <- as.character(dt.test$BsmtFinType2)
dt.test$BsmtFinType2[is.na(dt.test$BsmtFinType2) & dt.test$TotalBsmtSF == 0] <- "NA"
dt.test$FireplaceQu <- as.character(dt.test$FireplaceQu)
dt.test$FireplaceQu[is.na(dt.test$FireplaceQu) & dt.test$Fireplaces == 0] <- "NA"
dt.test$GarageType <- as.character(dt.test$GarageType)
dt.test$GarageType[is.na(dt.test$GarageType) & dt.test$GarageArea == 0] <- "NA"
dt.test$GarageType <- factor(dt.test$GarageType)
dt.test$GarageFinish <- as.character(dt.test$GarageFinish)
dt.test$GarageFinish[is.na(dt.test$GarageFinish) & dt.test$GarageArea == 0] <- "NA"
dt.test$GarageQual <- as.character(dt.test$GarageQual)
dt.test$GarageQual[is.na(dt.test$GarageQual) & dt.test$GarageArea == 0] <- "NA"
dt.test$GarageCond <- as.character(dt.test$GarageCond)
dt.test$GarageCond[is.na(dt.test$GarageCond) & dt.test$GarageArea == 0] <- "NA"
dt.test$MSSubClass[dt.test.complete$MSSubClass == 150] <- 160

dt.test$MSSubClass <- as.factor(as.character(dt.test$MSSubClass))
dt.test$OverallCond <- factor(as.character(dt.test$OverallCond), levels=c("1","2","3","4","5","6","7","8","9"), ordered=TRUE)
dt.test$OverallQual <- factor(as.character(dt.test$OverallQual), levels=c("1","2","3","4","5","6","7","8","9","10"), ordered=TRUE)
dt.test$ExterQual <- factor(dt.test$ExterQual, levels=c("Fa","TA","Gd","Ex"), ordered=TRUE)
dt.test$ExterCond <- factor(dt.test$ExterCond, levels=c("Po","Fa","TA","Gd","Ex"), ordered=TRUE)
dt.test$BsmtQual <- factor(dt.test$BsmtQual, levels=c("NA","Fa","TA","Gd","Ex"), ordered=TRUE)
dt.test$BsmtCond <- factor(dt.test$BsmtCond, levels=c("NA","Po","Fa","TA","Gd"), ordered=TRUE)
dt.test$BsmtExposure <- factor(dt.test$BsmtExposure, levels=c("NA","No","Mn","Av","Gd"), ordered=TRUE)
dt.test$BsmtFinType1 <- factor(dt.test$BsmtFinType1, levels=c("NA","Unf","LwQ","Rec","BLQ","ALQ","GLQ"), ordered=TRUE)
dt.test$BsmtFinType2 <- factor(dt.test$BsmtFinType2, levels=c("NA","Unf","LwQ","Rec","BLQ","ALQ","GLQ"), ordered=TRUE)
dt.test$HeatingQC <- factor(dt.test$HeatingQC, levels=c("Po","Fa","TA","Gd","Ex"), ordered=TRUE)
dt.test$KitchenQual <- factor(dt.test$KitchenQual, levels=c("Fa","TA","Gd","Ex"), ordered=TRUE)
dt.test$Functional <- factor(dt.test$Functional, levels=c("Sev","Maj2","Maj1","Mod","Min2","Min1","Typ"), ordered=TRUE)
dt.test$FireplaceQu <- factor(dt.test$FireplaceQu, levels=c("NA","Po","Fa","TA","Gd","Ex"), ordered=TRUE)
dt.test$GarageFinish <- factor(dt.test$GarageFinish, levels=c("NA","Unf","RFn","Fin"), ordered=TRUE)
dt.test$GarageQual <- factor(dt.test$GarageQual, levels=c("NA","Po","Fa","TA","Gd"), ordered=TRUE)
dt.test$GarageCond <- factor(dt.test$GarageCond, levels=c("NA","Po","Fa","TA","Gd","Ex"), ordered=TRUE)
dt.test$PavedDrive <- factor(dt.test$PavedDrive, levels=c("N","P","Y"), ordered=TRUE)
dt.test$PoolQC <- factor(dt.test$PoolQC, levels=c("NA","Gd","Ex"), ordered=TRUE)
dt.test$Fence <- factor(dt.test$Fence, levels=c("NA","MnWw","GdWo","MnPrv","GdPrv"), ordered=TRUE)

miceMod.test <- mice(dt.test[,],method="rf")
dt.test.complete <- complete(miceMod.test)
anyNA(dt.test.complete)

dt.test.complete$OverallCond <- factor(dt.test.complete$OverallCond, levels=c("1","2","3","4","5","6","7","8","9","10"), ordered=TRUE)
dt.test.complete$ExterQual <- factor(dt.test.complete$ExterQual, levels=c("Po","Fa","TA","Gd","Ex"), ordered=TRUE)
dt.test.complete$BsmtQual <- factor(dt.test.complete$BsmtQual, levels=c("NA","Po","Fa","TA","Gd","Ex"), ordered=TRUE)
dt.test.complete$KitchenQual <- factor(dt.test.complete$KitchenQual, levels=c("Po","Fa","TA","Gd","Ex"), ordered=TRUE)
dt.test.complete$Condition2 <- factor(dt.test.complete$Condition2, levels=c("Artery", "Feedr", "Norm", "PosA", "PosN", "RRAe", "RRAn", "RRNn"), ordered = FALSE)
levels(dt.test.complete$RoofMatl) <- list(ClyTile = "ClyTile",CompShg = "CompShg", Membran="Membran", Metal="Metal", Roll="Roll", `Tar&Grv` = "Tar&Grv", WdShake = "WdShake", WdShngl ="WdShngl")
levels(dt.test.complete$HouseStyle) <- list(`1.5Fin` = "1.5Fin",`1.5Unf` = "1.5Unf", `1Story`="1Story", `2.5Fin`="2.5Fin", `2.5Unf`="2.5Unf", `2Story` = "2Story", SFoyer = "SFoyer", SLvl ="SLvl")
list.level <- as.list(levels(dt.complete$Exterior1st))
names(list.level) <- levels(dt.complete$Exterior1st)
levels(dt.test.complete$Exterior1st) <- list.level
list.level <- as.list(levels(dt.complete$Exterior2nd))
names(list.level) <- levels(dt.complete$Exterior2nd)
levels(dt.test.complete$Exterior2nd) <- list.level
list.level <- as.list(levels(dt.complete$BsmtCond))
names(list.level) <- levels(dt.complete$BsmtCond)
levels(dt.test.complete$BsmtCond) <- list.level
list.level <- as.list(levels(dt.complete$Heating))
names(list.level) <- levels(dt.complete$Heating)
levels(dt.test.complete$Heating) <- list.level
list.level <- as.list(levels(dt.complete$Electrical))
names(list.level) <- levels(dt.complete$Electrical)
levels(dt.test.complete$Electrical) <- list.level
list.level <- as.list(levels(dt.complete$Functional))
names(list.level) <- levels(dt.complete$Functional)
levels(dt.test.complete$Functional) <- list.level
list.level <- as.list(levels(dt.complete$GarageQual))
names(list.level) <- levels(dt.complete$GarageQual)
levels(dt.test.complete$GarageQual) <- list.level
list.level <- as.list(levels(dt.complete$PoolQC))
names(list.level) <- levels(dt.complete$PoolQC)
levels(dt.test.complete$PoolQC) <- list.level
list.level <- as.list(levels(dt.complete$MiscFeature))
names(list.level) <- levels(dt.complete$MiscFeature)
levels(dt.test.complete$MiscFeature) <- list.level
dt.test.complete <- dt.test.complete[,!colnames(dt.test.complete) %in% "Utilities"]


naCol <- sapply(dt.test.complete, getNACol, index=seq_len(length(dt.test.complete)))

dt.test.complete$ageOfHouse <- dt.test.complete$YrSold - dt.test.complete$YearBuilt
dt.test.complete$totalSF <- dt.test.complete$GrLivArea + dt.test.complete$TotalBsmtSF + dt.test.complete$GarageArea
dt.test.complete$ageOfRemodel <- dt.test.complete$YrSold - dt.test.complete$YearRemodAdd
dt.test.complete$totalSF <- log(dt.test.complete$totalSF)
dt.test.complete$LotArea <- log(dt.test.complete$LotArea)
#dt.test.complete$GrLivArea <- log(dt.test.complete$GrLivArea)
dt.test.complete$GarageArea_T <- NA
dt.test.complete$GarageArea_T[dt.test.complete$GarageArea > quantile(dt.test.complete$GarageArea, probs = 0.95)] <- "Large"
dt.test.complete$GarageArea_T[dt.test.complete$GarageArea < quantile(dt.test.complete$GarageArea, probs = 0.05)] <- "Small"
dt.test.complete$GarageArea_T[is.na(dt.test.complete$GarageArea_T)] <- "Medium"
dt.test.complete$GarageArea_T <- factor(dt.test.complete$GarageArea_T, levels=c("Small", "Medium", "Large"), ordered=TRUE)
dt.test.complete$hasTwoFloors <- NA
dt.test.complete$hasTwoFloors[dt.test.complete$SecFlrSF > 0] <- 1
dt.test.complete$hasTwoFloors[dt.test.complete$SecFlrSF == 0] <- 0
dt.test.complete$isPosN <- ifelse(dt.test.complete$Condition1 == "PosN" | dt.test.complete$Condition2 == "PosN", 1, 0)
dt.test.complete$totalPorchSF <- dt.test.complete$OpenPorchSF + dt.test.complete$EnclosedPorch + dt.test.complete$ThreeSsnPorch + dt.test.complete$ScreenPorch
