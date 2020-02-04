library(partykit)
library(MLmetrics)

dt.complete$ageOfHouse <- as.numeric(dt.complete$YrSold) - as.numeric(dt.complete$YearBuilt) 
dt.complete$totalSF <- dt.complete$GrLivArea + dt.complete$TotalBsmtSF + dt.complete$GarageArea
dt.complete$ageOfRemodel <- dt.complete$YrSold - dt.complete$YearRemodAdd
dt.complete$SalePrice <- log(dt.complete$SalePrice)
dt.complete$totalSF <- log(dt.complete$totalSF)
dt.complete$LotArea <- log(dt.complete$LotArea)
dt.complete$GarageArea_T <- NA
dt.complete$GarageArea_T[dt.complete$GarageArea > quantile(dt.complete$GarageArea, probs = 0.95)] <- "Large"
dt.complete$GarageArea_T[dt.complete$GarageArea < quantile(dt.complete$GarageArea, probs = 0.05)] <- "Small"
dt.complete$GarageArea_T[is.na(dt.complete$GarageArea_T)] <- "Medium"
dt.complete$GarageArea_T <- factor(dt.complete$GarageArea_T, levels=c("Small", "Medium", "Large"), ordered=TRUE)
dt.complete$hasTwoFloors <- NA
dt.complete$hasTwoFloors[dt.complete$SecFlrSF > 0] <- 1
dt.complete$hasTwoFloors[dt.complete$SecFlrSF == 0] <- 0
dt.complete$isPosN <- ifelse(dt.complete$Condition1 == "PosN" | dt.complete$Condition2 == "PosN", 1, 0)
dt.complete$totalPorchSF <- dt.complete$OpenPorchSF + dt.complete$EnclosedPorch + dt.complete$ThreeSsnPorch + dt.complete$ScreenPorch

cfVarImp <- cforest(SalePrice ~ ., data=dt.complete, mtry = 10, ntree = 2001)
var <- varimp(cfVarImp)
var <- var[order(-var)]
View(var)