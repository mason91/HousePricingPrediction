library(partykit)
library(MLmetrics)
library(readr)

cfModel1 <- cforest(formula = SalePrice ~ totalSF + OverallQual + GrLivArea + Neighborhood + ExterQual
        + GarageCars + KitchenQual + FirstFlrSF + TotalBsmtSF + BsmtQual
        + GarageArea + TotRmsAbvGrd + ageOfHouse + FireplaceQu + FullBath
        + MSSubClass + Fireplaces + BsmtFinSF1 + SaleCondition + OverallCond
        + LotArea + BsmtFullBath + HeatingQC, data=dt.complete, mtry=5, ntree = 2001,
        control=ctree_control(testtype = "Bonferroni"))


cfModel2 <- cforest(formula = SalePrice ~ ., data=dt.complete[,c(names(var[1:57]), "SalePrice")], mtry=9, ntree = 2001,
                    control=ctree_control(testtype = "Bonferroni"))

oob3.cfmodel1 <- predict(cfModel1, OOB = TRUE)
oob.cfmodel2  <- predict(cfModel2, OOB = TRUE)

MAPE(oob.cfmodel1, dt.complete$SalePrice)
MAPE(oob2.cfmodel1, dt.complete$SalePrice)
MAPE(oob3.cfmodel1, dt.complete$SalePrice)
MAPE(oob.cfmodel2, dt.complete$SalePrice)

dt.test.complete <- dt.test.complete[,!names(dt.test.complete) %in% "SalePrice"]
dt.test.complete$SalePrice <- predict(cfModel2, dt.test.complete)
dt.submit <- dt.test.complete[,c("Id","SalePrice")]
write_csv(dt.submit, './submission.csv')
