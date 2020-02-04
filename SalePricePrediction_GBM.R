library(readr)
library(xgboost)
library(caTools)
library(Matrix)

trainY <- sample.split(dt.complete$SalePrice, SplitRatio = 0.8)
dt.train <- dt.complete[trainY == TRUE,]
dt.validate <- dt.complete[trainY == FALSE,]

sparseTrain <- sparse.model.matrix(SalePrice ~ -1 + totalSF + OverallQual + Neighborhood 
                                   + KitchenQual + BsmtQual + hasTwoFloors
                                   + ageOfHouse + FullBath + GarageArea
                                   + MSZoning + Fireplaces  + SaleCondition + OverallCond
                                   + LotArea + BsmtFullBath + BsmtUnfSF, data = dt.train)
sparseValidate <- sparse.model.matrix(SalePrice ~ -1 + totalSF + OverallQual + Neighborhood 
                                      + KitchenQual + BsmtQual + hasTwoFloors
                                      + ageOfHouse + FullBath + GarageArea
                                      + MSZoning + Fireplaces  + SaleCondition + OverallCond
                                      + LotArea + BsmtFullBath + BsmtUnfSF, data = dt.validate)
denseTrain <- xgb.DMatrix(sparseTrain, label=dt.train$SalePrice)
denseValidate <- xgb.DMatrix(sparseValidate, label=dt.validate$SalePrice)

watchlist <- list(train = denseTrain, test = denseValidate)
xgb.model <- xgb.train(booster="gblinear", eta=0.1, data = denseTrain, watchlist = watchlist, nrounds = 20000, early_stopping_rounds = 5)

sparseComplete <- sparse.model.matrix(SalePrice ~ -1 + totalSF + OverallQual + Neighborhood 
                                      + KitchenQual + BsmtQual + hasTwoFloors
                                      + ageOfHouse + FullBath + GarageArea
                                      + MSZoning + Fireplaces  + SaleCondition + OverallCond
                                      + LotArea + BsmtFullBath + BsmtUnfSF, data = dt.complete)
denseComplete <- xgb.DMatrix(sparseComplete, label=dt.complete$SalePrice)
xgb.model.predict <- xgb.train(booster="gblinear", eta=0.1, data = denseComplete, nrounds = 8690)

sparseTest <- sparse.model.matrix(~ -1 + totalSF + OverallQual + Neighborhood 
                                  + KitchenQual + BsmtQual + hasTwoFloors
                                  + ageOfHouse + FullBath + GarageArea
                                  + MSZoning + Fireplaces  + SaleCondition + OverallCond
                                  + LotArea + BsmtFullBath + BsmtUnfSF, data = dt.test.complete)

denseTest <- xgb.DMatrix(sparseTest)
dt.test.complete <- dt.test.complete[,!names(dt.test.complete) %in% "SalePrice"]
dt.test.complete$SalePrice <- predict(xgb.model.predict, denseTest)
dt.test.complete$SalePrice <- exp(dt.test.complete$SalePrice)
dt.submit <- dt.test.complete[,c("Id","SalePrice")]
write_csv(dt.submit, './submission.csv')
