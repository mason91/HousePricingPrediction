linearMod3 <- lm(SalePrice ~ totalSF + OverallQual + GrLivArea + Neighborhood + ExterQual
                 + GarageCars + KitchenQual + FirstFlrSF + TotalBsmtSF + BsmtQual
                 + GarageArea + TotRmsAbvGrd + ageOfHouse + FireplaceQu + FullBath
                 + MSSubClass + Fireplaces + BsmtFinSF1
                 + LotArea, data=dt.complete)

dt.test.complete$SalePrice <- NA
dt.test.complete$SalePrice <- predict(linearMod.best, dt.test.complete)
dt.test.complete$SalePrice <- exp(dt.test.complete$SalePrice)
dt.submit <- dt.test.complete[,c("Id","SalePrice")]
write_csv(dt.submit, './submission.csv')
