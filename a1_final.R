set.seed(100)
rm(list = ls())
x <- c('data.table','dplyr','mice')
lapply(x,require,character.only=TRUE)
memory.size(max = TRUE)
options(scipen = 99,digits = 10)
options(h2o.use.data.table = TRUE)


df_train <- fread("E:/MMA2020/5 MMA 867 Predictive modelling/Individual Assingment 1/house-prices-advanced-regression-techniques/train.csv",showProgress = T)

df_test <- fread("E:/MMA2020/5 MMA 867 Predictive modelling/Individual Assingment 1/house-prices-advanced-regression-techniques/test.csv",showProgress = T)

#md.pattern(df_train)
#######################################################################################################################3
###############################     h   2   o ##############################################
#######################################################################################################################3

# Dropping outliers based on Tableau
df_train <- df_train[-c(524,1299)]
df_train <- df_train %>% mutate_if(is.character,as.factor)

df_train <- df_train %>% rename('firstflrSF'='1stFlrSF','secondflrSF'='2ndFlrSF','third_SsnPorch'='3SsnPorch')
df_test <- df_test %>% rename('firstflrSF'='1stFlrSF','secondflrSF'='2ndFlrSF','third_SsnPorch'='3SsnPorch')
#NAcol <- which(colSums(is.na(df_train)) > 0)
#sort(colSums(sapply(df_train[NAcol], is.na)), decreasing = TRUE)

mice_imputes = mice(df_train, m=1, maxit = 2,method='cart')
temp <- complete(mice_imputes,1)
df_train <- temp

df_train$SalePrice <- log(df_train$SalePrice)
df_train$house_age <- 2020-df_train$YearBuilt
df_train$lg_house_age <- log(df_train$house_age)
df_train$MSSubClass <- as.factor(df_train$MSSubClass)
# df_train$`1stFlrSF` <- log(df_train$`1stFlrSF`)
df_train$`totFlrSF` <- df_train$`firstflrSF`+df_train$`secondflrSF`
df_train$totbath <- df_train$FullBath+df_train$HalfBath+df_train$BsmtHalfBath+df_train$BsmtFullBath
df_train$tot_ar <- df_train$GrLivArea + df_train$TotalBsmtSF+df_train$totFlrSF
df_train$tot_porch <- df_train$OpenPorchSF + df_train$EnclosedPorch + df_train$third_SsnPorch + df_train$ScreenPorch

df_train$`lg_totFlrSF` <- log(df_train$`firstflrSF`+df_train$`secondflrSF`)
df_train$lg_totbath <- log(df_train$FullBath+df_train$HalfBath+df_train$BsmtHalfBath+df_train$BsmtFullBath)
df_train$lg_tot_ar <- log(df_train$GrLivArea + df_train$TotalBsmtSF+df_train$totFlrSF)
df_train$lg_tot_porch <- log(df_train$OpenPorchSF + df_train$EnclosedPorch + df_train$third_SsnPorch + df_train$ScreenPorch)


# df_train$MoSold <- as.factor(df_train$MoSold)
# df_train$YrSold <- as.factor(df_train$YrSold)

df_train$Remod <- as.factor(ifelse(df_train$YearBuilt==df_train$YearRemodAdd, 0, 1))
df_train$Age <- as.numeric(df_train$YrSold)-df_train$YearRemodAdd
df_train$lg_Age <- log(df_train$Age)
##nEw
df_train$lg_MiscVal <- log(df_train$MiscVal+1)
df_train$lg_MasVnrArea <- log(df_train$MasVnrArea+1)
df_train$lg_LotArea <- log(df_train$LotArea+1)
df_train$lg_LotFrontage <- log(df_train$LotFrontage+1)
df_train$lg_ltarea_front <- log((df_train$LotArea+1)*(df_train$LotFrontage+1))
df_train$lg_BsmtFinSF1 <- log(df_train$BsmtFinSF1+1)
df_train$lg_BsmtUnfSF <- log(df_train$BsmtUnfSF+1)
df_train$lg_TotRmsAbvGrd <- log(df_train$TotRmsAbvGrd+1)
df_train$lg_WoodDeckSF <- log(df_train$WoodDeckSF+1)
df_train$lg_OpenPorchSF <- log(df_train$OpenPorchSF+1)






df_test$house_age <- 2020-df_test$YearBuilt

# df_test$`1stFlrSF` <- log(df_test$`1stFlrSF`)
df_test$MSSubClass <- as.factor(df_test$MSSubClass)
df_test$`totFlrSF` <- df_test$`firstflrSF`+df_test$`secondflrSF`

df_test$totbath <- df_test$FullBath+df_test$HalfBath+df_test$BsmtHalfBath+df_test$BsmtFullBath
df_test$tot_ar <- df_test$GrLivArea + df_test$TotalBsmtSF+df_test$`totFlrSF`
df_test$tot_porch <- df_test$OpenPorchSF + df_test$EnclosedPorch + df_test$third_SsnPorch + df_test$ScreenPorch

df_test$`lg_totFlrSF` <- log(df_test$`firstflrSF`+df_test$`secondflrSF`)
df_test$lg_totbath <- log(df_test$FullBath+df_test$HalfBath+df_test$BsmtHalfBath+df_test$BsmtFullBath)
df_test$lg_tot_ar <- log(df_test$GrLivArea + df_test$TotalBsmtSF+df_test$`totFlrSF`)
df_test$lg_tot_porch <- log(df_test$OpenPorchSF + df_test$EnclosedPorch + df_test$third_SsnPorch + df_test$ScreenPorch)


# df_train$OverallCond <- as.factor(df_train$OverallCond)
# df_test$OverallCond <- as.factor(df_test$OverallCond)
# df_train$OverallQual <- as.factor(df_train$OverallQual)
# df_test$OverallQual <- as.factor(df_test$OverallQual)


# df_test$MoSold <- as.factor(df_test$MoSold)
# df_test$MoSold <- as.numeric(df_test$MoSold)
df_test$Remod <- as.factor(ifelse(df_test$YearBuilt==df_test$YearRemodAdd, 0, 1)) #0=No Remodeling, 1=Remodeling
df_test$Age <- as.numeric(df_test$YrSold)-df_test$YearRemodAdd

df_test$lg_house_age <- log(df_test$house_age)
df_test$lg_Age <- log(df_test$Age)  

df_test$lg_MiscVal <- log(df_test$MiscVal+1)
df_test$lg_MasVnrArea <- log(df_test$MasVnrArea+1)
df_test$lg_LotArea <- log(df_test$LotArea+1)
df_test$lg_LotFrontage <- log(df_test$LotFrontage+1)
df_test$lg_ltarea_front <- log((df_test$LotArea+1)*(df_test$LotFrontage+1))
df_test$lg_BsmtFinSF1 <- log(df_test$BsmtFinSF1+1)
df_test$lg_BsmtUnfSF <- log(df_test$BsmtUnfSF+1)
df_test$lg_TotRmsAbvGrd <- log(df_test$TotRmsAbvGrd+1)
df_test$lg_WoodDeckSF <- log(df_test$WoodDeckSF+1)
df_test$lg_OpenPorchSF <- log(df_test$OpenPorchSF+1)




df_train <- df_train %>% mutate_if(is.character,as.factor)
df_test <- df_test %>% mutate_if(is.character,as.factor)

detach(package:h2o)
library(h2o)
h2o.init(nthreads = 2, max_mem_size = "10g")

df_train <- as.h2o(df_train)
df_test <- as.h2o(df_test)
df.splits <- h2o.splitFrame(data =  df_train, ratios = .8)
train <- df.splits[[1]]
valid <- df.splits[[2]]

colnames(df_train)
predictors <- colnames(df_train)[2:80]
predictors
# predictors <- c(predictors,"house_age","totFlrSF","totbath","tot_ar","tot_porch","lg_totFlrSF","lg_totbath"
predictors <- c(predictors,"totFlrSF","totbath","tot_ar","tot_porch","lg_totFlrSF","lg_totbath"
                ,"lg_tot_ar","lg_tot_porch","Remod","Age","lg_Age","lg_house_age",
                "lg_MiscVal","lg_MasVnrArea","lg_LotArea","lg_LotFrontage","lg_ltarea_front"
                ,"lg_BsmtFinSF1","lg_BsmtUnfSF","lg_TotRmsAbvGrd","lg_WoodDeckSF","lg_OpenPorchSF"
)
# predictors <- c(predictors,"house_age","totFlrSF")
sort(predictors)
response <- "SalePrice"

# interact_list <- c("MSSubClass","MSZoning","Neighborhood","OverallCond","Condition1","Condition2","RoofMatl","ExterQual","ExterCond")
# interact_list <- c("BldgType","MSSubClass")
# interact_list <- c("Condition1","Condition2")

housing_glm <- h2o.glm(x = predictors, y = response, training_frame = train,
                       validation_frame = valid,alpha = 0.35,nfolds = 10,lambda = 0.001,seed = 1#,lambda_search = TRUE
                       # ,interaction_pairs =list(
                       # c("MSSubClass","MSZoning","SaleCondition","OverallCond") )
                       # ,interactions = interact_list
                       # ignore_const_cols = FALSE
)

print(h2o.mse(housing_glm, valid=TRUE))
print(h2o.mae(housing_glm, valid=TRUE))
print(h2o.rmse(housing_glm, valid=TRUE))

h2o.std_coef_plot(housing_glm,num_of_features=40)
h2o.varimp_plot(housing_glm)
################################################################################
# GRID SEARCH
################################################################################
# hyper_params <- list( alpha = c(0, .25, .5,.75,.89,.99,1) )
hyper_params <- list( lambda = c(1, 0.5, 0.1, 0.01, 0.001, 0.0001, 0.00001, 0) ,
                      alpha = c(0, .25, .5,.75,.89,.99,1))
grid <- h2o.grid(x = predictors, y = response ,training_frame = train,nfolds=10,validation_frame = valid,
                 algorithm = "glm", grid_id = "allstate_grid", hyper_params = hyper_params,seed=1,#lambda_search = TRUE,#alpha=1,
                 search_criteria = list(strategy = "Cartesian"))

# Sort the grid models by mse
sortedGrid <- h2o.getGrid("allstate_grid", sort_by = "rmse", decreasing = FALSE)
sortedGrid
View(sortedGrid@summary_table)

pred <- exp(h2o.predict(object=housing_glm, newdata=df_test))
pred

mean(pred)
mean(pred[1090])

h2o.exportFile(
  pred,
  "E:/MMA2020/5 MMA 867 Predictive modelling/Individual Assingment 1/house-prices-advanced-regression-techniques/pred_h20_33.csv")



h2o.shutdown()
h2o.init(nthreads = -1)
