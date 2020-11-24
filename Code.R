library(tidyverse)
library(vroom)
library(mlr)
library(randomForest)
library(scales)
library(rlist) # list.save(), list.load()
library(Cairo)
loaded_package <- c("tidyverse", "vroom", "mlr", "randomForest", "scales", "rlist", "Cairo", "showtext")
.version <- map(loaded_package, packageVersion)
names(.version) <- loaded_package
.version

#### 1. Loading data ---------------------------------------------------------
system.time(
  train <- vroom("./data/train.csv"))
system.time(
  test <- vroom("./data/test.csv"))
glimpse(train)
glimpse(test)
cat("There are", dim(train)[1], "observations and", dim(train)[2], "variables in training dataset.")
cat("There are", dim(test)[1], "observations and", dim(test)[2], "variables in test dataset.")
# -> test dataset does not have Y.

#### 2. Data wrangling -------------------------------------------------------
test_ID <- test$Id
test$Id <- NULL
test$SalePrice <- NA
train$Id <- NULL

## To check entire data set
house <- bind_rows(train, test)

### (1) check distribution of Y
CairoWin()
ggplot(data = house %>% filter(!is.na(SalePrice))) +
  geom_histogram(aes(x = SalePrice), fill = "blue", alpha = 1/2, binwidth = 10000) +
  scale_x_continuous(labels = dollar_format()) +
  labs(
    title = "SalePrice(Y) is skewed to the right"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), # title 조정
  )
# ggsave("./data/Houseprice/y_dist.png", dpi = 100, device = "png", type = "cairo")

### (2) check missing data
col_na <- function(df, fun){
  out <- vector("double", length(df))
  for(i in seq_along(df)) out[i] <- fun(is.na(df[[i]]))
  names(out) <- names(df)
  sort(out[out > 0], decreasing = TRUE)
}

# Delete SalePrice(Y) because the test set does not have a response variable 
na_prop <- house %>% 
  select(-SalePrice) %>% 
  map(is.na) %>% 
  map_dfr(mean) %>% # use sum() if you want to know the number of missing values per columns
  pivot_longer(cols = everything(), names_to = "variables", values_to = "prop") %>% 
  filter(prop > 0) %>% 
  arrange(desc(prop))

CairoWin()
na_prop %>%
  ggplot(aes(x = fct_reorder(variables, prop), y = prop, fill = variables)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(legend.position = "none") +
  labs(
    x = "Explanatory variables",
    y = "The proportions of NA values per column"
  ) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  theme(axis.text.y = element_text(size = 12))
# ggsave("./data/Houseprice/NA_prop.png", dpi = 100, device = "png", type = "cairo")

### (3) Handling the columns which have missing values.
## A problem in which the label is incorrectly coded.
# Pool QC : Pool Quality. It's ordinal.
# MiscFeature : Miscellaneous feature not covered in other categories
# Alley : Type of alley access to property
# Fence : Fence quality. It's ordinal.
# FireplaceQu : Fireplace quality. It's ordinal.
# GarageType
# GarageFinish : It's seem to be ordinal.
# GarageQual : It's seem to be ordinal.
# GarageCond : Garage condition. ordinal
# GarageArea : Size of garage in square feet
# BsmtExposure : Refers to walkout or garden level walls. ordinal
# BsmtCond : Evaluates the general condition of the basement. ordinal
# BsmtQual: Evaluates the height of the basement. ordinal
# BsmtFinType1: Rating of basement finished area. ordinal
# BsmtFinType2: Rating of basement finished area (if multiple types)
house$PoolQC[is.na(house$PoolQC)] <- "None"
house$MiscFeature[is.na(house$MiscFeature)] <- "None"
house$Alley[is.na(house$Alley)] <- "No"
house$Fence[is.na(house$Fence)] <- "No"
house$FireplaceQu[is.na(house$FireplaceQu)] <- "No"
house$GarageType[is.na(house$GarageType)] <- "No"
house$GarageFinish[is.na(house$GarageFinish)] <- "No"
house$GarageQual[is.na(house$GarageQual)] <- "No"
house$GarageCond[is.na(house$GarageCond)] <- "No"
house$BsmtExposure[is.na(house$BsmtExposure)] <- "NoBs"
house$BsmtCond[is.na(house$BsmtCond)] <- "NoBs"
house$BsmtQual[is.na(house$BsmtQual)] <- "NoBs"
house$BsmtFinType1[is.na(house$BsmtFinType1)] <- "NoBs"
house$BsmtFinType2[is.na(house$BsmtFinType2)] <- "NoBs"

# To specify the levels of ordered factors
PoolQC_lev <- c("None", "Fa", "TA", "Gd", "Ex")
Fence_lev <- c("No", "MnWw", "GdWo", "MnPrv", "GdPrv")
FireplaceQu_lev <- c("No", "Po", "Fa", "TA", "Gd", "Ex")
GarageFinish_lev <- c("No", "Unf", "RFn", "Fin")
GarageQual_lev <- c("No", "Po", "Fa", "TA", "Gd", "Ex")
GarageCond_lev <- c("No", "Po", "Fa", "TA", "Gd", "Ex")
BsmtExposure_lev <- c("NoBs", "No", "Mn", "Av", "Gd")
BsmtCond_lev <- c("NoBs", "Po", "Fa", "TA", "Gd", "Ex")
BsmtQual_lev <- c("NoBs", "Po", "Fa", "TA", "Gd", "Ex")
BsmtFinType1_lev <- c("NoBs", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ")
BsmtFinType2_lev <- c("NoBs", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ")

house2 <- house %>%
  mutate(PoolQC = parse_factor(PoolQC, levels = PoolQC_lev, ordered = TRUE),
         MiscFeature = parse_factor(MiscFeature),
         Alley = parse_factor(Alley),
         Fence = parse_factor(Fence, levels = Fence_lev, ordered = TRUE),
         FireplaceQu = parse_factor(FireplaceQu, levels = FireplaceQu_lev, ordered = TRUE),
         GarageType = parse_factor(GarageType),
         GarageFinish = parse_factor(GarageFinish, levels = GarageFinish_lev, ordered = TRUE),
         GarageQual = parse_factor(GarageQual, levels = GarageQual_lev, ordered = TRUE),
         GarageCond = parse_factor(GarageCond, levels = GarageCond_lev, ordered = TRUE),
         BsmtExposure = parse_factor(BsmtExposure, levels = BsmtExposure_lev, ordered = TRUE),
         BsmtCond = parse_factor(BsmtCond, levels = BsmtCond_lev, ordered = TRUE),
         BsmtQual = parse_factor(BsmtQual, levels = BsmtQual_lev, ordered = TRUE),
         BsmtFinType1 = parse_factor(BsmtFinType1, levels = BsmtFinType1_lev, ordered = TRUE),
         BsmtFinType2 = parse_factor(BsmtFinType2, levels = BsmtFinType2_lev, ordered = TRUE))
col_na(house2[-length(house)], sum) # The number of NA values per column. 

## Garage Series
# GarageYrBlt : If there is a garage, maybe GarageYrBlt is similar to YearBuilt or YearRemodAdd. 
# so I'm going to remove this column.
house2$GarageYrBlt <- NULL

# GarageArea : Size of garage in square feet
house2 %>% 
  filter(is.na(GarageArea)) %>% 
  select(GarageType:GarageCond)
# Since this observation doesn't seem to have Garage, I'm going to impute 0 in GarageCars and GarageArea respectively.
house3 <- house2 %>% 
  mutate(GarageArea = ifelse(is.na(GarageArea), 0, GarageArea),
         GarageCars = ifelse(is.na(GarageCars), 0, GarageCars))
col_na(house3 %>% select(-SalePrice), sum)

## LotFrontage: Linear feet of street connected to property
# I'm going to impute NA values of LotFrontage using the Neighbothood colum.
# So Let's take the median per neighbor.
CairoWin()
ggplot(house3, aes(x = Neighborhood, y = LotFrontage)) +
  stat_boxplot(geom = 'errorbar') +
  geom_boxplot(fill = "skyblue", alpha = 0.8) +
  coord_flip() +
  labs(
    x = "Neighborhood"
  )
# ggsave("./data/Houseprice/LotFrontage.png", dpi = 100, device = "png", type = "cairo")

LotFront_med <- house3 %>%
  group_by(Neighborhood) %>%
  summarize(
    LotFrontage_med = median(LotFrontage, na.rm = TRUE)
  ) %>%
  arrange(desc(LotFrontage_med))

Lotfrontage_new <- house3 %>% 
  select(Neighborhood, LotFrontage) %>% 
  left_join(LotFront_med, by = "Neighborhood") %>% 
  mutate(LotFrontage_new = 
           ifelse(is.na(LotFrontage), LotFrontage_med, LotFrontage)) %>% 
  pull(LotFrontage_new)

house4 <- house3 %>% 
  mutate(LotFrontage = Lotfrontage_new)
col_na(house4 %>% select(-SalePrice), sum)

## MasVnrType: Masonry veneer type
## MasVnrArea: Masonry veneer area in square feet
house4 %>% 
  filter(is.na(MasVnrType) | is.na(MasVnrArea)) %>% 
  select(MasVnrType, MasVnrArea)

# First of all, I find the one that should have MasVnrType. I will impute mode of MasVnrType without None type. 
# Lastly, I impute "None" to NA values of MasVnrtype because the values of MasVnrArea are also NA. 
# and then I impute 0 to NA values of MasVnrArea.
house4 %>% 
  filter(is.na(MasVnrType), !is.na(MasVnrArea)) %>% 
  select(MasVnrType, MasVnrArea, everything())

mode_vector <- function(tb, .col){
  .col <- enquo(.col) # prevent the evaluation of R code
  tb %>%
    group_by(!!.col) %>% 
    count() %>% 
    arrange(desc(n))
}

mode_MasType <- mode_vector(house4, MasVnrType) %>%
  filter(MasVnrType != "None") %>% 
  pull(MasVnrType)

MasVnrType_lev <- c("Stone", "None", "CBlock", "BrkFace", "BrkCmn") # To factorize at the end.
house5 <-house4 %>% 
  select(MasVnrType, MasVnrArea, everything()) %>% 
  mutate(MasVnrType = case_when(
    is.na(MasVnrType) & !is.na(MasVnrArea) ~ mode_MasType[[1]],
    is.na(MasVnrType) ~ "None",
    TRUE ~ MasVnrType),
    MasVnrArea = ifelse(is.na(MasVnrArea), 0, MasVnrArea),
    MasVnrType = parse_factor(MasVnrType, levels = MasVnrType_lev))
col_na(house5 %>% select(-SalePrice), sum)

## MSZoning: Identifies the general zoning classification of the sale.
house5 %>% 
  filter(is.na(MSZoning))

mode_MSZoning <- mode_vector(house5, MSZoning) %>% 
  pull(MSZoning)
mode_MSZoning # There is a typo in MSZoning. C is written in C (all). I'm going to fix these values.

MSZoning_lev <- c("RM", "RP", "RL", "RH", "I", "FV", "C", "A") # To factorize at the end.
house6 <- house5 %>% 
  mutate(MSZoning = case_when(
    is.na(MSZoning) ~ mode_MSZoning[[1]],
    MSZoning == "C (all)" ~ "C",
    TRUE ~ MSZoning),
    MSZoning = parse_factor(MSZoning, levels = MSZoning_lev))
house6
col_na(house6 %>% select(-SalePrice), sum)

## Utilities: Type of utilities available
house6 %>% 
  filter(is.na(Utilities)) %>% 
  select(Utilities, everything())
# Only one data point is "NoSeWa". Let's find where it is.
mode_vector(house6, Utilities)
which(house7$Utilities == "NoSeWa") # it's in training set, which means all houses in the test set has "AllPub"
# This makes the variable "Utilities" useless for prediction. Consequently, I'm going to get rid of it.
house7 <- house6 %>% 
  select(everything(), -Utilities)
col_na(house7 %>% select(-SalePrice), sum)

# BsmtFullBath: Basement full bathrooms, ordered.
# BsmtHalfBath: Basement half bathrooms, ordered.
# BsmtFinSF1: Type 1 finished square feet
# BsmtFinSF2: Type 2 finished square feet
# BsmtUnfSF: Unfinished square feet of basement area
# TotalBsmtSF: Total square feet of basement area

# We need to inspect the data points which has NA in BsmtFullBath and BsmtHalfBath and BsmtFinSF1
house7 %>% 
  filter(is.na(BsmtFullBath) | is.na(BsmtHalfBath) | is.na(BsmtFinSF1) ) %>% 
  select(BsmtFullBath, BsmtHalfBath, starts_with("BsmtFin"), BsmtUnfSF, ends_with("BsmtSF"))
# That is, "NoBs" in the result means we can impute 0 to NA values.
house8 <- house7 %>% 
  mutate(BsmtFullBath = ifelse(is.na(BsmtFullBath), 0, BsmtFullBath),
         BsmtHalfBath = ifelse(is.na(BsmtHalfBath), 0, BsmtHalfBath),
         BsmtFinSF1 = ifelse(is.na(BsmtFinSF1), 0, BsmtFinSF1),
         BsmtFinSF2 = ifelse(is.na(BsmtFinSF2), 0, BsmtFinSF2),
         BsmtUnfSF = ifelse(is.na(BsmtUnfSF), 0, BsmtUnfSF),
         TotalBsmtSF = ifelse(is.na(TotalBsmtSF), 0, TotalBsmtSF),
         BsmtFullBath = factor(BsmtFullBath, ordered = TRUE),
         BsmtHalfBath = factor(BsmtHalfBath, ordered = TRUE))
col_na(house8 %>% select(-SalePrice), sum)

## Home functionality (Assume typical unless deductions are warranted). ordered.
house8 %>% 
  filter(is.na(Functional)) %>% 
  select(Functional, everything())
mode_vector(house8, Functional)
mode_Func <- mode_vector(house8, Functional) %>% 
  pull(Functional)

# impute mode
Functional_lev <- c("Sal", "Sev", "Maj2", "Maj1", "Mod", "Min2", "Min1", "Typ") # To factorize at the end. 
house9 <- house8 %>% 
  mutate(Functional = ifelse(is.na(Functional), mode_Func[[1]], Functional),
         Functional = parse_factor(Functional, levels = Functional_lev, ordered = TRUE))
col_na(house9 %>% select(-SalePrice), sum)

## Exterior1st : Exterior covering on house
## Exterior2nd : Exterior covering on house (if more than one material)
house9 %>% 
  filter(is.na(Exterior1st) | is.na(Exterior2nd)) %>% 
  select(Exterior1st, Exterior2nd, everything())
mode_vector(house9, Exterior1st)
mode_vector(house9, Exterior2nd)

mode_1st <- mode_vector(house9, Exterior1st) %>% 
  pull(Exterior1st)
mode_2nd <- mode_vector(house9, Exterior2nd) %>% 
  pull(Exterior2nd)
# Impute mode and, as you can see above, there are typos in Exterior2nd. CemntBd is written in CmentBD and 
# WdShing is written in WD Shng. BrkComm is written in Brk Cmn. I'm going to modify these values.
Exterior_lev <- c("WdShing", "Wd Sdng", "VinylSd", "Stucco", "Stone", "PreCast", "Plywood", "Other", "MetalSd",
                  "ImStucc", "HdBoard", "CemntBd", "CBlock", "BrkFace", "BrkComm", "AsphShn", "AsbShng")

house10 <- house9 %>% 
  mutate(Exterior1st = ifelse(is.na(Exterior1st), mode_1st[[1]], Exterior1st),
         Exterior2nd = case_when(
           is.na(Exterior2nd) ~ mode_2nd[[1]],
           Exterior2nd == "CmentBd" ~ "CemntBd",
           Exterior2nd == "Wd Shng" ~ "WdShing",
           Exterior2nd == "Brk Cmn" ~ "BrkComm",
           TRUE ~ Exterior2nd),
         Exterior1st = parse_factor(Exterior1st, levels = Exterior_lev),
         Exterior2nd = parse_factor(Exterior2nd, levels = Exterior_lev))
col_na(house10 %>% select(-SalePrice), sum)

## Electrical : Electrical system
house10 %>% 
  filter(is.na(Electrical)) %>% 
  select(Electrical, everything())
mode_vector(house10, Electrical)
mode_Elec <- mode_vector(house11, Electrical) %>% 
  pull(Electrical)

# Factorizing. it seems to be ordinal.
Electrical_lev <- c("Mix", str_c("Fuse", c("P", "F", "A")), "SBrkr")
house11 <- house10 %>% 
  mutate(Electrical = ifelse(is.na(Electrical), mode_Elec[[1]], Electrical),
         Electrical = parse_factor(Electrical, levels = Electrical_lev, ordered = TRUE))
col_na(house11 %>% select(-SalePrice), sum)

## KitchenQual : Kitchen quality
house11 %>% 
  filter(is.na(KitchenQual)) %>% 
  select(starts_with("Kitchen"), everything())
mode_vector(house11, KitchenAbvGr)

# I'm going to impute mode
mode_KitchenQual <- mode_vector(house11, KitchenQual) %>% 
  pull(KitchenQual)
KitchenQual_lev <- c("Po", "Fa", "TA", "Gd", "Ex")
house12 <- house11 %>% 
  mutate(KitchenQual = ifelse(is.na(KitchenQual), mode_KitchenQual[[1]], KitchenQual),
         KitchenQual = parse_factor(KitchenQual, levels = KitchenQual_lev, ordered = TRUE))
col_na(house12 %>% select(-SalePrice), sum)

## SaleType : Type of sale
house12 %>% 
  filter(is.na(SaleType )) %>% 
  select(SaleType , everything())
mode_vector(house12, SaleType)

# I'm going to impute mode
mode_SaleType <- mode_vector(house12, SaleType) %>% 
  pull(SaleType)
SaleType_lev <- c("Oth", "ConLD", "ConLI", "ConLw", "Con", "COD", "New", "VWD", "CWD", "WD")
house13 <- house12 %>% 
  mutate(SaleType = ifelse(is.na(SaleType), mode_SaleType[[1]], SaleType),
         SaleType = parse_factor(SaleType, levels = SaleType_lev))
col_na(house13 %>% select(-SalePrice), sum)

## Factorizing the remaining character variables
# check the remaining character variables
cols_chr <- map(house13, is.character) %>% 
  as_tibble %>% 
  pivot_longer(cols = everything(), names_to = "variables", values_to = "character") %>% 
  filter(character == 1) %>% 
  pull(variables)
cols_chr

# Street: Type of road access to property
# LotShape : General shape of property. ordinal
# LandContour: Flatness of the property
# LotConfig: Lot configuration
# LandSlope : Slope of property. ordinal
# Neighborhood: Physical locations within Ames city limits
# Condition1 : Proximity to various conditions
# Condition2 : Proximity to various conditions (if more than one is present)
# BldgType : Type of dwelling
# HouseStyle : Style of dwelling
# RoofStyle : Type of roof
# RoofMatl : Roof material
# ExterQual : Evaluates the quality of the material on the exterior. ordinal
# ExterCond : Evaluates the present condition of the material on the exterior. ordinal
# Foundation : Type of foundation
# Heating : Type of heating
# HeatingQC : Heating quality and condition. ordinal
# CentralAir : Central air conditioning
# PavedDrive : Paved driveway
# SaleCondition : Condition of sale

# specify labels of factors which is seems to be ordinal.
LotShape_lev <- c(str_c("IR", 3:1), "Reg")
LandSlope_lev <- c("Sev", "Mod", "Gtl")
Exter_lev <- c("Po", "Fa", "TA", "Gd", "Ex")
HeatingQC_lev <- c("Po", "Fa", "TA", "Gd", "Ex")

# I'm going to use across() in dplyr 1.0.0, in order to factorize the remaining character variables after parsing ordinal factors.
# It's really useful! `r emo::ji("smile)`
house14 <- house13 %>%
  mutate(LotShape = parse_factor(LotShape, levels = LotShape_lev, ordered = TRUE),
         LandSlope = parse_factor(LandSlope, levels = LandSlope_lev, ordered = TRUE),
         ExterQual = parse_factor(ExterQual, levels = Exter_lev, ordered = TRUE),
         ExterCond = parse_factor(ExterCond, levels = Exter_lev, ordered = TRUE),
         HeatingQC = parse_factor(HeatingQC, levels = HeatingQC_lev, ordered = TRUE)) %>% 
  mutate(across(is.character, parse_factor))

### (4) Preparing data for modeling R's variable naming conventions
## Some of the original column names cause problems with R's variable naming conventions,
## which causes problems in modelling step of {mlr}. so the following process is required:
idx_str <- str_detect(colnames(house14), "^\\d") # It starts with number. 
colnames(house14)[idx_str] <- str_c("a", colnames(house14)[idx_str])

#### 3. model ----------------------------------------------------------------
train <- house14 %>%
  filter(!is.na(SalePrice))
test <- house14 %>%
  filter(is.na(SalePrice)) %>%
  select(-SalePrice)
train_Task <- makeRegrTask(data = train, target = "SalePrice")

## normlize the variables
train_Task <- normalizeFeatures(train_Task, method = "standardize")
test_Task <- normalizeFeatures(test, method = "standardize")

## Ranger
# getParamSet("regr.ranger")
.ranger <- makeLearner("regr.ranger", predict.type = "response")

# set tunable parameters
# grid search to find optimal hyperparameters
ranger_params <- makeParamSet(
  makeIntegerParam("num.trees", lower = 450, upper = 550),
  makeIntegerParam("mtry", lower = 3, upper = 15),
  makeIntegerParam("min.node.size", lower = 5, upper = 20)
)

# From here, most of the steps would be similar to followed above,
# but this time I’ve done random search instead of grid search for parameter tuning, because it’s faster.
# Though random search is faster than grid search, but sometimes it turns out to be less efficient.
# In grid search, the algorithm tunes over every possible combination of parameters provided.
# In a random search, we specify the number of iterations and it randomly passes over the parameter combinations.
# In this process, it might miss out some important combination of parameters
# which could have returned maximum accuracy.

# let's do random search for 100 interations
rancontrol <- makeTuneControlRandom(maxit = 100L)
# set 3 fold cross validation
set_cv <- makeResampleDesc("CV", iters = 5L)
# hypertuning
set.seed(1)
ranger_tune <- tuneParams(learner = .ranger, resampling = set_cv, task = train_Task,
                          par.set = ranger_params, control = rancontrol, measures = rmsle)
# list.save(ranger_tune, "./data/ranger_tune.RData")
# ranger_tune <- list.load("./data/ranger_tune.RData")

ranger_tune$x # Optimal hyperparameters based on RMSLE
ranger_tune$x$importance <- "impurity" # To get variable importances
ranger_tune$x # best parmeters
ranger_tune$y

# using hyperparameters for modeling
ranger_tree <- setHyperPars(.ranger, par.vals = ranger_tune$x)
# train a model
set.seed(1)
mod_ranger <- mlr::train(ranger_tree, train_Task)
getLearnerModel(mod_ranger)

# Visualizing the variable importances of the Top 10 based on "MeanDecreaseGini"
imp <- getFeatureImportance(mod_ranger)
CairoWin()
imp$res %>%
  gather(key = "variable", value = "importance") %>%
  arrange(desc(importance)) %>% # 내림차순으로 정리
  slice(1:10) %>% # 상위 10개
  ggplot() +
  geom_bar(aes(x = fct_reorder(variable, importance), y = importance, fill = importance), 
           stat = "identity") +
  scale_y_continuous(breaks = NULL) +
  coord_flip() +
  labs(x = "Variables", y = "Importances") +
  theme(legend.position = "none")
ggsave("./data/Houseprice/variable_importances.png", dpi = 100, device = "png", type = "cairo")

# make predictions
pred_ranger <- predict(mod_ranger, newdata = test_Task)
# submission file
ranger_submit <- data.frame(ID = test_ID, SalePrice = pred_ranger$data$response)
write_excel_csv(ranger_submit, "./data/HousePrice/pred.csv") # RMSLE = 0.14430
exp(0.14430)
