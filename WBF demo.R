library(openxlsx)
library(ggplot2)
library(dplyr)
library(MASS)
library(psych)
library(corrplot)
library(magrittr)
library(scales)
library(reshape2)
library(gridExtra)
library(caret)
library(glmnet)

### Import data -----
wbf.data <- read.xlsx("~/WBF Data.xlsx", sheet = "Data") %>%
  mutate(EID = as.character(EID))

eng19.data <- read.xlsx("~/Engagement Data 2019.xlsx", sheet = "ENG19")


### Merge ----
wbf.full.data <- wbf.data %>%
  left_join(eng19.data, by = "EID") %>%
  mutate(Eng19.Agility = (A.1 + A.2 + A.3)/3) %>%
  mutate(Eng19.Com.Env = (CE.1 + CE.2 + CE.3)/3) %>%
  mutate(Eng19.DI = (DI.1 + DI.2 + DI.3 + DI.4)/4) %>%
  mutate(Eng19.EI = (EI.1 + EI.2 + EI.3 + EI.4)/4) %>%
  mutate(Eng19.FV = (FV.1 + FV.2 + FV.3 + FV.4)/4) %>%
  mutate(Eng19.GD = (GD.1 + GD.2 + GD.3 + GD.4)/4) %>%
  mutate(Eng19.MEI = (MEI.1 + MEI.2 + MEI.3 + MEI.4 + MEI.5)/5) %>%
  mutate(Eng19.PEI = (PEI.1 + PEI.2 + PEI.3 + PEI.4 + PEI.5 + PEI.6)/6) %>%
  mutate(Eng19.Safety = (S.1 + S.2 + S.3 + S.4 + S.5 + S.6 + S.7)/7) %>%
  mutate(Eng19 = (A.1 + A.2 + A.3 + CE.1 + CE.2 + CE.3
                  + DI.1 + DI.2 + DI.3 + DI.4
                  + EI.1 + EI.2 + EI.3 + EI.4
                  + FV.1 + FV.2 + FV.3 + FV.4
                  + GD.1 + GD.2 + GD.3 + GD.4
                  + MEI.1 + MEI.2 + MEI.3 + MEI.4 + MEI.5
                  + PEI.1 + PEI.2 + PEI.3 + PEI.4 + PEI.5 + PEI.6)/40) %>%
  mutate(Perf.ave = (Perf2015 + Perf2016 + Perf2017 + Perf2018)/4) %>%
  dplyr::select(EID, N, E, O, A, C,
                N.Worry, N.Intensity, N.Interpretation, N.Rebound.time,
                E.Warmth, E.Sociability, E.Activity.mode, E.Taking.charge, E.Trust, E.Tact,
                O.Imagination, O.Complexity, O.Change, O.Scope, 
                A.Others.needs, A.Agreement, A.Humility, A.Reserve,
                C.Perfectionism, C.Organization, C.Drive, C.Concentration, C.Methodicalness,
                Perf.ave, Engagement2017, Eng19,
                Eng19.Agility, Eng19.Com.Env, Eng19.DI, Eng19.EI, 
                Eng19.FV, Eng19.GD, Eng19.MEI, Eng19.PEI,
                Gender, Age.at.Test) %>%
  mutate_at(c("N", "E", "O", "A", "C",
              "N.Worry", "N.Intensity", "N.Interpretation", "N.Rebound.time",
              "E.Warmth", "E.Sociability", "E.Activity.mode", "E.Taking.charge", 
              "E.Trust", "E.Tact",
              "O.Imagination", "O.Complexity", "O.Change", "O.Scope", 
              "A.Others.needs", "A.Agreement", "A.Humility", "A.Reserve",
              "C.Perfectionism", "C.Organization", "C.Drive", "C.Concentration",
              "C.Methodicalness", "Perf.ave", "Engagement2017", "Eng19",
              "Eng19.Agility", "Eng19.Com.Env", "Eng19.DI", "Eng19.EI",
              "Eng19.FV", "Eng19.GD", "Eng19.MEI", "Eng19.PEI"),
            funs(scale(.)))
wbf.naomit.data <- na.omit(wbf.full.data)

### Correlations ----
wbf.cor <- wbf.data %>%
  dplyr::select(N, E, O , A, C, 
                Perf2015, Perf2016, Perf2017, Perf2018, PerfAvg,
                Engagement2017, Gender, Age.at.Test) %>%
  mutate(Gender = as.factor(Gender)) %>% #Female=1
  mutate_if(is.factor,as.numeric)

wbf.cor.matrix <- corr.test(wbf.cor, use = "pairwise")
wbf.cor.var <- round(wbf.cor.matrix$r,2)

corrplot::corrplot(wbf.cor.var, method = "color", addCoef.col = "black",
                   is.corr = T, order = "original",
                   addCoefasPercent = F,
                   number.font = 2,
                   number.cex = 0.5,
                   tl.col = "black")


bf.cor.matrix <- wbf.full.data %>%
  dplyr::select(N.Worry, N.Intensity, N.Interpretation, N.Rebound.time,
                E.Warmth, E.Sociability, E.Activity.mode, E.Taking.charge, E.Trust, E.Tact,
                O.Imagination, O.Complexity, O.Change, O.Scope, 
                A.Others.needs, A.Agreement, A.Humility, A.Reserve,
                C.Perfectionism, C.Organization, C.Drive, C.Concentration, C.Methodicalness,
                Eng19, Perf.ave) %>%
  corr.test(., use = "pairwise")
bf.cor <- round(bf.cor.matrix$r,2)
corrplot::corrplot(bf.cor, type = "lower", method = "color", addCoef.col = "black",
                   is.corr = T, order = "original",
                   addCoefasPercent = F,
                   number.font = 2,
                   number.cex = 0.5,
                   tl.col = "black")


eng.perf.cor.matrix <- wbf.full.data %>%
  dplyr::select(Gender, Age.at.Test, Perf.ave,
                Engagement2017, Eng19) %>%
  mutate(Gender = as.factor(Gender)) %>% #Female=1
  mutate_if(is.factor, as.numeric) %>%
  corr.test(., use = "pairwise")
eng.perf.cor <- round(eng.perf.cor.matrix$r, 2)
corrplot::corrplot(eng.perf.cor, type = "lower", 
                   method = "color", addCoef.col = "black",
                   is.corr = T, order = "original",
                   addCoefasPercent = F,
                   number.font = 2,
                   number.cex = 0.5,
                   tl.col = "black")

### Prediction with Big Five ----
perf15.mod <- lm(Perf2015 ~ Gender + Age.at.Test + N + E + O + A + C, data = wbf.data)
summary(perf15.mod)

perfavg.mod1 <- lm(PerfAvg ~ Gender + Age.at.Test + N + E + O + A + C
                   + N*E + N*O + N*A + N*C + O*C + E*A + E*O, data = wbf.data)
summary(perfavg.mod1)

perfavg.mod2 <- lm(PerfAvg ~ Gender + Age.at.Test + N + E + O + A + C, data = wbf.data)
summary(perfavg.mod2)


perfavg.mod2 <- lm(PerfAvg ~ Gender + Age.at.Test + Engagement2017 
                   + N + E + O + A + C, data = wbf.data)
summary(perfavg.mod2)

eng17.mod1 <- lm(Engagement2017 ~ Gender + Age.at.Test
                 + N + E + O + A + C, data = wbf.data)
summary(eng17.mod1)

eng19.mod1 <- lm(Eng19 ~ Gender + Age.at.Test + Perf.ave + Engagement2017
                 + N + E + O + A + C, data = wbf.full.data)
summary(eng19.mod1)

### Elastic Net with Big Five factors ----
##function for elastic nets with caret
#this function selects the best model given tuning parameters

get_best_result = function(caret_fit) {
  best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result = caret_fit$results[best, ]
  rownames(best_result) = NULL
  best_result
}

### Elastic net Performance average (2015 - 2018) with Big Five factors ----
# 80% of sample for training with 20 folds, repeat 3 times
set.seed(001)
wbf.training.perf <- createDataPartition(
  y = wbf.naomit.data$Perf.ave,
  p = 0.8, list = FALSE
)

wbf.train.data.perf  <- wbf.naomit.data[wbf.training.perf, ]
nrow(wbf.train.data.perf)
wbf.test.data.perf <- wbf.naomit.data[-wbf.training.perf, ]
nrow(wbf.test.data.perf)

wbf.modeltraining.perf.BF <- train(Perf.ave ~ Gender + Age.at.Test 
                                   + N + E + O + A + C,
                                   data = wbf.train.data.perf, method = "glmnet",
                                   trControl = trainControl("repeatedcv", 
                                                            number = 20, repeats = 3), 
                                   #Set cross-validation to be 20 fold, repeat 3 times
                                   tuneLength = 20  #Search 20 alpha values and 20 lambda values for each
)

wbf.modeltraining.perf.BF$resample %>% filter(is.na(Rsquared))

# Best tuning parameter
(wbf.stats.perf.BF <- get_best_result(wbf.modeltraining.perf.BF)[,1:4])

# Coefficients
wbf.coef.perf.BF <- coef(wbf.modeltraining.perf.BF$finalModel, 
                         wbf.modeltraining.perf.BF$bestTune$lambda, 
                         wbf.modeltraining.perf.BF$bestTune$alpha, exact = F)

wbf.coef.nonzero.perf.BF <- wbf.coef.perf[which(wbf.coef.perf.BF != 0)]

(wbf.coef.data.perf.BF <- data.frame(
  Variables = wbf.coef.perf.BF@Dimnames[[1]][which(wbf.coef.perf.BF != 0)],
  Coefficients = round(wbf.coef.perf.BF[which(wbf.coef.perf.BF != 0)],2)
))

# Make predictions on the test data
wbf.test.perf.BF <- model.matrix(Perf.ave ~ Gender + Age.at.Test 
                                 + N + E + O + A + C,
                                 data = wbf.test.data.perf)[,-1] #remove intercept
wbf.predictions.perf.BF <- predict(wbf.modeltraining.perf.BF, newdata = wbf.test.data.perf) 

# Statistics of Elastic Net
(wbf.stats.predict.perf.BF <- data.frame(
  Alpha = get_best_result(wbf.modeltraining.perf.BF)[,1],
  Lambda = get_best_result(wbf.modeltraining.perf.BF)[,2],
  RMSE = RMSE(wbf.predictions.perf.BF, wbf.test.data.perf$Perf.ave),
  Rsquare = R2(wbf.predictions.perf.BF, wbf.test.data.perf$Perf.ave)
))

### AIC Performance with Big Five factors ----
perfavg.mod2 <- lm(Perf.ave ~ Gender + Age.at.Test 
                   + N + E + O + A + C, data = wbf.naomit.data)
summary(perfavg.mod2)

perfavg.mod2.AIC <- step(perfavg.mod2,
                         scope = list(upper = Perf.ave ~ Gender + Age.at.Test
                                      + N + E + O + A + C, 
                                      lower = Perf.ave ~ 1),
                         direction = "both")
summary(perfavg.mod2.AIC)



### Elastic Net Engagement 2017 with Big Five factors ----
# 80% of sample for training, 20 folds, repeat 3 times
set.seed(002)
wbf.training.eng17 <- createDataPartition(
  y = wbf.naomit.data$Engagement2017,
  p = 0.8, list = FALSE
)

wbf.train.data.eng17  <- wbf.naomit.data[wbf.training.eng17, ]
nrow(wbf.train.data.eng17)
wbf.test.data.eng17 <- wbf.naomit.data[-wbf.training.eng17, ]
nrow(wbf.test.data.eng17)


wbf.modeltraining.eng17.BF <- train(Engagement2017 ~ Gender + Age.at.Test 
                                    + N + E + O + A + C,
                                    data = wbf.train.data.eng17, method = "glmnet",
                                    trControl = trainControl("repeatedcv", 
                                                             number = 20, repeats = 3), 
                                    #Set cross-validation to be 20 fold, repeat 3 times
                                    tuneLength = 20  #Search 20 alpha values and 20 lambda values for each
)

wbf.modeltraining.eng17.BF$resample %>% filter(is.na(Rsquared))

# Best tuning parameter
(wbf.stats.eng17.BF <- get_best_result(wbf.modeltraining.eng17.BF)[,1:4])

# Coefficients
wbf.coef.eng17.BF <- coef(wbf.modeltraining.eng17.BF$finalModel, 
                          wbf.modeltraining.eng17.BF$bestTune$lambda, 
                          wbf.modeltraining.eng17.BF$bestTune$alpha, exact = F)

wbf.coef.nonzero.eng17.BF <- wbf.coef.eng17.BF[which(wbf.coef.eng17.BF != 0)]

(wbf.coef.data.eng17.BF <- data.frame(
  Variables = wbf.coef.eng17.BF@Dimnames[[1]][which(wbf.coef.eng17.BF != 0)],
  Coefficients = round(wbf.coef.eng17.BF[which(wbf.coef.eng17.BF != 0)],2)
))

# Make predictions on the test data
wbf.test.eng17.BF <- model.matrix(Engagement2017 ~ Gender + Age.at.Test 
                                  + N + E + O + A + C,
                                  data = wbf.test.data.eng17)[,-1] #remove intercept
wbf.predictions.eng17.BF <- predict(wbf.modeltraining.eng17.BF, newdata = wbf.test.data.eng17) 

# Statistics of Elastic Net
(wbf.predictions.eng17.BF <- data.frame(
  Alpha = get_best_result(wbf.modeltraining.eng17.BF)[,1],
  Lambda = get_best_result(wbf.modeltraining.eng17.BF)[,2],
  RMSE = RMSE(wbf.predictions.eng17.BF, wbf.test.data.eng17$Engagement2017),
  Rsquare = R2(wbf.predictions.eng17.BF, wbf.test.data.eng17$Engagement2017)
))


### AIC Engagement 2017 with Big Five factors ----
eng17.mod1 <- lm(Engagement2017 ~ Gender + Age.at.Test
                 + N + E + O + A + C, data = wbf.naomit.data)
summary(eng17.mod1)

eng17.mod1.AIC <- step(eng17.mod1,
                       scope = list(upper = Perf.ave ~ Gender + Age.at.Test + Engagement2017 
                                    + N + E + O + A + C, 
                                    lower = Perf.ave ~ 1),
                       direction = "both")
summary(eng17.mod1.AIC)


### Predictions based on Big Five aspects -----
# Taking charge (E), Change (O), Complexity (O), Methodicalness (C), Drive (C), Agreement (A), Concentration (C)
perfavg.mod3 <- lm(Perf.ave ~ Gender + Age.at.Test 
                   + N.Worry + N.Intensity + N.Interpretation + N.Rebound.time
                   + E.Warmth + E.Sociability + E.Activity.mode 
                   + E.Taking.charge + E.Trust + E.Tact
                   + O.Imagination + O.Complexity + O.Change + O.Scope
                   + A.Others.needs + A.Agreement + A.Humility + A.Reserve
                   + C.Perfectionism + C.Organization + C.Drive 
                   + C.Concentration + C.Methodicalness, 
                   data = wbf.full.data)
summary(perfavg.mod3)

perfavg.mod3.AIC <- step(perfavg.mod3,
                         scope = list(upper = Perf.ave ~ Gender + Age.at.Test 
                                      + N.Worry + N.Intensity + N.Interpretation + N.Rebound.time
                                      + E.Warmth + E.Sociability + E.Activity.mode 
                                      + E.Taking.charge + E.Trust + E.Tact
                                      + O.Imagination + O.Complexity + O.Change + O.Scope
                                      + A.Others.needs + A.Agreement + A.Humility + A.Reserve
                                      + C.Perfectionism + C.Organization + C.Drive 
                                      + C.Concentration + C.Methodicalness,
                                      lower = Perf.ave ~ 1),
                         direction = "both")
summary(perfavg.mod3.AIC)


### ElasticNet models ----

##function for elastic nets with caret
#this function selects the best model given tuning parameters

get_best_result = function(caret_fit) {
  best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result = caret_fit$results[best, ]
  rownames(best_result) = NULL
  best_result
}

### Performance average (2015 - 2018) ----
# 80% of sample for training with 20 folds, repeat 3 times
set.seed(001)
wbf.training.perf <- createDataPartition(
  y = wbf.naomit.data$Perf.ave,
  p = 0.8, list = FALSE
)

wbf.train.data.perf  <- wbf.naomit.data[wbf.training.perf, ]
nrow(wbf.train.data.perf)
wbf.test.data.perf <- wbf.naomit.data[-wbf.training.perf, ]
nrow(wbf.test.data.perf)

wbf.modeltraining.perf <- train(Perf.ave ~ Gender + Age.at.Test 
                                + N.Worry + N.Intensity + N.Interpretation + N.Rebound.time
                                + E.Warmth + E.Sociability + E.Activity.mode 
                                + E.Taking.charge + E.Trust + E.Tact
                                + O.Imagination + O.Complexity + O.Change + O.Scope
                                + A.Others.needs + A.Agreement + A.Humility + A.Reserve
                                + C.Perfectionism + C.Organization + C.Drive 
                                + C.Concentration + C.Methodicalness,
                                data = wbf.train.data.perf, method = "glmnet",
                                trControl = trainControl("repeatedcv", 
                                                         number = 20, repeats = 3), 
                                #Set cross-validation to be 20 fold, repeat 3 times
                                tuneLength = 20  #Search 20 alpha values and 20 lambda values for each
)

wbf.modeltraining.perf$resample %>% filter(is.na(Rsquared))

# Best tuning parameter
(wbf.stats.perf <- get_best_result(wbf.modeltraining.perf)[,1:4])

# Coefficients
wbf.coef.perf <- coef(wbf.modeltraining.perf$finalModel, 
                      wbf.modeltraining.perf$bestTune$lambda, 
                      wbf.modeltraining.perf$bestTune$alpha, exact = F)

wbf.coef.nonzero.perf <- wbf.coef.perf[which(wbf.coef.perf != 0)]

(wbf.coef.data.perf <- data.frame(
  Variables = wbf.coef.perf@Dimnames[[1]][which(wbf.coef.perf != 0)],
  Coefficients = round(wbf.coef.perf[which(wbf.coef.perf != 0)],2)
))

# Make predictions on the test data
wbf.test.perf <- model.matrix(Perf.ave ~ Gender + Age.at.Test 
                              + N.Worry + N.Intensity + N.Interpretation + N.Rebound.time
                              + E.Warmth + E.Sociability + E.Activity.mode 
                              + E.Taking.charge + E.Trust + E.Tact
                              + O.Imagination + O.Complexity + O.Change + O.Scope
                              + A.Others.needs + A.Agreement + A.Humility + A.Reserve
                              + C.Perfectionism + C.Organization + C.Drive 
                              + C.Concentration + C.Methodicalness,
                              data = wbf.test.data.perf)[,-1] #remove intercept
wbf.predictions.perf <- predict(wbf.modeltraining.perf, newdata = wbf.test.data.perf) 

# Statistics of Elastic Net
(wbf.stats.predict.perf <- data.frame(
  Alpha = get_best_result(wbf.modeltraining.perf)[,1],
  Lambda = get_best_result(wbf.modeltraining.perf)[,2],
  RMSE = RMSE(wbf.predictions.perf, wbf.test.data.perf$Perf.ave),
  Rsquare = R2(wbf.predictions.perf, wbf.test.data.perf$Perf.ave)
))


### Engagement 2017 ----
# 80% of sample for training with 20 folds, repeat 3 times
set.seed(002)
wbf.training.eng17 <- createDataPartition(
  y = wbf.naomit.data$Engagement2017,
  p = 0.8, list = FALSE
)

wbf.train.data.eng17  <- wbf.naomit.data[wbf.training.eng17, ]
nrow(wbf.train.data.eng17)
wbf.test.data.eng17 <- wbf.naomit.data[-wbf.training.eng17, ]
nrow(wbf.test.data.eng17)


wbf.modeltraining.eng17 <- train(Engagement2017 ~ Gender + Age.at.Test 
                                 + N.Worry + N.Intensity + N.Interpretation + N.Rebound.time
                                 + E.Warmth + E.Sociability + E.Activity.mode 
                                 + E.Taking.charge + E.Trust + E.Tact
                                 + O.Imagination + O.Complexity + O.Change + O.Scope
                                 + A.Others.needs + A.Agreement + A.Humility + A.Reserve
                                 + C.Perfectionism + C.Organization + C.Drive 
                                 + C.Concentration + C.Methodicalness,
                                 data = wbf.train.data.perf, method = "glmnet",
                                 trControl = trainControl("repeatedcv", 
                                                          number = 20, repeats = 3), 
                                 #Set cross-validation to be 20 fold, repeat 3 times
                                 tuneLength = 20  #Search 20 alpha values and 20 lambda values for each
)

wbf.modeltraining.eng17$resample %>% filter(is.na(Rsquared))

# Best tuning parameter
(wbf.stats.eng17 <- get_best_result(wbf.modeltraining.eng17)[,1:4])

# Coefficients
wbf.coef.eng17 <- coef(wbf.modeltraining.eng17$finalModel, 
                       wbf.modeltraining.eng17$bestTune$lambda, 
                       wbf.modeltraining.eng17$bestTune$alpha, exact = F)

wbf.coef.nonzero.eng17 <- wbf.coef.eng17[which(wbf.coef.eng17 != 0)]

(wbf.coef.data.eng17 <- data.frame(
  Variables = wbf.coef.eng17@Dimnames[[1]][which(wbf.coef.eng17 != 0)],
  Coefficients = round(wbf.coef.eng17[which(wbf.coef.eng17 != 0)],2)
))

# Make predictions on the test data
wbf.test.eng17 <- model.matrix(Engagement2017 ~ Gender + Age.at.Test 
                               + N.Worry + N.Intensity + N.Interpretation + N.Rebound.time
                               + E.Warmth + E.Sociability + E.Activity.mode 
                               + E.Taking.charge + E.Trust + E.Tact
                               + O.Imagination + O.Complexity + O.Change + O.Scope
                               + A.Others.needs + A.Agreement + A.Humility + A.Reserve
                               + C.Perfectionism + C.Organization + C.Drive 
                               + C.Concentration + C.Methodicalness,
                               data = wbf.test.data.eng17)[,-1] #remove intercept
wbf.predictions.eng17 <- predict(wbf.modeltraining.eng17, newdata = wbf.test.data.eng17) 

# Statistics of Elastic Net
(wbf.stats.predict.eng17 <- data.frame(
  Alpha = get_best_result(wbf.modeltraining.eng17)[,1],
  Lambda = get_best_result(wbf.modeltraining.eng17)[,2],
  RMSE = RMSE(wbf.predictions.eng17, wbf.test.data.eng17$Engagement2017),
  Rsquare = R2(wbf.predictions.eng17, wbf.test.data.eng17$Engagement2017)
))


### Engagement 2019  ----
# 80% of sample for training with 20 folds, repeat 3 times
set.seed(001)
wbf.training.eng19 <- createDataPartition(
  y = wbf.naomit.data$Eng19,
  p = 0.8, list = FALSE
)

wbf.train.data.eng19  <- wbf.naomit.data[wbf.training.eng19, ]
nrow(wbf.train.data.eng19)
wbf.test.data.eng19 <- wbf.naomit.data[-wbf.training.eng19, ]
nrow(wbf.test.data.eng19)


wbf.modeltraining.eng19 <- train(Eng19 ~ Gender + Age.at.Test 
                                 + N.Worry + N.Intensity + N.Interpretation + N.Rebound.time
                                 + E.Warmth + E.Sociability + E.Activity.mode 
                                 + E.Taking.charge + E.Trust + E.Tact
                                 + O.Imagination + O.Complexity + O.Change + O.Scope
                                 + A.Others.needs + A.Agreement + A.Humility + A.Reserve
                                 + C.Perfectionism + C.Organization + C.Drive 
                                 + C.Concentration + C.Methodicalness,
                                 data = wbf.train.data.perf, method = "glmnet",
                                 trControl = trainControl("repeatedcv", 
                                                          number = 20, repeats = 3), 
                                 #Set cross-validation to be 20 fold, repeat 3 times
                                 tuneLength = 20  #Search 20 alpha values and 20 lambda values for each
)

wbf.modeltraining.eng19$resample %>% filter(is.na(Rsquared))

# Best tuning parameter
(wbf.stats.eng19 <- get_best_result(wbf.modeltraining.eng19)[,1:4])

# Coefficients
wbf.coef.eng19 <- coef(wbf.modeltraining.eng19$finalModel, 
                       wbf.modeltraining.eng19$bestTune$lambda, 
                       wbf.modeltraining.eng19$bestTune$alpha, exact = F)

wbf.coef.nonzero.eng19 <- wbf.coef.eng19[which(wbf.coef.eng19 != 0)]

(wbf.coef.data.eng19 <- data.frame(
  Variables = wbf.coef.eng19@Dimnames[[1]][which(wbf.coef.eng19 != 0)],
  Coefficients = wbf.coef.eng19[which(wbf.coef.eng19 != 0)]
))

# Make predictions on the test data
wbf.test.eng19 <- model.matrix(Eng19 ~ Gender + Age.at.Test 
                               + N.Worry + N.Intensity + N.Interpretation + N.Rebound.time
                               + E.Warmth + E.Sociability + E.Activity.mode 
                               + E.Taking.charge + E.Trust + E.Tact
                               + O.Imagination + O.Complexity + O.Change + O.Scope
                               + A.Others.needs + A.Agreement + A.Humility + A.Reserve
                               + C.Perfectionism + C.Organization + C.Drive 
                               + C.Concentration + C.Methodicalness,
                               data = wbf.test.data.eng19)[,-1] #remove intercept
wbf.predictions.eng19 <- predict(wbf.modeltraining.eng19, newdata = wbf.test.data.eng19) 

# Statistics of Elastic Net
(wbf.stats.predict.eng19 <- data.frame(
  Alpha = get_best_result(wbf.modeltraining.eng19)[,1],
  Lambda = get_best_result(wbf.modeltraining.eng19)[,2],
  RMSE = RMSE(wbf.predictions.eng19, wbf.test.data.eng19$Engagement2017),
  Rsquare = R2(wbf.predictions.eng19, wbf.test.data.eng19$Engagement2017)
))

### Engagement 2019 - Agility ----
# 80% of sample for training with 20 folds, repeat 3 times
set.seed(001)
wbf.training.eng19.A <- createDataPartition(
  y = wbf.naomit.data$Eng19.Agility,
  p = 0.8, list = FALSE
)

wbf.train.data.eng19.A  <- wbf.naomit.data[wbf.training.eng19.A, ]
nrow(wbf.train.data.eng19.A)
wbf.test.data.eng19.A <- wbf.naomit.data[-wbf.training.eng19.A, ]
nrow(wbf.test.data.eng19.A)


wbf.modeltraining.eng19.A <- train(Eng19.Agility ~ Gender + Age.at.Test 
                                   + N.Worry + N.Intensity + N.Interpretation + N.Rebound.time
                                   + E.Warmth + E.Sociability + E.Activity.mode 
                                   + E.Taking.charge + E.Trust + E.Tact
                                   + O.Imagination + O.Complexity + O.Change + O.Scope
                                   + A.Others.needs + A.Agreement + A.Humility + A.Reserve
                                   + C.Perfectionism + C.Organization + C.Drive 
                                   + C.Concentration + C.Methodicalness,
                                   data = wbf.train.data.eng19.A, method = "glmnet",
                                   trControl = trainControl("repeatedcv", 
                                                            number = 20, repeats = 3), 
                                   #Set cross-validation to be 20 fold, repeat 3 times
                                   tuneLength = 20  #Search 20 alpha values and 20 lambda values for each
)

wbf.modeltraining.eng19.A$resample %>% filter(is.na(Rsquared))

# Best tuning parameter
(wbf.stats.eng19.A <- get_best_result(wbf.modeltraining.eng19.A)[,1:4])

# Coefficients
wbf.coef.eng19.A <- coef(wbf.modeltraining.eng19.A$finalModel, 
                         wbf.modeltraining.eng19.A$bestTune$lambda, 
                         wbf.modeltraining.eng19.A$bestTune$alpha, exact = F)

wbf.coef.nonzero.eng19.A <- wbf.coef.eng19.A[which(wbf.coef.eng19.A != 0)]

(wbf.coef.data.eng19.A <- data.frame(
  Variables = wbf.coef.eng19.A@Dimnames[[1]][which(wbf.coef.eng19.A != 0)],
  Coefficients = wbf.coef.eng19.A[which(wbf.coef.eng19.A != 0)]
))

# Make predictions on the test data
wbf.test.eng19.A <- model.matrix(Eng19.Agility ~ Gender + Age.at.Test 
                                 + N.Worry + N.Intensity + N.Interpretation + N.Rebound.time
                                 + E.Warmth + E.Sociability + E.Activity.mode 
                                 + E.Taking.charge + E.Trust + E.Tact
                                 + O.Imagination + O.Complexity + O.Change + O.Scope
                                 + A.Others.needs + A.Agreement + A.Humility + A.Reserve
                                 + C.Perfectionism + C.Organization + C.Drive 
                                 + C.Concentration + C.Methodicalness,
                                 data = wbf.test.data.eng19.A)[,-1] #remove intercept
wbf.predictions.eng19.A <- predict(wbf.modeltraining.eng19.A, newdata = wbf.test.data.eng19.A) 

# Statistics of Elastic Net
(wbf.stats.predict.eng19.A <- data.frame(
  Alpha = get_best_result(wbf.modeltraining.eng19.A)[,1],
  Lambda = get_best_result(wbf.modeltraining.eng19.A)[,2],
  RMSE = RMSE(wbf.predictions.eng19.A, wbf.test.data.eng19.A$Engagement2017),
  Rsquare = R2(wbf.predictions.eng19.A, wbf.test.data.eng19.A$Engagement2017)
))


### Engagement 2019 - Company Environment ----
# 80% of sample for training with 20 folds, repeat 3 times
set.seed(001)
wbf.training.eng19.CE <- createDataPartition(
  y = wbf.naomit.data$Eng19.Com.Env,
  p = 0.8, list = FALSE
)

wbf.train.data.eng19.CE  <- wbf.naomit.data[wbf.training.eng19.CE, ]
nrow(wbf.train.data.eng19.CE)
wbf.test.data.eng19.CE <- wbf.naomit.data[-wbf.training.eng19.CE, ]
nrow(wbf.test.data.eng19.CE)


wbf.modeltraining.eng19.CE <- train(Eng19.Com.Env ~ Gender + Age.at.Test 
                                    + N.Worry + N.Intensity + N.Interpretation + N.Rebound.time
                                    + E.Warmth + E.Sociability + E.Activity.mode 
                                    + E.Taking.charge + E.Trust + E.Tact
                                    + O.Imagination + O.Complexity + O.Change + O.Scope
                                    + A.Others.needs + A.Agreement + A.Humility + A.Reserve
                                    + C.Perfectionism + C.Organization + C.Drive 
                                    + C.Concentration + C.Methodicalness,
                                    data = wbf.train.data.eng19.CE, method = "glmnet",
                                    trControl = trainControl("cv", 
                                                             number = 20), 
                                    #Set cross-validation to be 20 fold
                                    tuneLength = 20  #Search 20 alpha values and 20 lambda values for each
)

wbf.modeltraining.eng19.CE$resample %>% filter(is.na(Rsquared))

# Best tuning parameter
(wbf.stats.eng19.CE <- get_best_result(wbf.modeltraining.eng19.CE)[,1:4])

# Coefficients
wbf.coef.eng19.CE <- coef(wbf.modeltraining.eng19.CE$finalModel, 
                          wbf.modeltraining.eng19.CE$bestTune$lambda, 
                          wbf.modeltraining.eng19.CE$bestTune$alpha, exact = F)

wbf.coef.nonzero.eng19.CE <- wbf.coef.eng19.CE[which(wbf.coef.eng19.CE != 0)]

(wbf.coef.data.eng19.CE <- data.frame(
  Variables = wbf.coef.eng19.CE@Dimnames[[1]][which(wbf.coef.eng19.CE != 0)],
  Coefficients = wbf.coef.eng19.CE[which(wbf.coef.eng19.CE != 0)]
))

# Make predictions on the test data
wbf.test.eng19.CE <- model.matrix(Eng19.Com.Env ~ Gender + Age.at.Test 
                                  + N.Worry + N.Intensity + N.Interpretation + N.Rebound.time
                                  + E.Warmth + E.Sociability + E.Activity.mode 
                                  + E.Taking.charge + E.Trust + E.Tact
                                  + O.Imagination + O.Complexity + O.Change + O.Scope
                                  + A.Others.needs + A.Agreement + A.Humility + A.Reserve
                                  + C.Perfectionism + C.Organization + C.Drive 
                                  + C.Concentration + C.Methodicalness,
                                  data = wbf.test.data.eng19.CE)[,-1] #remove intercept
wbf.predictions.eng19.CE <- predict(wbf.modeltraining.eng19.CE, newdata = wbf.test.data.eng19.CE) 

# Statistics of Elastic Net
(wbf.stats.predict.eng19.CE <- data.frame(
  Alpha = get_best_result(wbf.modeltraining.eng19.CE)[,1],
  Lambda = get_best_result(wbf.modeltraining.eng19.CE)[,2],
  RMSE = RMSE(wbf.predictions.eng19.CE, wbf.test.data.eng19.CE$Engagement2017),
  Rsquare = R2(wbf.predictions.eng19.CE, wbf.test.data.eng19.CE$Engagement2017)
))