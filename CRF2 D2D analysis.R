library(openxlsx)
library(dplyr)
require(pracma)
library(reshape2)
library(DTK)
library(gtools)
library(car)
library(stringr)

### Import data ----
crf_data <- read.xlsx("/Volumes/GoogleDrive/My Drive/Khue_MACBOOK/RESEARCH/Communicating research findings/Analysis/CRF2 - D2D data.xlsx", sheet = "Data") %>%
  arrange(., Group) %>%
  mutate(Participant = as.factor(Participant),
         Group       = as.factor(Group),
         Ratio       = as.factor(Ratio),
         Frame       = as.factor(Frame),
         Gender      = as.factor(Gender),
         Age         = as.numeric(Age),
         Degree  = as.numeric(Degree)) %>%
  mutate("C1_M1" = Q34_1_CRF1,  "C1_M2" = Q34_2_CRF1,  "C1_M3" = Q34_3_CRF1,  "C1_M4" = Q34_4_CRF1,
         "C1_C1" = Q35_1_CRF1,  "C1_C2" = Q35_2_CRF1,  "C1_C3" = Q35_3_CRF1,  "C1_C4" = Q35_4_CRF1,
         "C1_I1" = Q36_1_CRF1,  "C1_I2" = Q36_2_CRF1,  "C1_I3" = Q36_3_CRF1,  "C1_I4" = Q36_4_CRF1,
         "C2_M1" = Q169_1_CRF2, "C2_M2" = Q169_2_CRF2, "C2_M3" = Q169_3_CRF2, "C2_M4" = Q169_4_CRF2,
         "C2_C1" = Q170_1_CRF2, "C2_C2" = Q170_2_CRF2, "C2_C3" = Q170_3_CRF2, "C2_C4" = Q170_4_CRF2,
         "C2_I1" = Q171_1_CRF2, "C2_I2" = Q171_2_CRF2, "C2_I3" = Q171_3_CRF2, "C2_I4" = Q171_4_CRF2,
         "C3_M1" = Q173_1_CRF3, "C3_M2" = Q173_2_CRF3, "C3_M3" = Q173_3_CRF3, "C3_M4" = Q173_4_CRF3,
         "C3_C1" = Q174_1_CRF3, "C3_C2" = Q174_2_CRF3, "C3_C3" = Q174_3_CRF3, "C3_C4" = Q174_4_CRF3,
         "C3_I1" = Q175_1_CRF3, "C3_I2" = Q175_2_CRF3, "C3_I3" = Q175_3_CRF3, "C3_I4" = Q175_4_CRF3,
         "C4_M1" = Q177_1_CRF4, "C4_M2" = Q177_2_CRF4, "C4_M3" = Q177_3_CRF4, "C4_M4" = Q177_4_CRF4,
         "C4_C1" = Q178_1_CRF4, "C4_C2" = Q178_2_CRF4, "C4_C3" = Q178_3_CRF4, "C4_C4" = Q178_4_CRF4,
         "C4_I1" = Q179_1_CRF4, "C4_I2" = Q179_2_CRF4, "C4_I3" = Q179_3_CRF4, "C4_I4" = Q179_4_CRF4) %>%
  mutate(M1 = scale(rowSums(cbind(C1_M1,C2_M1,C3_M1,C4_M1), na.rm=T), center = T, scale = T),
         M2 = scale(rowSums(cbind(C1_M2,C2_M2,C3_M2,C4_M2), na.rm=T), center = T, scale = T),
         M3 = scale(rowSums(cbind(C1_M3,C2_M3,C3_M3,C4_M3), na.rm=T), center = T, scale = T),
         M4 = scale(rowSums(cbind(C1_M4,C2_M4,C3_M4,C4_M4), na.rm=T), center = T, scale = T),
         C1 = scale(rowSums(cbind(C1_C1,C2_C1,C3_C1,C4_C1), na.rm=T), center = T, scale = T),
         C2 = scale(rowSums(cbind(C1_C2,C2_C2,C3_C2,C4_C2), na.rm=T), center = T, scale = T),
         C3 = scale(rowSums(cbind(C1_C3,C2_C3,C3_C3,C4_C3), na.rm=T), center = T, scale = T),
         C4 = scale(rowSums(cbind(C1_C4,C2_C4,C3_C4,C4_C4), na.rm=T), center = T, scale = T),
         I1 = scale(rowSums(cbind(C1_I1,C2_I1,C3_I1,C4_I1), na.rm=T), center = T, scale = T),
         I2 = scale(rowSums(cbind(C1_I2,C2_I2,C3_I2,C4_I2), na.rm=T), center = T, scale = T),
         I3 = scale(rowSums(cbind(C1_I3,C2_I3,C3_I3,C4_I3), na.rm=T), center = T, scale = T),
         I4 = scale(rowSums(cbind(C1_I4,C2_I4,C3_I4,C4_I4), na.rm=T), center = T, scale = T)) %>%
  mutate(RR = (M1 + C1 + I1)/3, #Risk ratio
         OR = (M2 + C2 + I2)/3, #Odds ratio
         NF = (M3 + C3 + I3)/3, #Natural frequencies
         TR = (M4 + C4 + I4)/3) #Taylor-Russell

hist(crf_data$M1)
hist(crf_data$RR)
### Data transformantion ----
crf_datalong <- crf_data %>%
  dplyr::select(Participant, Group, Ratio, Frame,
                RR, OR, NF, TR, 
                Age, Gender, Degree) %>%
  reshape2::melt(., id.vars = c("Participant", "Group", "Ratio", "Frame", 
                                "Age", "Gender", "Degree")) %>%
  rename(Method = variable)

# M, C, I in all 1 model
crf_datalong2 <- crf_data %>%
  dplyr::select(Participant, 
                Group, Ratio, Frame,
                Age, Gender, Degree,
                M1, C1, I1, 
                M2, C2, I2,
                M3, C3, I3,
                M4, C4, I4) %>%
  reshape2::melt(., id.vars = c("Participant", "Group", "Ratio", "Frame", 
                                "Age", "Gender", "Degree")) %>%
  rename(Method = variable) 


### Correlations ----
crf_cor <- crf_data %>%
  dplyr::select(M1, C1, I1, 
                M2, C2, I2,
                M3, C3, I3,
                M4, C4, I4) %>%
  cor() %>%
  round(.,2)
crf_cor

### Factor analysis ----
require(psych)
require(GPArotation)
crf_parallel <- fa.parallel(crf_cor, fm = 'minres', fa = 'fa', n.obs = 401)
(crf_fa <- fa(crf_cor, nfactors = 4, n.obs = 401))


### Descriptives and regression with grouped method (combining M, C, I) ----

# Input: dataset = dataset to run analysis and colName = outcome variable name
# Output: (1) Mean, (2) Box plots, (3) Regression output (lm), (4) ANOVA output, (5) Post-hoc tests 

crf_analysis <- function(dataset, colName){
  # Descriptives
      box.plot1 <- boxplot(dataset[[colName]] ~ Ratio + Frame, 
                          data  = dataset,
                          ylab  = "Rating",
                          main  = "Ratio x Frame")
      box.plot2 <- boxplot(dataset[[colName]] ~ Method, 
                           data  = dataset,
                           ylab  = "Rating",
                           main  = "Interpretation method")
      box.plot3 <- boxplot(dataset[[colName]] ~ Method + Ratio, 
                           data  = dataset,
                           ylab  = "Rating",
                           main  = "Interpretation method x Ratio")
      box.plot4 <- boxplot(dataset[[colName]] ~ Method + Frame, 
                           data  = dataset,
                           ylab  = "Rating",
                           main  = "Interpretation method x Frame")
      
  # Regression 
      lm_mod <- summary(lm(dataset[[colName]] ~ Ratio + Frame + Method + Age + Degree 
                           + Ratio*Frame + Method*Ratio + Method*Frame, 
                             data = dataset))
      
      Coefficients <- as.data.frame(round(lm_mod$coef[,c("Estimate","Std. Error","Pr(>|t|)")],2)) %>%
        dplyr::rename(Coefficient    = Estimate,
                      Standard.error = "Std. Error",
                      P.value        = 'Pr(>|t|)') %>%
        dplyr::mutate(Value = as.vector(rownames(lm_mod$coef)),
                      Significance = gtools::stars.pval(P.value)) %>%
        dplyr::select(Value, Coefficient, Standard.error, P.value, Significance) 
      
      R.squared <- as.data.frame(lm_mod$r.squared) %>%
        dplyr::mutate(lm_mod$adj.r.squared) %>%
        dplyr::rename('R-squared'          = 'lm_mod$r.squared',
                      'Adjusted R-squared' = 'lm_mod$adj.r.squared') %>%
        round(.,2)

      linear.model <- list(
        Coefficients                       = Coefficients,
        R.squared                          = R.squared,
        F.stats                            = round(data.frame(lm_mod$fstatistic),2))
      
  # Interaction plots
      inter.plot1 <- dataset %>%
        ggplot() +
        aes(x = Ratio, color = Frame, group = Frame, 
              y = dataset[[colName]]) +
        stat_summary(fun.y = mean, geom = "point") +
        stat_summary(fun.y = mean, geom = "line") + 
        ylab("Rating") + theme_classic()
      inter.plot2 <- dataset %>%
        ggplot() +
        aes(x = Method, color = Ratio, group = Ratio, 
            y = dataset[[colName]]) +
        stat_summary(fun.y = mean, geom = "point") +
        stat_summary(fun.y = mean, geom = "line")+ 
        ylab("Rating") + theme_classic()
      inter.plot3 <- dataset %>%
        ggplot() +
        aes(x = Method, color = Frame, group = Frame, 
            y = dataset[[colName]]) +
        stat_summary(fun.y = mean, geom = "point") +
        stat_summary(fun.y = mean, geom = "line")+ 
        ylab("Rating") + theme_classic()
      
  # ANOVA
      aov_mod <- aov(dataset[[colName]] ~ Ratio + Frame + Method + Age + Degree 
                     + Ratio*Frame + Method*Ratio + Method*Frame, 
                     data = dataset)
      aov_sum <- car::Anova(aov_mod, type = 3)
      
      
  
  # Post-hoc tests
      tukey <- TukeyHSD(aov(dataset[[colName]] ~ Ratio + Frame + Method
                            + Ratio*Frame + Method*Ratio + Method*Frame, 
                         data = dataset))
      plot.tukey <- plot(tukey, cex.axis = 0.5)
      
      DTK_method <- DTK.test(x = dataset[[colName]], 
                             f = dataset$Method)
      DTK_ratio <- DTK.test(x = dataset[[colName]], 
                            f = dataset$Ratio)
      DTK_frame <- DTK.test(x = dataset[[colName]], 
                            f = dataset$Frame)


  # Output
      output <- list(
        histogram        = histogram,
        boxplot          = list(box.plot1, box.plot2, box.plot3, box.plot4),
        linear.model     = linear.model,
        interaction      = list(inter.plot1, inter.plot2, inter.plot3),
        ANOVA            = aov_sum,
        Tukey            = tukey,
        DTK              = list(DTK_method, DTK_ratio, DTK_frame)
      )
      # return(output)
      # break
}

(crf_aov <- crf_analysis(crf_datalong,"value"))
crf_aov$linear.model
crf_aov$interaction

### Regression with each M, C, I ----


crf_aov_item <- crf_analysis(crf_datalong2,"value")
crf_aov_item$linear.model
crf_aov_item$ANOVA

# M only
crf_datalong2_M <- crf_datalong2 %>% 
  dplyr::filter(str_detect(Method, "M")) %>%
  droplevels()

(crf_aov_M <- crf_analysis(crf_datalong2_M,"value"))
crf_aov_M$linear.model


# C only
crf_datalong2_C <- crf_datalong2 %>% 
  dplyr::filter(str_detect(Method, "C")) %>%
  droplevels()

(crf_aov_C <- crf_analysis(crf_datalong2_C,"value"))
crf_aov_C

# I only
crf_datalong2_I <- crf_datalong2 %>% 
  dplyr::filter(str_detect(Method, "I")) %>%
  droplevels()

(crf_aov_I <- crf_analysis(crf_datalong2_I,"value"))
crf_aov_I


### MANCOVA based on FA results ----

y <- cbind(crf_data$RR, crf_data$OR, crf_data$NF, crf_data$TR)
mancova_mod <- summary(manova(y ~ Ratio*Frame + Age, data = crf_data), 
                       test = "Wilks")
mancova_mod

