# note 
# don't trust any numbers in this file

library(Metrics)
library(readxl)
# dep_col in LUR function  
# not 298, coz duplicates removal code change order already

source('~/Box Sync/from_dropbox/ACE hugh/Github_CAPS_spatial/make_lur.R', echo=TRUE)

library(tidyverse)
library(car)
library(ape)
library(DAAG)

LUR_input <- read_csv("LUR_input.csv") # here chi is the right chi! (but I remove and join other chi anyway...)

chi_new_dep <- read_excel('revised input 10252018/new_dep.xlsx', sheet = 1) %>% rename(ID = Cell_ID) %>% dplyr::select(-"1 - chi")

OA_new_dep <- read_excel('revised input 10252018/new_dep.xlsx', sheet = 2) %>% rename(ID = '200cell id')

OA_LUR_input <- LUR_input %>% dplyr::select(-HOA, -COA, -chi) %>% inner_join(OA_new_dep)

chi_LUR_input <- LUR_input %>% dplyr::select(-HOA, -COA, -chi) %>% inner_join(chi_new_dep)
  
LUR_input_01 <- OA_LUR_input %>% dplyr::select(-ID)

LUR_input_02 = LUR_input_01[!duplicated(lapply(LUR_input_01, summary))]
zero_filter = LUR_input_02 %>% map_dbl(~sum(.x == 0)/nrow(LUR_input_02))
filter_25  = LUR_input_02 %>% map_dbl(~sum(.x == 25)/nrow(LUR_input_02))
LUR_input_f = LUR_input_02[zero_filter < 0.5 & filter_25 < 0.5]

# LUR_input_f = LUR_input_f %>% mutate(chi = 1 - chi) # changing to 1 minus here

ignore_list <- names(LUR_input_f)[1:262]

sx <- LUR_input_f

# remove some point source vars, like Sb, Cr, As, Cl, Co, Ni
unwanted1 <- names(dplyr::select(LUR_input_f, contains('Sb')))
unwanted2 <- names(dplyr::select(LUR_input_f, contains('Cr')))
unwanted3 <- names(dplyr::select(LUR_input_f, contains('As')))
unwanted4 <- names(dplyr::select(LUR_input_f, contains('Cl')))
unwanted5 <- names(dplyr::select(LUR_input_f, contains('Co')))
unwanted5_01 <- unwanted5[unwanted5 %>% stringr::str_detect('COMM') == FALSE]
unwanted6 <- names(dplyr::select(LUR_input_f, contains('Ni')))

unwanted <- c(unwanted1, unwanted2, unwanted3, unwanted4, unwanted5_01, unwanted6)

# COA full ---------------------------------------------------------------------
# not 298, coz duplicates removal code change order already
COA_unwanted <- c(unwanted, 'PointDe_NEI_PM_1000', 'PointDe_NEI_1000')
COA_full <- make_lur(dat1 = LUR_input_f, response = "COA", dep_col = 262, exclude = COA_unwanted)

COA_full$formula
COA_full$summary
# validation 
COA_lm_full <- lm(formula("COA ~  + PointDe_Rest_100meters + EucDistinv_PM + POPDEN1000 + RDMAJ1000"), sx)

plot(COA_lm_full, which = 4)
car::vif(COA_lm_full)

library(relaimpo)
calc.relimp(COA_lm_full, type="lmg") 
# moran's I
# todo

# LOOCV R2
loocv_COA <- cv.lm(COA_lm_full$model, COA_lm_full, m=64, legend.pos = "topright")
cor(loocv_COA$COA,loocv_COA$cvpred)**2


# 3 fold valdation
fold3_COA <- cv.lm(COA_lm_full$model, COA_lm_full, m=3, legend.pos = "topright")
cor(fold3_COA$COA, fold3_COA$cvpred)**2

# 10 fold
fold10_COA <- cv.lm(COA_lm_full$model, COA_lm_full, m=10, legend.pos = "topright")
cor(fold10_COA$COA, fold10_COA$cvpred)**2


# mean studentized prediction residuals (sd used n-1)
M_COA <-rstudent(COA_lm_full)
mean(M_COA)
0.00168
# root mean square of studentized
sqrt(mean(M_COA^2))
1.01

# RMSE and MAE
rmse(COA_lm_full$model$COA, COA_lm_full$fitted.values)

mean(abs(COA_lm_full$residuals))


# COA source specifc ------------------------------------------------------
COA_unwanted <- c(unwanted, 'PointDe_NEI_PM_1000', 'PointDe_NEI_1000', 'EucDistinv_PM', 'EucDistinv2_PM')

# then steal the upper one for more stats
COA_lm_source <- lm(formula("COA ~  + PointDe_Rest_100meters + RDMAJ1000 + POPDEN1000"), sx)
# partial r2 

calc.relimp(COA_lm_source, type="lmg")

# COA no elevation -----------------------------------------------------
COA_unwanted <- 'Elevation'
COA_source <- make_lur(dat1 = LUR_input_f, response = "COA", dep_col = 262, exclude =  COA_unwanted)

# COA_source here also means no elevation... COZ coa full and coa source the same

COA_lm_source <- lm(formula("COA ~  + PointDe_Rest_100meters + LUCOMM1000"), sx)

summary(COA_lm_source)
0.69, adj 0.68
plot(COA_lm_source, which = 4)
car::vif(COA_lm_source)

# moran's I
# todo

# 3 fold valdation
fold3_COA <- cv.lm(COA_lm_source$model, COA_lm_source, m=3, legend.pos = "topright")
cor(fold3_COA$COA, fold3_COA$cvpred)**2
0.63

# mean studentized prediction residuals (sd used n-1)
M_COA <-rstudent(COA_lm_source)
mean(M_COA)
0.00317

# root mean square of studentized
sqrt(mean(M_COA^2))

# RMSE and MAE
rmse(COA_lm_source$model$COA, COA_lm_source$fitted.values)
587
mean(abs(COA_lm_source$residuals))
474



# COA just restaurant density ---------------------------------------------

COA_solo <- lm(formula("COA ~  + PointDe_Rest_100meters "), sx)

summary(COA_solo)
0.66 or 0.65

rmse(COA_solo$model$COA, COA_solo$fitted.values)
619
mean(abs(COA_solo$residuals))
500

# HOA full use all---------------------------------------------------------------------
# HOA <- make_lur(dat1 = LUR_input_f, response = "HOA", dep_col = 288)
unwanted
HOA_unwanted <- c(unwanted, 'PointDe_NEI_PM_Popu_30000') # I really don't think the 30k var should be inside...

HOA_full <- make_lur(dat1 = LUR_input_f, response = "HOA", exclude = HOA_unwanted, dep_col = 262)
# validation 

HOA_lm <- lm(formula("HOA ~  + VEHDENALL100 + LUUtTr5000"), sx)

summary(HOA_lm)
0.60, adj 0.58

plot(HOA_lm, which = 4)
car::vif(HOA_lm)

# moran's I
# todo

# LOOCV R2
HOA_lm_full <- HOA_lm
loocv_HOA <- cv.lm(HOA_lm_full$model, HOA_lm_full, m=64, legend.pos = "topright")
cor(loocv_HOA$HOA,loocv_HOA$cvpred)**2
0.54

# 3 fold
fold3_HOA <- cv.lm(HOA_lm_full$model, HOA_lm_full, m=3, legend.pos = "topright")
cor(fold3_HOA$HOA, fold3_HOA$cvpred)**2
0.47

# 10 fold
fold10_HOA <- cv.lm(HOA_lm_full$model, HOA_lm_full, m=10, legend.pos = "topright")
cor(fold10_HOA$HOA, fold3_HOA$cvpred)**2


# mean studentized prediction residuals (sd used n-1)
M_HOA<-rstudent(HOA_lm_full)
mean(M_HOA)

# root mean square of studentized
sqrt(mean(M_HOA^2))


# different R2
library(relaimpo)
calc.relimp(HOA_lm_full, type="lmg") 

# RMSE and MAE
rmse(HOA_lm_full$model$HOA, HOA_lm_full$fitted.values)

mean(abs(HOA_lm_full$residuals))



# # calculate correlation of these vars
# HOA_cor <- sx %>% dplyr::select(TRKDENALL100, LUAGRI500, EucDistinv_PM, LUINDUS5000, ALLDIESAADT_DIS2)
# 
# cor(HOA_cor) # find alldiesaadt_dis2 and trkdenall100 highly correlated 0.68


# nope HOA source specific -----------------------------------------------------
unwanted_HOA <- names(dplyr::select(LUR_input_f, contains('LUAGRI')))
HOA_unwanted <- c(unwanted, unwanted_HOA, 'PointDe_NEI_PM_Popu_30000', 'LUUtTr5000', 'PointDe_NEI_PM_Popu_15000', 'PointDe_NEI_30000', 'PointDe_NEI_PM_7500', 'PointDe_NEI_PM_30000', 'PointDe_NEI_7500', 'HOUSDEN100')
HOA_source <- make_lur(dat1 = LUR_input_f, response = "HOA", exclude = HOA_unwanted, dep_col = 262)

HOA_source$formula

HOA_lm <- lm(formula("HOA ~  + VEHDENALL100 + ALLDIESAADT_DIS2 "), sx)

summary(HOA_lm)


plot(HOA_lm, which = 4)
car::vif(HOA_lm)

fold3_HOA <- cv.lm(HOA_lm$model, HOA_lm, m=3, legend.pos = "topright")

cor(fold3_HOA$HOA, fold3_HOA$cvpred)**2

M_HOA<-rstudent(HOA_lm)
mean(M_HOA)

# root mean square of studentized
sqrt(mean(M_HOA^2))

# loocv
loocv_HOA <- cv.lm(HOA_lm_source$model, HOA_lm_source, m=64, legend.pos = "topright")
cor(loocv_HOA$HOA,loocv_HOA$cvpred)**2



# replaced by upper: HOA if only What Albert mentioned ---------------------------------------
unwanted_HOA <- names(dplyr::select(LUR_input_f, contains('LUAGRI')))
unwanted_HOA1 <- names(dplyr::select(LUR_input_f, contains('PointDe_NEI_PM_Popu')))
unwanted_HOA2 <- names(dplyr::select(LUR_input_f, contains('Idw_PM_1')))
unwanted_HOA3 <- names(dplyr::select(LUR_input_f, contains('PointDe_NEI')))
unwanted_HOA4 <- names(dplyr::select(LUR_input_f, contains('Allo_Dist2')))
unwanted_HOA5 <- names(dplyr::select(LUR_input_f, contains('Idw')))
unwanted_HOA6 <- names(dplyr::select(LUR_input_f, contains('Allo')))
unwanted_HOA7 <- names(dplyr::select(LUR_input_f, contains('LURES')))
# HOA_unwanted <- c(unwanted, unwanted_HOA, unwanted_HOA1, unwanted_HOA2, unwanted_HOA3, unwanted_HOA4, unwanted_HOA5, unwanted_HOA6, unwanted_HOA7)
# HOA_source <- make_lur(dat1 = LUR_input_f, response = "HOA", exclude = HOA_unwanted, dep_col = 262)
# 
# HOA_lm_source <- lm(formula("HOA ~  + TRKDENALL100 + TRKDENSMAJ1000 + DISTINVALL2"), sx)

HOA_lm_source  <- lm(formula("HOA ~  + VEHDENALL100 + ALLDIESAADT_DIS2 "), sx)


summary(HOA_lm_source)

# r2 portion
library(relaimpo)
calc.relimp(HOA_lm_source, type="lmg") 

summary(HOA_lm_source)
plot(HOA_lm_source, which = 4)
car::vif(HOA_lm_source)
fold3_HOA <- cv.lm(HOA_lm_source$model, HOA_lm_source, m=3, legend.pos = "topright")
cor(fold3_HOA$HOA, fold3_HOA$cvpred)**2

M_HOA<-rstudent(HOA_lm_source)
mean(M_HOA)
# root mean square of studentized
sqrt(mean(M_HOA^2))


# RMSE and MAE
rmse(HOA_lm_source$model$HOA, HOA_lm_source$fitted.values)

mean(abs(HOA_lm_source$residuals))




# Measured vs. predicted --------------------------------------------------
# compile here
# my final chi need to the initial one
measured_predicted <- LUR_input %>% dplyr::select(ID, HOA, COA, chi) %>% mutate(COA_full_pred = COA_lm_full$fitted.values, COA_source_pred = COA_lm_source$fitted.values, COA_solo_pred = COA_solo$fitted.values, HOA_full_pred = HOA_lm_full$fitted.values, HOA_source_pred = HOA_lm_source$fitted.values, chi_pred = chi_lm$fitted.values) %>% mutate(chi_pred = 1- chi_pred)

write_csv(measured_predicted, 'measured_predicted.csv')

