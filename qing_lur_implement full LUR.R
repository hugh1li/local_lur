# dep_col in LUR function  
# not 298, coz duplicates removal code change order already

source('~/Box Sync/from_dropbox/ACE hugh/Github_CAPS_spatial/make_lur.R', echo=TRUE)

library(tidyverse)
library(car)
library(ape)
library(DAAG)

LUR_input <- read_csv("LUR_input.csv")
LUR_input_01 <- LUR_input %>% select(-ID) 
LUR_input_02 = LUR_input_01[!duplicated(lapply(LUR_input_01, summary))]
zero_filter = LUR_input_02 %>% map_dbl(~sum(.x == 0)/nrow(LUR_input_02))
filter_25  = LUR_input_02 %>% map_dbl(~sum(.x == 25)/nrow(LUR_input_02))
LUR_input_f = LUR_input_02[zero_filter < 0.5 & filter_25 < 0.5]

LUR_input_f = LUR_input_f %>% mutate(chi = 1 - chi) # changing to 1 minus here
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
COA <- make_lur(dat1 = LUR_input_f, response = "COA", dep_col = 262)

# validation 
COA_lm <- lm(formula("COA ~  + PointDe_Rest_100meters + Elevation"), sx)

# adj 0.68
plot(COA_lm, which = 4)
car::vif(COA_lm)

# moran's I
# todo

# LOOCV R2
loocv_COA <- cv.lm(COA_lm$model, COA_lm, m=72, legend.pos = "topright")
cor(loocv_COA$COA,loocv_COA$cvpred)**2
0.67

# 3 fold valdation
fold3_COA <- cv.lm(COA_lm$model, COA_lm, m=3, legend.pos = "topright")
cor(fold3_COA$COA, fold3_COA$cvpred)**2
0.63
# 10 fold
fold10_COA <- cv.lm(COA_lm$model, COA_lm, m=10, legend.pos = "topright")
cor(fold10_COA$COA, fold10_COA$cvpred)**2
0.67



# mean studentized prediction residuals (sd used n-1)
M_COA <-rstudent(COA_lm)
mean(M_COA)
0.00168
# root mean square of studentized
sqrt(mean(M_COA^2))
1.01





# COA no elevation -----------------------------------------------------
COA_unwanted <- 'Elevation'
COA <- make_lur(dat1 = LUR_input_f, response = "COA", dep_col = 262, exclude =  COA_unwanted)

COA_lm <- lm(formula("COA ~  + PointDe_Rest_100meters + LUCOMM1000"), sx)
summary(COA_lm)
0.69, adj 0.68
plot(COA_lm, which = 4)
car::vif(COA_lm)

# moran's I
# todo

# 3 fold valdation
fold3_COA <- cv.lm(COA_lm$model, COA_lm, m=3, legend.pos = "topright")
cor(fold3_COA$COA, fold3_COA$cvpred)**2
0.62

# mean studentized prediction residuals (sd used n-1)
M_COA <-rstudent(COA_lm)
mean(M_COA)

# root mean square of studentized
sqrt(mean(M_COA^2))


# HOA full 3 or 2 ---------------------------------------------------------------------
# HOA <- make_lur(dat1 = LUR_input_f, response = "HOA", dep_col = 288)
HOA <- make_lur(dat1 = LUR_input_f, response = "HOA", exclude = unwanted, dep_col = 262)
# validation 

HOA_lm <- lm(formula( "HOA ~  + TRKDENALL100 + LUAGRI500 "), sx)

summary(HOA_lm)
0.72, adj 0.71

plot(HOA_lm, which = 4)
car::vif(HOA_lm)

# moran's I
# todo

# LOOCV R2
loocv_HOA <- cv.lm(HOA_lm$model, HOA_lm, m=72, legend.pos = "topright")
cor(loocv_HOA$HOA,loocv_HOA$cvpred)**2
0.69

# 3 fold
fold3_HOA <- cv.lm(HOA_lm$model, HOA_lm, m=3, legend.pos = "topright")
cor(fold3_HOA$HOA, fold3_HOA$cvpred)**2
0.68

# 10 fold
fold10_HOA <- cv.lm(HOA_lm$model, HOA_lm, m=10, legend.pos = "topright")
cor(fold10_HOA$HOA, fold3_HOA$cvpred)**2
0.68

# mean studentized prediction residuals (sd used n-1)
M_HOA<-rstudent(HOA_lm)
mean(M_HOA)
-0.0017
# root mean square of studentized
sqrt(mean(M_HOA^2))
1.01

# different R2
library(relaimpo)
calc.relimp(HOA_lm, type="lmg") 

# # calculate correlation of these vars
# HOA_cor <- sx %>% dplyr::select(TRKDENALL100, LUAGRI500, EucDistinv_PM, LUINDUS5000, ALLDIESAADT_DIS2)
# 
# cor(HOA_cor) # find alldiesaadt_dis2 and trkdenall100 highly correlated 0.68



# HOA source specific -----------------------------------------------------

unwanted_HOA <- names(dplyr::select(LUR_input_f, contains('LUAGRI')))
HOA_unwanted <- c(unwanted, unwanted_HOA)
HOA <- make_lur(dat1 = LUR_input_f, response = "HOA", exclude = HOA_unwanted, dep_col = 262)
HOA_lm <- lm(formula( "HOA ~  + TRKDENALL100 + TRKDENSMAJ1000 + PointDe_NEI_PM_Popu_5000 + LURES5000 + DISTINVALL2"), sx)




summary(HOA_lm)
0.80 and adj 0.78

plot(HOA_lm, which = 4)
car::vif(HOA_lm)

fold3_HOA <- cv.lm(HOA_lm$model, HOA_lm, m=3, legend.pos = "topright")

cor(fold3_HOA$HOA, fold3_HOA$cvpred)**2
0.76

M_HOA<-rstudent(HOA_lm)
mean(M_HOA)
0.00206
# root mean square of studentized
sqrt(mean(M_HOA^2))
1.02

# HOA if only What Albert mentioned ---------------------------------------
unwanted_HOA <- names(dplyr::select(LUR_input_f, contains('LUAGRI')))
unwanted_HOA1 <- names(dplyr::select(LUR_input_f, contains('PointDe_NEI_PM_Popu')))
unwanted_HOA2 <- names(dplyr::select(LUR_input_f, contains('Idw_PM_1')))
unwanted_HOA3 <- names(dplyr::select(LUR_input_f, contains('PointDe_NEI')))
unwanted_HOA4 <- names(dplyr::select(LUR_input_f, contains('Allo_Dist2')))
unwanted_HOA5 <- names(dplyr::select(LUR_input_f, contains('Idw')))
unwanted_HOA6 <- names(dplyr::select(LUR_input_f, contains('Allo')))
unwanted_HOA7 <- names(dplyr::select(LUR_input_f, contains('LURES')))
HOA_unwanted <- c(unwanted, unwanted_HOA, unwanted_HOA1, unwanted_HOA2, unwanted_HOA3, unwanted_HOA4, unwanted_HOA5, unwanted_HOA6, unwanted_HOA7)
HOA <- make_lur(dat1 = LUR_input_f, response = "HOA", exclude = HOA_unwanted, dep_col = 262)

HOA_lm <- lm(formula("HOA ~  + TRKDENALL100 + TRKDENSMAJ1000 + DISTINVALL2 + LURES5000 + ALLDIESAADT"), sx)
summary(HOA_lm)

# r2 portion
library(relaimpo)
calc.relimp(HOA_lm, type="lmg") 

test <- lm(formula("HOA ~  + TRKDENALL100 + TRKDENSMAJ1000 + DISTINVALL2 "), sx)
test2 <- lm(formula("HOA ~  + TRKDENALL100 + TRKDENSMAJ1000 + DISTINVALL2 + LURES5000 "), sx)
summary(test)
summary(test2)


summary(HOA_lm)
plot(HOA_lm, which = 4)
car::vif(HOA_lm)
fold3_HOA <- cv.lm(HOA_lm$model, HOA_lm, m=3, legend.pos = "topright")
cor(fold3_HOA$HOA, fold3_HOA$cvpred)**2
M_HOA<-rstudent(HOA_lm)
mean(M_HOA)
# root mean square of studentized
sqrt(mean(M_HOA^2))


# chi ---------------------------------------------------------------------

# wait, if we delete LUINDUS first
# answer: doesn't change much

chi <- make_lur(dat1 = LUR_input_f, response = "chi", dep_col = 262, exclude = unwanted) 

# validation 
"chi ~  + TRKDENSMAJ1000 + HOUSDEN300 + PointDe_NEI_30000 + LURES300"

chi_lm <- lm(formula( "chi ~  + TRKDENSMAJ1000 + HOUSDEN300 + PointDe_NEI_30000"), sx)
summary(chi_lm)

# adj 0.68
plot(chi_lm, which = 4)
car::vif(chi_lm)

# moran's I
# todo

# LOOCV R2
loocv_chi <- cv.lm(chi_lm$model, chi_lm, m=72, legend.pos = "topright")
cor(loocv_chi$chi,loocv_chi$cvpred)**2
0.60 (compared to 0.65 and adj 0.63)

# 3 fold r2 
fold3_chi <- cv.lm(chi_lm$model, chi_lm, m=3, legend.pos = "topright")
cor(fold3_chi$chi,fold3_chi$cvpred)**2

# 10 fold r2 
fold10_chi <- cv.lm(chi_lm$model, chi_lm, m=10, legend.pos = "topright")
cor(fold10_chi$chi,fold10_chi$cvpred)**2


# mean studentized prediction residuals (sd used n-1)
M_chi <- rstudent(chi_lm)
mean(M_chi)
0.00707
# root mean square of studentized
sqrt(mean(M_chi^2))
1.03

