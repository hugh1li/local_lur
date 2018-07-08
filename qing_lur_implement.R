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

LUR_input_f = LUR_input_f %>% mutate(chi = 1 - chi)
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

# COA ---------------------------------------------------------------------
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




# HOA---------------------------------------------------------------------
# HOA <- make_lur(dat1 = LUR_input_f, response = "HOA", dep_col = 288)
HOA <- make_lur(dat1 = LUR_input_f, response = "HOA", exclude = unwanted, dep_col = 262)
# validation 

HOA_lm <- lm(formula( "HOA ~  + TRKDENALL100 + LUAGRI500 + EucDistinv_PM + LUINDUS5000 + ALLDIESAADT_DIS2"), sx)

HOA_lm <- lm(formula( "HOA ~  + TRKDENALL100 + LUAGRI500 + EucDistinv_PM + LUINDUS5000"), sx)

summary(HOA_lm)

plot(HOA_lm, which = 4)
car::vif(HOA_lm)

# moran's I
# todo

# LOOCV R2
loocv_HOA <- cv.lm(HOA_lm$model, HOA_lm, m=72, legend.pos = "topright")
cor(loocv_HOA$HOA,loocv_HOA$cvpred)**2
0.74

# 3 fold
fold3_HOA <- cv.lm(HOA_lm$model, HOA_lm, m=3, legend.pos = "topright")
cor(fold3_HOA$HOA, fold3_HOA$cvpred)**2
0.74

# 10 fold
fold10_HOA <- cv.lm(HOA_lm$model, HOA_lm, m=10, legend.pos = "topright")
cor(fold10_HOA$HOA, fold3_HOA$cvpred)**2
0.75

# mean studentized prediction residuals (sd used n-1)
M_HOA<-rstudent(HOA_lm)
mean(M_HOA)
0.00168
# root mean square of studentized
sqrt(mean(M_HOA^2))
1

# different R2
library(relaimpo)
calc.relimp(HOA_lm, type="lmg") # meet with singular error, coz your vars correlate with each other
calc.relimp(HOA_lm, type = c("lmg", "last", "first", "betasq", "pratt", "genizi", "car")) # nope, cannot work out

# calculate correlation of these vars
HOA_cor <- sx %>% dplyr::select(TRKDENALL100, LUAGRI500, EucDistinv_PM, LUINDUS5000, ALLDIESAADT_DIS2)

cor(HOA_cor) # find alldiesaadt_dis2 and trkdenall100 highly correlated 0.68



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

