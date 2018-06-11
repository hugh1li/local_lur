source('~/Box Sync/from_dropbox/ACE hugh/Github_CAPS_spatial/make_lur.R', echo=TRUE)

library(tidyverse)
library(car)
library(ape)
library(DAAG)

LUR_input <- read_csv("LUR_input.csv")
LUR_input_f <- LUR_input %>% select(-ID) 

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
COA <- make_lur(dat1 = LUR_input_f, response = "COA", dep_col = 298)


# validation 

COA_lm <- lm(formula("COA ~  + PointDe_Rest_100meters + LUCOMM1000"), sx)
# adj 0.68
plot(COA_lm, which = 4)
car::vif(COA_lm)

# moran's I
# todo

# LOOCV R2
loocv_COA <- cv.lm(COA_lm$model, COA_lm, m=72, legend.pos = "topright")
cor(loocv_COA$COA,loocv_COA$cvpred)**2
0.664

# mean studentized prediction residuals (sd used n-1)
M_COA<-rstudent(COA_lm)
mean(M_COA)
0.0035
# root mean square of studentized
sqrt(mean(M_COA^2))
1.01

# wrong sign chi ---------------------------------------------------------------------

# wait, if we delete LUINDUS first
# answer: doesn't change much
chi <- make_lur(dat1 = LUR_input_f, response = "chi", dep_col = 298, exclude= c("PointDe_NEI_Ni_Popu_3000", "PointDe_NEI_Sb_Popu_10000", 'Idw_Sb_2', "PointDe_NEI_Co_Popu_10000", "PointDe_NEI_Co_Popu_10000")) 

chi <- make_lur(dat1 = LUR_input_f, response = "chi", dep_col = 298, exclude= c("LUINDUS1000", "PointDe_NEI_Ni_Popu_3000", "PointDe_NEI_Cr_Popu_3000", "PointDe_NEI_Co_Popu_10000", "PointDe_NEI_Co_Popu_7500"))



chi <- make_lur(dat1 = LUR_input_f, response = "chi", dep_col = 288, exclude=unwanted) 

# validation 
"chi ~  + LUINDUS1000 + LURES5000 + MAJAADT + LURES1000 + PointDe_NEI_PM_Popu_3000"

chi_lm <- lm(formula("chi ~  + LUINDUS1000 + LURES5000 + MAJAADT + LURES1000 + PointDe_NEI_PM_Popu_3000"), sx)
# adj 0.68
plot(chi_lm, which = 4)
car::vif(chi_lm)

# moran's I
# todo

# LOOCV R2
loocv_chi <- cv.lm(chi_lm$model, chi_lm, m=72, legend.pos = "topright")
cor(loocv_chi$chi,loocv_chi$cvpred)**2
0.54 (compared to 0.61 and adj 0.58)

# mean studentized prediction residuals (sd used n-1)
M_chi <- rstudent(chi_lm)
mean(M_chi)
0.00756
# root mean square of studentized
sqrt(mean(M_chi^2))
1.04


# HOA ---------------------------------------------------------------------
# HOA <- make_lur(dat1 = LUR_input_f, response = "HOA", dep_col = 288)
HOA <- make_lur(dat1 = LUR_input_f, response = "HOA", exclude = unwanted, dep_col = 288)
# validation 

HOA_lm <- lm(formula( "HOA ~  + TRKDENALL100 + PointDe_NEI_PM_Popu_5000 + RDALL1000 + LURES5000"), sx)

plot(HOA_lm, which = 4)
car::vif(HOA_lm)

# moran's I
# todo

# LOOCV R2
loocv_HOA <- cv.lm(HOA_lm$model, HOA_lm, m=72, legend.pos = "topright")
cor(loocv_HOA$HOA,loocv_HOA$cvpred)**2
0.71

# mean studentized prediction residuals (sd used n-1)
M_HOA<-rstudent(HOA_lm)
mean(M_HOA)
-0.00118
# root mean square of studentized
sqrt(mean(M_HOA^2))
1.01


# chi ---------------------------------------------------------------------

# wait, if we delete LUINDUS first
# answer: doesn't change much

chi <- make_lur(dat1 = LUR_input_f, response = "chi", dep_col = 288, exclude=unwanted) 

# validation 
"chi ~  + TRKDENSMAJ1000 + HOUSDEN300 + PointDe_NEI_30000 + LURES300"

chi_lm <- lm(formula("chi ~  + TRKDENSMAJ1000 + HOUSDEN300 + PointDe_NEI_30000 "), sx)
# adj 0.68
plot(chi_lm, which = 4)
car::vif(chi_lm)

# moran's I
# todo

# LOOCV R2
loocv_chi <- cv.lm(chi_lm$model, chi_lm, m=72, legend.pos = "topright")
cor(loocv_chi$chi,loocv_chi$cvpred)**2
0.59 (compared to 0.65 and adj 0.63)

# mean studentized prediction residuals (sd used n-1)
M_chi <- rstudent(chi_lm)
mean(M_chi)
0.00728
# root mean square of studentized
sqrt(mean(M_chi^2))
1.03

