# rm all vars

LUR_input_01 <- chi_LUR_input %>% dplyr::select(-ID)

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

# chi ---------------------------------------------------------------------

# wait, if we delete LUINDUS first
# answer: doesn't change much

chi <- make_lur(dat1 = LUR_input_f, response = "chi", dep_col = 262, exclude = unwanted) 
chi$formula
chi$summary

# validation 
"chi ~  + RDMAJ1000 + HOUSDEN300 + PointDe_NEI_30000 + LURES100"

chi_lm <- lm(formula( "chi ~  + RDMAJ1000 + HOUSDEN300 + PointDe_NEI_30000"), sx)
summary(chi_lm)

plot(chi_lm, which = 4)
car::vif(chi_lm)

# moran's I
# todo

# LOOCV R2
loocv_chi <- cv.lm(chi_lm$model, chi_lm, m=62, legend.pos = "topright")
cor(loocv_chi$chi,loocv_chi$cvpred)**2


# 3 fold r2 
fold3_chi <- cv.lm(chi_lm$model, chi_lm, m=3, legend.pos = "topright")
cor(fold3_chi$chi,fold3_chi$cvpred)**2

# 10 fold r2 
fold10_chi <- cv.lm(chi_lm$model, chi_lm, m=10, legend.pos = "topright")
cor(fold10_chi$chi,fold10_chi$cvpred)**2


# mean studentized prediction residuals (sd used n-1)
M_chi <- rstudent(chi_lm)
mean(M_chi)

# root mean square of studentized
sqrt(mean(M_chi^2))



# RMSE and MAE use 1 minus here
rmse(chi_lm$model$chi, chi_lm$fitted.values)
0.06

mean(abs(chi_lm$residuals))
0.05

# partial r2
library(relaimpo)
calc.relimp(chi_lm, type="lmg") 
