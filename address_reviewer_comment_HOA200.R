#* I might just use the 1st address_reviewer_comment code for the HOA....

unwanted_HOA <- names(dplyr::select(LUR_input_f, contains('LUAGRI')))
HOA_unwanted <- c(unwanted_HOA, 'PointDe_NEI_PM_Popu_30000', 'LUUtTr5000', 'PointDe_NEI_PM_Popu_15000', 'PointDe_NEI_30000', 'PointDe_NEI_PM_7500', 'PointDe_NEI_PM_30000', 'PointDe_NEI_7500', 'HOUSDEN100', 'LUVaFo500')

# HOA_source <- make_lur(dat1 = LUR_input_f, response = "HOA", dep_col = 262, exclude = HOA_unwanted)  # error for subscript out of bound
#* I figure out why. Coz I sliced data frames....
HOA_input = LUR_input_f[!duplicated(lapply(LUR_input_f, summary))]
names(HOA_input)

HOA_source <- make_lur(dat1 = HOA_input, response = "HOA", dep_col = 248, exclude = HOA_unwanted) 

HOA_source$formula
HOA_source$summary

HOA_lm <- lm(formula("HOA ~  + VEHDENALL100 + ALLDIESAADT_DIS2 "), sx)

summary(HOA_lm)

plot(HOA_lm, which = 4)
car::vif(HOA_lm)

# calc.relimp(HOA_lm, type="lmg") # what package gives calc.relimp?

fold10_HOA <- cv.lm(HOA_lm$model, HOA_lm, m=10, legend.pos = "topright")
cor(fold10_HOA$HOA, fold10_HOA$cvpred)**2
0.43

fold3_HOA <- cv.lm(HOA_lm$model, HOA_lm, m=3, legend.pos = "topright")
cor(fold3_HOA$HOA, fold3_HOA$cvpred)**2

# RMSE and MAE
rmse(HOA_lm$model$HOA, HOA_lm$fitted.values)

mean(abs(HOA_lm$residuals))

# partial R2
library(relaimpo)
calc.relimp(HOA_lm, type="lmg") 
