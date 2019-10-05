
# COA only subdivided area -------------------------------------------------------

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

LUR_input <- read_csv("LUR_input.csv")  # here chi is the right chi! (but I remove and join other chi anyway...). But 10/05/2019 found this input not the one we used for final paper. It has 72 rows, but revised final chi input only 63, HOA/COA 65...

chi_new_dep <- read_excel('revised input 10252018/new_dep.xlsx', sheet = 1) %>% rename(ID = Cell_ID) %>% dplyr::select(-"1 - chi") # here i removed the 1-chi column

OA_new_dep <- read_excel('revised input 10252018/new_dep.xlsx', sheet = 2) %>% rename(ID = '200cell id') # OA_new_dep only 65 rows, but LUR_input has 72 rows
# 10/05/2019 here "200cell" misleading, coz OA has both 200 and 1km2 id.

OA_LUR_input <- LUR_input %>% dplyr::select(-HOA, -COA, -chi) %>% inner_join(OA_new_dep)

chi_LUR_input <- LUR_input %>% dplyr::select(-HOA, -COA, -chi) %>% inner_join(chi_new_dep)

LUR_input_01 <- OA_LUR_input %>% dplyr::select(-ID)

LUR_input_02 = LUR_input_01[!duplicated(lapply(LUR_input_01, summary))]
zero_filter = LUR_input_02 %>% map_dbl(~sum(.x == 0)/nrow(LUR_input_02))
filter_25  = LUR_input_02 %>% map_dbl(~sum(.x == 25)/nrow(LUR_input_02))
LUR_input_f = LUR_input_02[zero_filter < 0.5 & filter_25 < 0.5]

# LUR_input_f = LUR_input_f %>% mutate(chi = 1 - chi) # changing to 1 minus here

# Removed all 1km2 area, leaving just subdivided
# LUR_input_f <- LUR_input_f %>% filter(ID > 300) %>% select(-ID) # if I leave ID inside the columns

LUR_input_f = LUR_input_f %>% slice(11:64)

  
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


# COA only subdivided source specifc ------------------------------------------------------
# COA_unwanted <- c(unwanted, 'PointDe_NEI_PM_1000', 'PointDe_NEI_1000', 'EucDistinv_PM', 'EucDistinv2_PM')
COA_unwanted <- c(unwanted, 'PointDe_NEI_1000', 'LUAGRI5000', 'EucDistinv_PM', 'LUVaFo1000', 'LUVaFo500', 'EucDistinv2_PM', 'HOA')
#* 10/06/19 My initial scan of the LUR code, I feel HOA should not be inside the final models...Well, i got it. -c(264:249), params command will delete all qualifying col indexes...

COA_source <- make_lur(dat1 = LUR_input_f, response = "COA", dep_col = 262, exclude = COA_unwanted) #* 10/05/2019 during NM trip, I feel weird HOA can hop into the final LUR models...

COA_source$formula
COA_source$summary

COA_lm_source <- lm(formula("COA ~  + PointDe_Rest_100meters + RDMAJ1000 + LURES100"), sx)
# partial r2 

calc.relimp(COA_lm_source, type="lmg")


fold10_COA <- cv.lm(COA_lm_source$model, COA_lm_source, m=10, legend.pos = "topright")
cor(fold10_COA$COA, fold10_COA$cvpred)**2
0.55

fold3_COA <- cv.lm(COA_lm_source$model, COA_lm_source, m=3, legend.pos = "topright")
cor(fold3_COA$COA, fold3_COA$cvpred)**2

# RMSE and MAE
rmse(COA_lm_source$model$COA, COA_lm_source$fitted.values)

mean(abs(COA_lm_source$residuals))
