# Based on questions received qing thesis defense
# this file based on qing_lur_implement full LUR.R
# LUR_input_f only for COA and HOA
# note sometimes I gave up models with cook's D larger than 1. Coz removing observation is always a hard task

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

sx <- LUR_input_f

# LUR_input_f = LUR_input_f %>% mutate(chi = 1 - chi) # changing to 1 minus here

ignore_list <- names(LUR_input_f)[1:262]


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
# sample 2/3 * 64 = 42
# i just run the function again and again to get 10 models
COA_unwanted <- c(unwanted, 'PointDe_NEI_PM_1000', 'PointDe_NEI_1000')

# COA_models <- LUR_input_f %>% sample_n(42, replace = FALSE) %>%  make_lur(dat1 = ., response = "COA", dep_col = 262, exclude = COA_unwanted)
# COA_models$formula
# COA_models$summary

COA_models <- LUR_full_set(source_data = LUR_input_f, n_rows = 42, dep_col = 262, response = 'COA', exclude = COA_unwanted)

check <- lm(formula("xxxx"), COA_models$data)
summary(check)
plot(check, which = 4)
car::vif(check)

# sim 1 (number corresponds to your google sheet file)
check <- lm(formula("COA ~  + PointDe_Rest_100meters + EucDistinv_PM + POPDEN500"), COA_models$data)
summary(check)
plot(check, which = 4)
car::vif(check)

# sim 3 
check <- lm(formula("COA ~  + RDMAJ1000 + PointDe_NEI_PM_Popu_15000 + LURES1000 + PointDe_Rest_100meters "), COA_models$data)
summary(check)
plot(check, which = 4)
car::vif(check)

# sim 4
check <- lm(formula("COA ~  + PointDe_Rest_100meters + EucDistinv_PM + POPDEN1000"), COA_models$data)
summary(check)
plot(check, which = 4)
car::vif(check)




# COA partial R2 ----------------------------------------------------------

check <- lm(formula( "COA ~  + PointDe_Rest_100meters + PointDe_NEI_PM_1500 + EucDistinv_PM + PointDe_NEI_PM_Popu_15000"), sx)
summary(check)
library(relaimpo)
calc.relimp(check, type="lmg", rela = TRUE)



# HOA--------
# HOA_unwanted change slightly than the original paper 
# i don't trust these 30k buffer variables
HOA_unwanted <- c(unwanted, 'PointDe_NEI_PM_Popu_30000', 'PointDe_NEI_PM_Popu_15000', 'PointDe_NEI_30000', 'PointDe_NEI_7500', 'PointDe_NEI_PM_30000') # I really 

HOA_models <- LUR_full_set(source_data = LUR_input_f, n_rows = 42, dep_col = 262, response = 'HOA', exclude = HOA_unwanted)

check <- lm(formula(), HOA_models$data)
summary(check)
plot(check, which = 4)
car::vif(check)

# sim1
check <- lm(formula("HOA ~  + RDMAJ100 + DISTINVALL2 + TRKDENALL300 + LURES100 "), HOA_models$data)
summary(check)
plot(check, which = 4)
car::vif(check)

# sim2 
check <- lm(formula("HOA ~  + VEHDENALL100 + PointDe_Rest_300meters + ALLDIESAADT_DIS2 "), HOA_models$data)
summary(check)
plot(check, which = 4)
car::vif(check)

# sim10
check <- lm(formula("HOA ~  + VEHDENMAJ100 + LUUtTr5000 "), HOA_models$data)
summary(check)
plot(check, which = 4)
car::vif(check)

# HOA partial R2 ----------------------------------------------------------
check <- lm(formula("HOA ~  + VEHDENMAJ100 + LUUtTr5000 "), sx)
summary(check)
library(relaimpo)
calc.relimp(check, type="lmg", rela = TRUE)





