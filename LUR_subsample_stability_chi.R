# note 
# don't trust any numbers in this file

library(Metrics)
library(readxl)
# dep_col in LUR function  
# not 298, coz duplicates removal code change order already

source('~/Box Sync/from_dropbox/ACE hugh/Github_CAPS_spatial/make_lur.R', echo=TRUE)
source('~/Box Sync/from_dropbox/ACE hugh/ACE R/ACE_LUR/local_lur/lur_full_set_dev_validation.R', echo=TRUE)

library(tidyverse)
library(car)
library(ape)
library(DAAG)

LUR_input <- read_csv("LUR_input.csv") # here chi is the right chi! (but I remove and join other chi anyway...)

chi_new_dep <- read_excel('revised input 10252018/new_dep.xlsx', sheet = 1) %>% rename(ID = Cell_ID) %>% dplyr::select(-"1 - chi") # here i removed the 1-chi column

chi_LUR_input <- LUR_input %>% dplyr::select(-HOA, -COA, -chi) %>% inner_join(chi_new_dep)


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


# mixing state ------------------------------------------------------------
chi_models <- LUR_full_set(source_data = LUR_input_f, n_rows = 42, dep_col = 262, response = 'chi', exclude = unwanted)

check <- lm(formula("xxxx"), COA_models$data)
summary(check)
plot(check, which = 4)
car::vif(check)

#sim 2
check <- lm(formula( "chi ~  + RDMAJ1000 + LURES300 + PointDe_NEI_PM_Popu_30000  "), chi_models$data)
summary(check)
plot(check, which = 4)
car::vif(check)

#sim 3
check <- lm(formula("chi ~  + TRKDENSMAJ1000 + LURES300 + PointDe_NEI_PM_Popu_7500 + RDMAJ100 "), chi_models$data)
summary(check)
plot(check, which = 4)
car::vif(check)

# sim 8
"chi ~  + RDMAJ1000 + LURES100 + RDMAJ25"
check <- lm(formula("chi ~  + RDMAJ1000 + LURES100"), chi_models$data)
summary(check)
plot(check, which = 4)
car::vif(check)


# choose no.1 to draw -----------------------------------------------------
check <- lm(formula( "chi ~  + PointDe_Rest_1000meters + POPDEN1000 + LUVaFo500"), sx)
summary(check)
# the formula
3.981039e-01 + 2.805095e-03 * PointDe_Rest_1000meters + 5.341151e-06 *  POPDEN1000 
+ 5.735680e-08  * LUVaFo500 # note here you don't need to put negative sign in LuVaFo500


# mixing state partial R2 -------------------------------------------------

check <- lm(formula(  "chi ~  + RDMAJ1000 + PointDe_NEI_PM_Popu_15000 + PointDe_Rest_500meters + LURES100"), sx)
summary(check)
library(relaimpo)
calc.relimp(check, type="lmg", rela = TRUE)

 
