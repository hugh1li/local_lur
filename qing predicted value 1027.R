# I think for provat's stuff I already did the multiplications...

# check if qing's new formula be applied using the table I sent to provat
library(tidyverse)

base <- read_csv('qing_predicted1027/provat_0920.csv', col_types = cols(.default = col_double())) #ok, no polygon200 interger parse error and miss two problems.

# so i think provat_0920 already solved the problem of some vars needs multiplication
HOA_full = -543 + 25 * VEHDENALL100 + 1.49e-3 * LUUtTr5000
HOA_source = 1577.24 + 21.48 * VEHDENALL100 + 321.5 * ALLDIESAADT_DIS2
COA_full = -734 + 11.3 * PointDe_Rest_100meters + 2.03e5 * EucDistinv_PM + 0.148 * POPDEN1000 + 3.61e-2 *RDMAJ1000
COA_source = -717.793 + 12.8734 * PointDe_Rest_100meters + 0.0505 * RDMAJ1000 + 0.1633 * POPDEN1000
1-chi = -0.304 + 5.77e-6*RDMAJ1000 + HOUSDEN300 * 7.59e-5 + 9.44 * PointDe_NEI_30000

# add PointDe_NEI_3000 to the base
with_PointDe_NEI <- read_csv("raw_data/centroid_200_before_raster_cal.csv")%>% dplyr::select(Polygon200ID  = PageNumber, Eucdist_pm, PointDe_NEI_30000 = pointde_ne)
summary(with_PointDe_NEI$Eucdist_pm) 

# get the final version with all covars
final <- base %>% inner_join(with_PointDe_NEI) %>% mutate(EucDistinv_PM = 1/Eucdist_pm) 

# now go the raster calculator and do your thing----

box_200 <- final 

# calculate COA hoa and mixing state ----

box_200_01 <- box_200 %>% mutate(coa_full = -734 + 11.3 * PointDe_Rest_100meters + 2.03e5 * EucDistinv_PM + 0.148 * POPDEN1000 + 3.61e-2 *RDMAJ1000
, coa_source = -717.793 + 12.8734 * PointDe_Rest_100meters + 0.0505 * RDMAJ1000 + 0.1633 * POPDEN1000,  hoa_full = -543 + 25 * VEHDENALL100 + 1.49e-3 * LUUtTr5000, hoa_source = 1577.24 + 21.48 * VEHDENALL100 + 321.5 * ALLDIESAADT_DIS2 , mixing_state_oppo = -0.304 + 5.77e-6*RDMAJ1000 + HOUSDEN300 * 7.59e-5 + 9.44 * PointDe_NEI_30000) 


# compare with qing's value
summary(box_200_01$coa_full)
summary(box_200_01$coa_source)
summary(box_200_01$hoa_full)
summary(box_200_01$hoa_source)
summary(box_200_01$mixing_state_oppo) # still 1 minus mixing state here

# well some values outside qing's range....
box_200_01_f <- box_200_01 %>% dplyr::select(Polygon200ID, long, lat, hoa_full, hoa_source, coa_full, coa_source, mixing_state_oppo)
  
write_csv(box_200_01_f, 'qing_102718new-prediction.csv')


# 011419 1-chi prediction -------------------------------------------------
# start from box_200 <- final
box_200_011419 <- box_200 %>% mutate(OneMinusChi = 3.981039e-01 + 2.805095e-03 * PointDe_Rest_1000meters + 5.341151e-06 *  POPDEN1000 - 5.735680e-08*LUVaFo500)
box_200_011419 %>% dplyr::select(Polygon200ID, long, lat, OneMinusChi) %>% write_csv('qing_011419_OneMinusChi.csv')

summary(box_200_011419$OneMinusChi)
