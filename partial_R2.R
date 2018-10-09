
COA_lm_full <- lm(formula("COA ~  + PointDe_Rest_100meters + Elevation"), sx)

COA_lm_source <- lm(formula("COA ~  + PointDe_Rest_100meters + LUCOMM1000"), sx)

HOA_lm_full <- lm(formula("HOA ~  + TRKDENALL100 + LUAGRI500 + EucDistinv_PM + LUINDUS5000 + ALLDIESAADT_DIS2"), sx)

temp <- lm(formula("HOA ~  + TRKDENALL100+ LUAGRI500 + EucDistinv_PM + LUINDUS5000 "), sx)
summary(temp)

library(relaimpo)
calc.relimp(HOA_lm_full, type="lmg")

HOA_lm_source <- lm(formula("HOA ~  + TRKDENALL100 + TRKDENSMAJ1000 + DISTINVALL2"), sx)

chi_lm <- lm(formula( "chi ~  + TRKDENSMAJ1000 + HOUSDEN300 + PointDe_NEI_30000"), sx)

