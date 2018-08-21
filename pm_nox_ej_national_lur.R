# 08/20 adding last bit of qing's EJ paper
library(tidyverse)

national_id <- read_csv('pm nox ej/national_to_qing_paper_final-bit/uwc15345187995723739195db35f2577a4f64e1755b96040.csv')

city_id <- read_csv("pm nox ej/demog_UFP_just_pit.csv")

# separate pm and nox
national_pm <- national_id %>% filter(pollutant == 'pm25') %>% select(block_group = fips, pm25 = pred_wght)
national_nox <- national_id %>% filter(pollutant == 'no2') %>% select(block_group = fips, no2 = pred_wght)

# inner join them, note here 0 block will be left out (national lur does not account for zero population blocks)

qing_ej_pm_no2 <- city_id %>% inner_join(national_pm) %>% inner_join(national_nox) 

write_csv(qing_ej_pm_no2, "qing_ej_pm_no2.csv")
