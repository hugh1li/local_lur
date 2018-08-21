# you need to use the ACS block group area as denominator

library(tidyverse)
library(googlesheets)

# read the huge file from arcgis after doing intersection
demo_data <- read_csv("demog_data/ACS_200_intersect.csv")

# select the list i want
selected_demo <- read_csv('demog_data/ACS_selected_demog.csv')

# have to clean the long name to short one, then I can select.

toMatch <- selected_demo$Short_Name
matches <- unique(grep(paste(toMatch, collapse = "|"), names(demo_data), value = TRUE))

demo_data_01 <- demo_data %>% select(PageNumber, GEOID_Data = ACS_2016_5YR_BG_42_PENNSYLVANIA_GEOID_Data, Shape_Area, matches)  


# check which variables are character
df_types <- map_df(demo_data_01, class) %>% gather()

# character columns include
df_types[df_types$value == 'logical', ]

df_types[df_types$value == 'character', ]

# need to use ACS block group area as denominator
ACS_block_group_area <- read_csv("demog_data/ACS_block_group_area.csv") 
ACS_block_group_area_01 <- ACS_block_group_area %>% rename(block_group_area = Shape_Area)

allocation_f <- demo_data_01 %>% inner_join(ACS_block_group_area_01) %>% group_by(PageNumber) %>% summarize_at(funs(sum(.*Shape_Area/block_group_area)), .vars = colnames(.)[4:195])

write_csv(allocation_f, "demog_apportioned.csv")

allocation_f_01 <- allocation_f %>% rename(asian_non_his = X03_HISPANIC_OR_LATINO_ORIGIN_B03002e6, hispanic_any_race = X03_HISPANIC_OR_LATINO_ORIGIN_B03002e12, black_non_hisp = X03_HISPANIC_OR_LATINO_ORIGIN_B03002e4, two_or_more_race = X03_HISPANIC_OR_LATINO_ORIGIN_B03002e9, white_non_his =  X03_HISPANIC_OR_LATINO_ORIGIN_B03002e3, american_indian = X03_HISPANIC_OR_LATINO_ORIGIN_B03002e5) %>% mutate(house_less20k = X19_INCOME_B19001e2 + X19_INCOME_B19001e3 + X19_INCOME_B19001e4, house_20_35k = X19_INCOME_B19001e5 + X19_INCOME_B19001e6 + X19_INCOME_B19001e7, house_35_50k = X19_INCOME_B19001e8 + X19_INCOME_B19001e9 + X19_INCOME_B19001e10, house_50_75k = X19_INCOME_B19001e11 + X19_INCOME_B19001e12 , house_more_75 = X19_INCOME_B19001e13 + X19_INCOME_B19001e14+X19_INCOME_B19001e15+X19_INCOME_B19001e16+X19_INCOME_B19001e17) %>% mutate( education_less_highschool = X15_EDUCATIONAL_ATTAINMENT_B15003e2 + X15_EDUCATIONAL_ATTAINMENT_B15003e3 + X15_EDUCATIONAL_ATTAINMENT_B15003e4 + X15_EDUCATIONAL_ATTAINMENT_B15003e5 + X15_EDUCATIONAL_ATTAINMENT_B15003e6+X15_EDUCATIONAL_ATTAINMENT_B15003e7+X15_EDUCATIONAL_ATTAINMENT_B15003e8+X15_EDUCATIONAL_ATTAINMENT_B15003e9+X15_EDUCATIONAL_ATTAINMENT_B15003e10+X15_EDUCATIONAL_ATTAINMENT_B15003e11+X15_EDUCATIONAL_ATTAINMENT_B15003e12+X15_EDUCATIONAL_ATTAINMENT_B15003e13+X15_EDUCATIONAL_ATTAINMENT_B15003e14+X15_EDUCATIONAL_ATTAINMENT_B15003e15+X15_EDUCATIONAL_ATTAINMENT_B15003e16, education_highschool = X15_EDUCATIONAL_ATTAINMENT_B15003e17 + X15_EDUCATIONAL_ATTAINMENT_B15003e18, education_some_college = X15_EDUCATIONAL_ATTAINMENT_B15003e19 + X15_EDUCATIONAL_ATTAINMENT_B15003e20 + X15_EDUCATIONAL_ATTAINMENT_B15003e21, education_college_degree = X15_EDUCATIONAL_ATTAINMENT_B15003e22, education_graduate = X15_EDUCATIONAL_ATTAINMENT_B15003e23 + X15_EDUCATIONAL_ATTAINMENT_B15003e24 + X15_EDUCATIONAL_ATTAINMENT_B15003e25) %>% mutate(age_less_5_male = X01_AGE_AND_SEX_B01001e3, age_5_17_male = X01_AGE_AND_SEX_B01001e4 + X01_AGE_AND_SEX_B01001e5 + X01_AGE_AND_SEX_B01001e6, age_18_65_male = X01_AGE_AND_SEX_B01001e7 + X01_AGE_AND_SEX_B01001e8+X01_AGE_AND_SEX_B01001e9+X01_AGE_AND_SEX_B01001e10+X01_AGE_AND_SEX_B01001e11+X01_AGE_AND_SEX_B01001e12+X01_AGE_AND_SEX_B01001e13+X01_AGE_AND_SEX_B01001e14+X01_AGE_AND_SEX_B01001e15+X01_AGE_AND_SEX_B01001e16+X01_AGE_AND_SEX_B01001e17+X01_AGE_AND_SEX_B01001e18+X01_AGE_AND_SEX_B01001e19, age_more_65_male = X01_AGE_AND_SEX_B01001e20 + X01_AGE_AND_SEX_B01001e21+X01_AGE_AND_SEX_B01001e22+X01_AGE_AND_SEX_B01001e23+X01_AGE_AND_SEX_B01001e24+X01_AGE_AND_SEX_B01001e25, age_less_5_female = X01_AGE_AND_SEX_B01001e27, age_5_17_female = X01_AGE_AND_SEX_B01001e28 + X01_AGE_AND_SEX_B01001e29 + X01_AGE_AND_SEX_B01001e30, age_18_65_female = X01_AGE_AND_SEX_B01001e31 + X01_AGE_AND_SEX_B01001e31 + X01_AGE_AND_SEX_B01001e33 + X01_AGE_AND_SEX_B01001e34 + X01_AGE_AND_SEX_B01001e35 +X01_AGE_AND_SEX_B01001e36 +X01_AGE_AND_SEX_B01001e37+X01_AGE_AND_SEX_B01001e38+X01_AGE_AND_SEX_B01001e39+X01_AGE_AND_SEX_B01001e40+X01_AGE_AND_SEX_B01001e41+X01_AGE_AND_SEX_B01001e42+X01_AGE_AND_SEX_B01001e43, age_more_65_female = X01_AGE_AND_SEX_B01001e44+X01_AGE_AND_SEX_B01001e45+X01_AGE_AND_SEX_B01001e46+X01_AGE_AND_SEX_B01001e47+X01_AGE_AND_SEX_B01001e48+X01_AGE_AND_SEX_B01001e49)

allocation_f_02 <- allocation_f_01 %>% select(PageNumber, asian_non_his, hispanic_any_race, black_non_hisp, two_or_more_race, white_non_his, american_indian, house_less20k, house_20_35k, house_35_50k, house_50_75k, house_more_75, education_less_highschool, education_highschool, education_some_college, education_college_degree, education_graduate, age_less_5_male, age_5_17_male,age_18_65_male,age_more_65_male, age_less_5_female, age_5_17_female, age_18_65_female, age_more_65_female)
  

write_csv(allocation_f_02, "demog_apportioned_transformed.csv")

# i missed 47329 - 47356 200m cells, I guess it's because of bounday stuff.

# self testing----
if sums of all rows in a specific column equal each others --> does not help

summary(allocation_f_02) # make sense the max 

# then the sum of any columns
column_sums <- map_df(allocation_f_02, sum) %>% gather()
# asian non hispanic, make sense, 4% of ACHD total population 1 million

