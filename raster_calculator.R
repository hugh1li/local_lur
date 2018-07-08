# after 200m centroid extraction
Calculate and then sent back to arcgis to plot

box_200 <- read_csv("raw_data/centroid_200_before_raster_cal.csv")

-9999 meaning no data, I will treat with 0 then, though previouly I will change land use ones to be 25

# check class
temp <- map_df(box_200, class) %>% gather()

# -9999 to be 0
box_200[box_200 == -9999] <- 0 

# dist the smallest value to be the second smallest value 4.5
box_200$distall[box_200$distall == 0] <- 4.5
# and euc distinv_pm, the smallest value to be second smallest 58.5 
box_200$Eucdist_pm[box_200$Eucdist_pm == 0] <- 58.5

# calculate COA hoa and mixing state ----

box_200_01 <- box_200 %>% mutate(coa_pub = 1857.351 - 4.85 * elevation + rest100m * 7.312, hoa_pub = 839 + 67.1 * trkdenall1 - 0.0199 * AGRI500m + 1/Eucdist_pm* 4.59 * 10^4 + INDUS500x0 *4.39*10^-4 + 60.5 * alldiesaad * 1/distall/distall, mixing_state_pub = -0.473 + 0.0212 * TRKDENSMAJ + 7.91 * 10^-5 *houseden30 * 25 + 11.9 * pointde_ne) %>% select(-coa, -hoa)

# compare with qing's value
summary(box_200_01$coa_pub)
summary(box_200_01$hoa_pub)
summary(box_200_01$mixing_state_pub)
# well, some values outside qing's measurements
write_csv(box_200_01, 'qing_final_box_200_to_plot_before_changing_extremes.csv')

# limiting max and min values
box_200_02 <- box_200_01
box_200_02 <- box_200_02 %>% mutate(hoa_pub = if_else(hoa_pub < 200, 200, hoa_pub), hoa_pub = if_else(hoa_pub > 5114, 5114, hoa_pub)) %>% mutate(coa_pub = if_else(coa_pub < 0, 0, coa_pub), coa_pub = if_else(coa_pub > 3826, 3826, coa_pub)) %>% mutate(mixing_state_pub = if_else(mixing_state_pub < 0.289, 0.289, mixing_state_pub), mixing_state_pub = if_else(mixing_state_pub > 0.704, 0.704, mixing_state_pub))
  
write_csv(box_200_02, 'qing_final_box_200_to_plot.csv')

