# you need all boxes coz you need prediction in some large 1km2 box (outside city)

# after 200m centroid extraction
Calculate and then sent back to arcgis to plot

box_200 <- read_csv("raw_data/centroid_200_before_raster_cal.csv")


# check class
temp <- map_df(box_200, class) %>% gather()

# check how many -9999 for each column
temp <- map_df(box_200, ~sum(.x == -9999)) %>% gather()
table(temp$value)

temp[temp$value == 6, 'key']

key       
<chr>     
  1 rest100m  
2 elevation  # trouble for the COA bounday, the least value is 208
3 trkdenall1
4 AGRI500m  
5 INDUS500x0
6 alldiesaad
7 distall   
8 TRKDENSMAJ
9 houseden30
10 lucomm1000

# -9999 to remove
box_200 <- box_200 %>% filter(elevation > -9999)

# dist the smallest value to be the second smallest value 4.5
box_200$distall[box_200$distall == 0] <- 4.5
# and euc distinv_pm, the smallest value to be second smallest 58.5 
box_200$Eucdist_pm[box_200$Eucdist_pm == 0] <- 58.5

# calculate COA hoa and mixing state ----

box_200_01 <- box_200 %>% mutate(coa_full = 1857.351 - 4.85 * elevation + rest100m * 7.312, coa_source = 163 +  7.17 * rest100m + 0.00117 * lucomm1000, coa_solo_rest = 572.06 + 7.92 * rest100m,  hoa_full = 839 + 67.1 * trkdenall1 - 0.0199 * AGRI500m + 1/Eucdist_pm* 4.59 * 10^4 + INDUS500x0 *4.39*10^-4 + 60.5 * alldiesaad * 1/distall/distall, hoa_source = 1102.8 + 70.2 * trkdenall1 + 44.1 * TRKDENSMAJ + 39070.5/ distall/distall , mixing_state_pub = -0.473 + 0.0212 * TRKDENSMAJ + 7.91 * 10^-5 *houseden30 * 25 + 11.9 * pointde_ne) %>% dplyr::select(-coa, -hoa)

# compare with qing's value
summary(box_200_01$coa_full)
summary(box_200_01$hoa_full)
summary(box_200_01$mixing_state_pub) # still 1 minus mixing state here

# well, some values outside qing's measurements
write_csv(box_200_01, 'qing_final_box_200_to_plot_before_changing_extremes.csv')

# limiting max and min values
# not done yet
box_200_02 <- box_200_01
box_200_02 <- box_200_02 %>% mutate(hoa_full = if_else(hoa_full < 200, 200, hoa_full), hoa_full = if_else(hoa_full > 5114, 5114, hoa_full)) %>% mutate(coa_pub = if_else(coa_pub < 0, 0, coa_pub), coa_pub = if_else(coa_pub > 3826, 3826, coa_pub)) %>% mutate(mixing_state_pub = if_else(mixing_state_pub < 0.289, 0.289, mixing_state_pub), mixing_state_pub = if_else(mixing_state_pub > 0.704, 0.704, mixing_state_pub)) 
  
write_csv(box_200_02, 'qing_final_box_200_to_plot.csv')
