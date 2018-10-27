LUR_input <- read_csv("LUR_input.csv") # here chi is the right chi!

OA_new_dep <- read_excel('revised input 10252018/new_dep.xlsx', sheet = 2) %>% rename(ID = '200cell id')

chi_new_dep <- read_excel('revised input 10252018/new_dep.xlsx', sheet = 1) %>% rename(ID = Cell_ID) %>% dplyr::select(-"1 - chi")

# need to remove for OA
OA_removal <- LUR_input[c('ID', 'Elevation')] %>% anti_join(OA_new_dep[c('ID', 'COA')]) # oh, one COA ID even different....

# need to remove for chi
chi_removal <- LUR_input[c('ID', 'Elevation')] %>% anti_join(chi_new_dep[c('ID', 'chi')]) 

# so the mistake was 
OA_removal %>% inner_join(chi_removal, by = 'ID')

# 17334, or 17584, 21188
# check excel LUR_input, nope, these three are all inside the csv

# well the anti joined cells are actually quite different...