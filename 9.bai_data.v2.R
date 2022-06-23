#### Clean up data and prepare for BAI modeling #### 

#Read in data
source("4.readformat_pgp_recentlymeasured.R")
source("5.species_comp_functions.R")
source("8.larch_frac_fns.R")
source("6.plotvars.R")
source("7.clustervars.new.R")

#Site data
load("data/ingy_settings_si.Rdata")
load('data/ingy_clusters_googleearth.Rdata')

#create lat long variables extracted from list element in ingy_clusters
options(digits = 10) #without this, the loop rounds numbers. Not sure why. 
lat.cl <- NA
lon.cl <- NA
for(i in 1:length(ingy_clusters$geometry)){
  
  lat.cl[[i]] <- ingy_clusters$geometry[[i]][1]
  lon.cl[[i]] <- ingy_clusters$geometry[[i]][2]
}
ingy_clusters <- ingy_clusters %>% mutate(lat.cl = lat.cl, lon.cl = lon.cl) %>% select(-geometry)

### get stand data
stand.data <- read_csv('data/pgp_stand_summary_data_18_21.csv')
stand.data <- stand.data %>% 
  select(SETTING_ID, NF, HabType, SCIENTIFIC_NAME, COMMON_NAME)
#combine stand data with cluster attributes
stand.cl <- left_join(ingy_clusters, stand.data, by = 'SETTING_ID')

#Site index - merge before running bai calcs

pgp_data_all <- left_join(pgp_data_all, ingy_si)

## Prep data

## BAI Calculations
source("9_a.tree_matching.R")
# #calculating basal area increment for every tree. Choosing relevant variables. 
# bai.calc <- pgp_data_all %>% 
#   filter(LIVE_DEAD == 'L', MEASUREMENT_NO != 0) %>% #make sure BAI is only being considered for live trees
#   mutate(treeba = BASAL_AREA_EQUIV.pl/TPA_EQUIV.pl) %>% #create individual tree basal area variable
#   group_by(SETTING_ID, PLOT, SPECIES_SYMBOL, DISTANCE, AZIMUTH) %>% #Group by stand, plot, distance and az - to isolate individual trees
#   mutate(id = cur_group_id()) %>% #create index id
#   ungroup() %>% 
#   filter(!is.na(DISTANCE), !is.na(AZIMUTH)) %>% 
#   group_by(id) %>% 
#   mutate(bai = (lead(treeba, order_by = MEASUREMENT_NO)-treeba)/(lead(myear, order_by = MEASUREMENT_NO)-myear)) %>%   #calculate bai variable based on difference between future measurement and current measurement
#   ungroup() %>%  #housekeeping step
#   filter(!is.nan(bai), !is.infinite(bai)) #Get rid of infinities/irrationals

bai.calc1 <- bai.calc %>% 
  mutate(treeba = BASAL_AREA_EQUIV.pl/TPA_EQUIV.pl) %>%  #create individual tree basal area variable
  group_by(unique_tree_id) %>% 
  arrange(SETTING_ID, unique_tree_id, MEASUREMENT_NO) %>% 
  mutate(next.yr = lead(myear)) %>% 
  mutate(grwth.yr = next.yr-myear) %>%  #create growth period variable
  rename(bai = bai.calc)

# calculate larch fraction plot and cluster levels - add them to the data
bai.larch.frac <- bai.calc1 %>% larch.fraction.plot() %>% larch.fraction.clu() %>% larch.frac.tpa() %>% dom.spp.ba()

#Plot-level variables
bai.plot <- bai.larch.frac %>% variables.plot()

#Cluster-level - this step requires that the previous step is done
bai.cluster <- bai.plot %>% variables.cluster()

#shade tolerance
bai.shade <- shade.tolerance.plot.cluster(bai.cluster)
#saveRDS(bai.shade, 'data/bai.shade.rds')

#filter out na's and non-larch
bai.data1 <- bai.shade %>% filter(!is.na(bai) & bai >= 0, SPECIES_SYMBOL == 'LAOC')
bai.data2 <- left_join(bai.data1, stand.data, by = "SETTING_ID") # adding stand data/site chars. 
bai.data3 <- bai.data2 %>% group_by(SETTING_ID, NF) %>% 
  mutate(stand = case_when(NF == 'Kootenai' ~ 1400+cur_group_id(),
                           NF == 'Lolo' ~ 1600+cur_group_id())) %>% #maybe turn this step into a function
  ungroup() %>% 
  mutate(bai = bai*144, treeba = treeba*144) %>%  #Convert from square feet to square inches
  mutate(log.bai = log(bai), sqrt.bai = sqrt(bai), 
         log.diam = log(DIAMETER), stand = as.factor(stand)) %>% #add some vars
  filter(!(stand %in% c(1613, 1620)), MEASUREMENT_NO != 0) #Remove stands with barely any larch data


bai.data <- left_join(bai.data3, ingy_clusters)
bai.data <- bai.data %>% select(!c(SET_CN, PLOT_CN, TREE_CN))
#bai.data <- left_join(bai.data, ingy_si)
#how many NA's?
#sum(is.na(bai.data$bai))
#summary(bai.data$bai)