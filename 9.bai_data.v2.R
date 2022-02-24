#### Clean up data and prepare for BAI modeling #### 

#Read in data
source("4.readformat_pgp_recentlymeasured.R")
source("5.species_comp_functions.R")
source("8.larch_frac_fns.R")
source("6.plotvars.R")
source("7.clustervars.new.R")

stand.data <- read_csv('data/pgp_stand_summary_data_18_21.csv')
stand.data <- stand.data %>% 
  select(SETTING_ID, NF, ASPECT_CLASS, ELEVATION, SLOPE_CLASS, HabType, SCIENTIFIC_NAME, COMMON_NAME)
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
bai.larch.frac <- bai.calc1 %>% larch.fraction.plot() %>% larch.fraction.clu()

#Plot-level variables
bai.plot <- bai.larch.frac %>% variables.plot()

#Cluster-level - this step requires that the previous step is done
bai.cluster <- bai.plot %>% variables.cluster()

#shade tolerance
bai.shade <- shade.tolerance.plot.cluster(bai.cluster)

#filter out na's and non-larch
bai.data1 <- bai.shade %>% filter(!is.na(bai) & bai >= 0, SPECIES_SYMBOL == 'LAOC')
bai.data2 <- left_join(bai.data1, stand.data, by = "SETTING_ID") # adding stand data/site chars. 
bai.data <- bai.data2 %>% group_by(SETTING_ID, NF) %>% 
  mutate(stand = case_when(NF == 'Kootenai' ~ 1400+cur_group_id(),
                           NF == 'Lolo' ~ 1600+cur_group_id())) %>% #maybe turn this step into a function
  ungroup() %>% 
  mutate(log.bai = log(bai), log.diam = log(DIAMETER), stand = as.factor(stand))

#how many NA's?
#sum(is.na(bai.data$bai))
#summary(bai.data$bai)