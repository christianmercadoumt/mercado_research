#### Clean up data and prepare for BAI modeling #### 

#Read in data
source("3.readformat_pgp_recentlymeasured.R")
source("4.species_comp_functions.R")
source("5.larch_frac_fns.R")
source("5.plotvars.R")
source("5.clustervars.R")

## Prep data

#change date format
pgp_data_all <- mutate(pgp_data_all, mdate = as.Date(MEASUREMENT_DATE, format = "%m/%d/%Y"))
pgp_data_all <- mutate(pgp_data_all, myear = format(mdate, format = "%Y"))
pgp_data_all <- pgp_data_all %>% mutate(myear = as.numeric(myear))

#Filter out subplot data, missing Azimuth data and Distance, only look at live trees 
#NOTE:consider subplot data?

# DA: Need subplot data for cluster or plot level calculations of BA, TPA, CCF, etc

pgp_data_all <- pgp_data_all %>% filter(is.na(SUBPLOT), !is.na(AZIMUTH), !is.na(DISTANCE), LIVE_DEAD == "L")

#Add cluster variable
pgp_data_all <- mutate(pgp_data_all, cluster = 100*floor(PLOT/100))

## BAI Calculations
#calculating basal area increment for every tree. Choosing relevant variables. 
bai.data <- pgp_data_all %>% 
  mutate(treeba = BASAL_AREA_EQUIV/20) %>% #create individual tree basal area variable
  group_by(SETTING_ID, PLOT, DISTANCE, AZIMUTH) %>% #Group by stand, plot, distance and az - to isolate individual trees
  select(SETTING_ID, myear, MEASUREMENT_NO, cluster, PURPOSE_CODE,
         PLOT, AZIMUTH, DISTANCE, TAG_ID, SPECIES_SYMBOL,
         DIAMETER, HEIGHT, TPA_EQUIV, BASAL_AREA_EQUIV, treeba, 
         CROWN_CLASS, AGE, CROWN_RATIO) %>% #Select variables of interest
  mutate(bai = (treeba[MEASUREMENT_NO + 1]-treeba)/(myear[MEASUREMENT_NO + 1]-myear)) %>%   #calculate bai variable based on difference between future measurement and current measurement
  ungroup() #ungroup - housekeeping step

#Get rid of irrational numbers (infinities) data
bai.data <- bai.data %>% filter(!is.nan(bai), !is.infinite(bai))

#calculate larch fraction plot and cluster levels - add them to the data
bai.data <- bai.data %>% larch.fraction.plot() %>% larch.fraction.clu()


#how many NA's?
sum(is.na(bai.data$bai))
summary(bai.data$bai)

#Plot-level variables

bai.plot <- bai.data %>% variables.plot()

#Cluster-level
bai.cluster <- bai.plot %>% variables.cluster()

#shade tolerance

bai.shade <- shade.tolerance.plot.cluster(bai.cluster)

