#### Clean up data and prepare for BAI modeling #### 

## TO DO: SEE LINE 22

#Read in data
source("4.readformat_pgp_recentlymeasured.R")
source("5.species_comp_functions.R")
source("8.larch_frac_fns.R")
source("6.plotvars.R")
source("7.clustervars.new.R")

## Prep data

# DA: Need subplot data for cluster or plot level calculations of BA, TPA, CCF, etc
## CM: done

## BAI Calculations
#calculating basal area increment for every tree. Choosing relevant variables. 
bai.data <- pgp_data_all %>% 
  filter(LIVE_DEAD == 'L') %>% #make sure BAI is only being considered for live trees
  mutate(treeba = BASAL_AREA_EQUIV.pl/TPA_EQUIV.pl) %>% #create individual tree basal area variable
  group_by(SETTING_ID, PLOT, DISTANCE, AZIMUTH) %>% #Group by stand, plot, distance and az - to isolate individual trees
  mutate(bai = (treeba[MEASUREMENT_NO + 1]-treeba)/(myear[MEASUREMENT_NO + 1]-myear)) %>%   #calculate bai variable based on difference between future measurement and current measurement
  ungroup() #housekeeping step

### CM: line 21- How do we track small tree BA's when they don't have distance and azimuth
### - perahps we won't have BAI data for small trees.. unless we use tag id's, however those remain inconsistent


#Get rid of irrational numbers (infinities) data
bai.data <- bai.data %>% filter(!is.nan(bai), !is.infinite(bai))

#calculate larch fraction plot and cluster levels - add them to the data
bai.data <- bai.data %>% larch.fraction.plot() %>% larch.fraction.clu()


#how many NA's?
sum(is.na(bai.data$bai))
summary(bai.data$bai)

#Plot-level variables

bai.plot <- bai.data %>% variables.plot()

#Cluster-level - this step requires that the previous step is done
bai.cluster <- bai.plot %>% variables.cluster()

#shade tolerance

bai.shade <- shade.tolerance.plot.cluster(bai.cluster)

