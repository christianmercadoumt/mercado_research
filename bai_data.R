#### Clean up data and prepare for BAI modeling #### 

#Read in data
source("readformat_pgp_recentlymeasured.R")
source("species_comp_functions.R")
source("larch_frac_fns.R")

## Prep data

#change date format
pgp_data_all <- mutate(pgp_data_all, mdate = as.Date(MEASUREMENT_DATE, format = "%m/%d/%Y"))
pgp_data_all <- mutate(pgp_data_all, myear = format(mdate, format = "%Y"))
pgp_data_all <- pgp_data_all %>% mutate(myear = as.numeric(myear))

#Filter out subplot data, missing Azimuth data and Distance, only look at live trees 
#NOTE:consider subplot data?
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
#need to add, DBH:QMD, crown competition factor, and shade tolerance
ccf_coef <- read_csv('data/ccf_species_coefficients.csv')


bai.dens.comp.pl <- bai.data %>% #call data
  left_join(x = ., y = ccf_coef, by = "SPECIES_SYMBOL") %>%  #This step adds the ccf coefficients from FVS-IE literature for the purpose of calculating CCF
  group_by(SETTING_ID, PLOT, MEASUREMENT_NO) %>% #group to the plot level for plot-level calculations
  mutate(ba.pl = sum(BASAL_AREA_EQUIV), #plot basal area calculations - sum of BA over plot
         tpa.pl = n()*20, #plot tpa - (number of large trees) x (expansion factor)
         bal.pl = map_dbl(BASAL_AREA_EQUIV, ~sum(BASAL_AREA_EQUIV[BASAL_AREA_EQUIV>.x])), #plot-level BAL - used basal area/acre here. This step performs the specified calculation across each record within the plot grouping. Thus, this step sums the basal areas for trees based on the condition that they are greater than the basal area of the observed record
         dq.pl = DIAMETER/sqrt(sum(DIAMETER^2)/n()), #DBH:QMD ratio calculated
         ccf.tree = case_when(
           (SPECIES_SYMBOL %in% c('POTR5', 'BEPA') & DIAMETER >= 1.0) ~ r1 + (r2*DIAMETER) + (r3*(DIAMETER^2)),
           (SPECIES_SYMBOL %in% c('POTR5', 'BEPA') & DIAMETER < 1.0) ~ r4*DIAMETER^(r5),
           DIAMETER >= 10.0 ~ r1 + (r2*DIAMETER) + (r3*(DIAMETER^2)),
           DIAMETER < 10.0 ~ r4*DIAMETER^(r5))) %>% #This step adds tree crown competition factor based on a series of conditionals which determine which equation/coefficients to use - this step could probably be simplified with custom funcions
  mutate(ccf.pl = sum(ccf.tree)) %>%
  ungroup() #housekeeping step

############ MORE COMING SOON ###############

