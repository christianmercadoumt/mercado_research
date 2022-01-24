#### Read-in and filter data to stands that were measured most recently



### DA: NEED to update the TPA_EQUIV (and perhaps BA_EQUIV) for most setting_id
###  records to reflect that there were 3 small tree subplots. This means
###  changing the TPA_EQUIV from 300 tr/ac to 100 tr/ac, and likewise dividing
###  any BA_EQUIV by 3 (for small trees)

### CM: Done. 

source("1.readformat.R")
source("2.readformat_kt_F14.R")
source("3.recentmeasures_1821.R")
#check data sets have the same variables
colnames(kt_all_raw) == colnames(lolo_all_raw)
colnames(kt1) == colnames(lolo1)

#Identify list of stands we want (stands with the most recent measurements)
sumdata <- read_csv("data/pgp_stand_summary_data_18_21.csv", col_types = cols(.default = "?", SETTING_ID = "c"))
sumdata <- sumdata %>% filter(!is.na(sumdata$SETTING_ID))
(a <- unique(sumdata$SETTING_ID))


#Bind two main datasets
pgp_data_all <- rbind(kt_all_raw, lolo_all_raw)

#change date format
pgp_data_all <- mutate(pgp_data_all, mdate = as.Date(MEASUREMENT_DATE, format = "%m/%d/%Y"))
pgp_data_all <- mutate(pgp_data_all, myear = format(mdate, format = "%Y"))
pgp_data_all <- pgp_data_all %>% mutate(myear = as.numeric(myear))
#Add cluster variable
pgp_data_all <- mutate(pgp_data_all, cluster = 100*floor(PLOT/100))
pgp_data1 <- rbind(kt1, lolo1)

#filter to stands of interest - all data
pgp_data_all <- pgp_data_all %>% filter(SETTING_ID %in% a)
#filter to stands of interest - reduced variables
pgp_data1 <- pgp_data1 %>% filter(SETTING_ID %in% a)

#add in recentmeasures_1821
pgp_data_all <- bind_rows(pgp_data_all, lolo.koot.18.21)

#Add measurement numbers for latest measurement data - missing form imported data set
pgp_data_all <- pgp_data_all %>% group_by(SETTING_ID) %>% 
  mutate(MEASUREMENT_NO = 
           case_when(is.na(MEASUREMENT_NO) ~ 1+max(MEASUREMENT_NO, na.rm = T),
                     !is.na(MEASUREMENT_NO) ~ MEASUREMENT_NO)) %>% 
  ungroup()
summary(pgp_data_all$MEASUREMENT_NO) #There shouldn't be any NA's
##something that doesn't have a measurement date should have the maximum measurement number for a setting id

#Get rid of SE and CI purpose codes
pgp_data_all <- pgp_data_all %>% filter(!(PURPOSE_CODE %in% c('SE', 'CI')))

#Recalculate TPA Equiv and BA Equiv to reflect small tree plots - how to handle tree counts for small tree plots
#BA_EQUIV - correct them before changing
pgp_data_all <- pgp_data_all %>% 
  mutate(BASAL_AREA_EQUIV = .005454154*TPA_EQUIV*DIAMETER^2)

#TPA EQUIV
pgp_data_all <- pgp_data_all %>% 
  mutate(TreeCount = case_when(is.na(TreeCount) & TPA_EQUIV >= 300 ~ TPA_EQUIV/300,
                               is.na(TreeCount) & TPA_EQUIV < 300 ~ 1)) %>% #Adds Tree counts to subplots. Tree counts were extrapolated form TPA_EQUIV
  mutate(TPA_EQUIV.pl = case_when(TPA_EQUIV >= 300 ~ TPA_EQUIV/3, 
                                  TPA_EQUIV < 300 & TPA_EQUIV != 0 ~ TPA_EQUIV,
                                  TPA_EQUIV == 0 ~ 20)) %>% #Alters TPA_EQUIV for small tree plots, and retains values for large tree plots. 
  mutate(TPA_EQUIV.cl = case_when(TPA_EQUIV >= 300 ~ TPA_EQUIV/9, 
                                  TPA_EQUIV < 300 & TPA_EQUIV != 0 ~ TPA_EQUIV/3,
                                  TPA_EQUIV == 0 ~ (20/3))) # Added bit of code to account for 0's (bonus data) in TPA_EQUIV so those can be considerred for BAI

#BA_EQUIV - adjust small tree plots
pgp_data_all <- pgp_data_all %>% 
  mutate(BASAL_AREA_EQUIV.pl = .005454154*TPA_EQUIV.pl*DIAMETER^2,
         BASAL_AREA_EQUIV.cl = .005454154*TPA_EQUIV.cl*DIAMETER^2)

