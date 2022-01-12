#### Read-in and filter data to stands that were measured most recently


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


#check
#unique(pgp_data_all$SETTING_ID) == unique(pgp_data1$SETTING_ID)
