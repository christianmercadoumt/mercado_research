#### Read-in and filter data to stands that were measured most recently
## This does not include the latest measurement data!

source("1.readformat.R")
source("readformat_kt_F14.R")
#check data sets have the same variables
colnames(kt_all_raw) == colnames(lolo_all_raw)
colnames(kt1) == colnames(lolo1)

#Identify list of stands we want (stands with the most recent measurements)
sumdata <- read_csv("data/pgp_stand_summary_data_18_21.csv", col_types = cols(.default = "?", SETTING_ID = "c"))
sumdata <- sumdata %>% filter(!is.na(sumdata$SETTING_ID))
a <- unique(sumdata$SETTING_ID)


#Bind two main datasets
pgp_data_all <- rbind(kt_all_raw, lolo_all_raw)

pgp_data1 <- rbind(kt1, lolo1)

#filter to stands of interest - all data
pgp_data_all <- pgp_data_all %>% filter(SETTING_ID %in% a)
#filter to stands of interest - reduced variables
pgp_data1 <- pgp_data1 %>% filter(SETTING_ID %in% a)

#check
unique(pgp_data_all$SETTING_ID) == unique(pgp_data1$SETTING_ID)