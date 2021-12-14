#Stand summary tables
setwd("C:/git/research/mercado_research")

sumdata <- read_csv("data/pgp_stand_summary_data_18_21.csv", col_types = cols(.default = "?", SETTING_ID = "c"))
sumdata <- sumdata %>% filter(!is.na(sumdata$SETTING_ID))

(tbl1 <- sumdata %>% select(SETTING_ID, NF, ELEVATION, ASPECT_CLASS, HabType, COMMON_NAME, TPA_lead_CP1, TPA_TOT_CP1, TPA_lead_TP1, TPA_TOT_TP1, Init_disturbance, Remarks))

(tb1_red <- sumdata %>% select(SETTING_ID, NF, ELEVATION, ASPECT_CLASS, HabType, Init_disturbance))
tb1_red <- tb1_red %>% rename('Stand ID' = SETTING_ID, 'National Forest' = NF, 'Elevation' = ELEVATION, 'Aspect' = ASPECT_CLASS, 'Habitat Type' = HabType, 'Initial Disturbance' = Init_disturbance)

tb1_red

stand_summary_tbl1 <- kable(tb1_red)