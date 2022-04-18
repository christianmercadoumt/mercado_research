#Stand summary tables

source('species_comp_functions.R')
source('readformat_pgp_recentlymeasured.R')

sumdata <- read_csv("data/pgp_stand_summary_data_18_21.csv", col_types = cols(.default = "?", SETTING_ID = "c"))

sumdata <- sumdata %>% filter(!is.na(sumdata$SETTING_ID))

pct_larch <- species_comp_largetrees_ba(pgp_data1) %>%
  filter(SPECIES_SYMBOL == "LAOC", cluster != 100) %>%
  group_by(SETTING_ID, MEASUREMENT_NO, myear) %>% 
  summarise(averagelarchba = round(mean(percent_BA, na.rm = T), 0)) %>%
  ungroup() %>% 
  filter(MEASUREMENT_NO == 1) %>%
  select(SETTING_ID, averagelarchba) %>% 
  slice(-c(15, 20))

#Adding slice here is a quick fix, still need to address the fact that there are two measurement 1's for those setting ID's.


stands <- sumdata$SETTING_ID
lastyear <- c(rep(2021, 8), rep(2021, 4), rep(2018, 3), 2021, rep(2018, 4))
lastyr <- tibble(stands, lastyear) %>% rename(SETTING_ID = stands)

(tbl1 <- sumdata %>%
  select(SETTING_ID, NF, FirstYear, ELEVATION, ASPECT_CLASS, HabType, COMMON_NAME, TPA_lead_CP1, TPA_TOT_CP1, TPA_lead_TP1, TPA_TOT_TP1, Init_disturbance, NumMeasures) %>% 
  mutate(ELEVATION = round(ELEVATION*0.3048, 0), 
  cnt.tph = round(TPA_TOT_CP1*2.471, 0), 
  trt.tph = round(TPA_TOT_TP1*2.471, 0),
  num.meas = (NumMeasures + 1)))



sumdata <- right_join(tbl1, pct_larch, by = "SETTING_ID")
sumdata <- right_join(sumdata, lastyr, by = "SETTING_ID")

(tb1_red <- sumdata %>% select(SETTING_ID, NF, FirstYear, lastyear, num.meas, ELEVATION, ASPECT_CLASS, COMMON_NAME, Init_disturbance, averagelarchba, cnt.tph, trt.tph))
tb1_red <- tb1_red %>% rename('Stand ID' = SETTING_ID, 
                              'National Forest' = NF,
                              'First Measurement' = FirstYear,
                              'Last Measurement' = lastyear,
                              'Measurements' = num.meas,
                              'Elevation(m)' = ELEVATION,
                              'Aspect' = ASPECT_CLASS,
                              'Habitat Type' = COMMON_NAME,
                              'Initial Disturbance' = Init_disturbance,
                              '% Basal Area Larch' = averagelarchba,
                              'Control Density (ha^-1)' = cnt.tph,
                              'Treatment Density (ha^-1)' = trt.tph)
                              

tb1_red



#stand_summary_tbl1 <- tb1_red %>% kable(caption = "Stand Summary Table", format = 'markdown', booktabs = T) %>% kable_styling(latex_options = 'hold_position', full_width = F)

#Model Variables

#source("readformat_pgp_recentlymeasured.R")

Size <- c('Diameter', 'Basal Area', '', '', '', '')

Site <- c('Slope', 'Aspect(\u00B0)', 'Elevation', 'Stand Identity', 'Heatload', 'Site Index')

#Density <- c(, '', '', '')

Competition <- c('DBH:QMD Ratio', 'BAL', 'Crown Ratio', 'Crown Competition Factor', 'Trees Per Hectare', 'Stand Basal Area')

Species_Mixing <- c('Larch Basal Area Fraction', 'Stand Shade Tolerance', '', '', '', '')

Variables <- tibble(Size, Site, Competition, Species_Mixing)

Variables <- Variables %>% rename('Species Mixing' = Species_Mixing)

#Variables_table <- Variables %>% kable(caption = "List of Potential Model Variables", format = 'markdown') %>% kable_styling(latex_options = c('hold_position'), full_width = F)
