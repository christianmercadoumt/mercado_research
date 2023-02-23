#load packages
require(tidyverse)
require(mgcv)

hab_loc_codes <- read_csv('data/habtypes.csv')
hab_loc_codes <- hab_loc_codes %>% select(SETTING_ID, habclass, locationcode)

#load data
bai.train <- readRDS('data/bai.train.7_14_22.rds')
bai.train <- left_join(bai.train, hab_loc_codes, by = "SETTING_ID")

bai.spmx <- bai.train %>% 
  select(SETTING_ID, stand, MEASUREMENT_NO, myear, PLOT, cluster, unique_tree_id, 
         bai, DIAMETER, treeba, log.diam, log.bai, grwth.yr, mean_si, aspect_deg, 
         heatload, slope_deg, elev_m, NF, CROWN_RATIO, CROWN_CLASS, tpa.pl.all, tpa.pl.cutoff,
         ba.pl, bal.pl, ccf.pl, ccf.nospp, qmd.pl.all, qmd.pl.cutoff, dq.pl.all, 
         dq.pl.cutoff, tpa.cl.all, tpa.pl.cutoff,
         ba.cl, ccf.cl, bal.cl, dq.cl.all, dq.cl.cutoff, HabType, habclass,
         locationcode, percent_LAOC, shade.tol.pl, dom.spp.pl.ba, percent_PSME, 
         percent_PIEN, percent_ABLA, percent_ABGR, percent_PICO, percent_PIPO, percent_other) %>% 
  mutate(slope_pct = tan(slope_deg*(pi/180))*100, 
         asp_sin = sin(aspect_deg*(pi/180)),
         asp_cos = cos(aspect_deg*(pi/180))) %>%
  mutate(sl_asp_sin = asp_sin*slope_pct, 
         sl_asp_cos = asp_cos*slope_pct, 
         asp.trasp = trasp(aspect_deg), 
         HabType = as.factor(HabType),
         cr = CROWN_RATIO/100,
         bal.pl.ratio = bal.pl/ba.pl,
         unique_tree_id = factor(unique_tree_id)) %>% 
  filter(bai != 0)


bai.spmx.ids <- bai.spmx %>% 
  group_by(stand) %>% 
  mutate(unique.stand = cur_group_id()) %>%
  ungroup() %>% 
  mutate(unique.cluster = (1000*unique.stand)+cluster, 
         unique.plot = (1000*unique.stand)+PLOT) %>% 
  mutate(unique.cluster.meas = as.factor(unique.cluster + MEASUREMENT_NO), 
         unique.plot.meas = as.factor(unique.plot + MEASUREMENT_NO),
         unique.cluster.f = as.factor(unique.cluster), 
         unique.plot.f = as.factor(unique.plot), 
         unique.tree.f = as.factor(unique_tree_id)) %>% 
  filter(unique_tree_id != 15431)

#convert units to metric
bai.spmx.ids_metric <- bai.spmx.ids %>% 
  mutate(dbh.cm = DIAMETER*2.54, treeba.cm = treeba*(2.54^2), 
         qmd.pl.all.cm = qmd.pl.all*2.54, tph.pl = tpa.pl.all*2.471,
         bah.pl = ba.pl*(2.471/10.764), bai.cm = bai*(2.54^2))
