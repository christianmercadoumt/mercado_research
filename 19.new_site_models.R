#load packages
library(tidyverse)
library(mgcv)
library(gratia)
library(ggrepel)
library(corrplot)
library(GGally)
# library(mgcViz)
source('12.gam_fns.R')

hab_loc_codes <- read_csv('data/habtypes.csv')
hab_loc_codes <- hab_loc_codes %>% select(SETTING_ID, habclass, locationcode)

#load data
bai.train <- readRDS('data/bai.train.7_14_22.rds')
bai.train <- left_join(bai.train, hab_loc_codes, by = "SETTING_ID")

bai.spmx <- bai.train %>% 
  select(SETTING_ID, stand, MEASUREMENT_NO, myear, PLOT, cluster, unique_tree_id, 
         bai, DIAMETER, treeba, log.diam, log.bai, grwth.yr, mean_si, aspect_deg, 
         heatload, slope_deg, elev_m, NF, CROWN_RATIO, tpa.pl.all, tpa.pl.cutoff,
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
  mutate(unique.stand = group_indices()) %>%
  ungroup() %>% 
  mutate(unique.cluster = (1000*unique.stand)+cluster, 
         unique.plot = (1000*unique.stand)+PLOT) %>% 
  mutate(unique.cluster.meas = as.factor(unique.cluster + MEASUREMENT_NO), 
         unique.plot.meas = as.factor(unique.plot + MEASUREMENT_NO),
         unique.cluster.f = as.factor(unique.cluster), 
         unique.plot.f = as.factor(unique.plot), 
         unique.tree.f = as.factor(unique_tree_id)) %>% 
  filter(unique_tree_id != 15431)

gam.s1a <- gam(bai~s(DIAMETER), family = 'Gamma'(link = log), data = bai.spmx.ids, method = 'ML')

op2 <- gam(bai~s(DIAMETER) + s(cr, k = 9) + s(bal.pl.ratio) + 
             s(dq.pl.all) + s(qmd.pl.all) + s(ccf.pl), 
           family = 'Gamma'(link = log), 
           data = bai.spmx.ids, method = 'ML')

compd.ba1 <- gam(bai~s(DIAMETER) + s(cr, k = 9) + 
                   s(bal.pl.ratio) + s(ba.pl), 
                 family = 'Gamma'(link = log), 
                 data = bai.spmx.ids, method = 'ML')



##### compare sl-asp-el, heatload, si, and stand intercepts
site.sltraspel <- update(compd.ba1, . ~ . + s(asp.trasp) + s(slope_pct) + s(elev_m))
site.htld <- update(compd.ba1, . ~ . + s(heatload))
site.si <- update(compd.ba1, . ~ . + s(mean_si))
site.stand <- update(compd.ba1, . ~ . + stand)

site.gam.list <- list('trasp_sl_el' = site.sltraspel, 'heatld' = site.htld, 'site.ind.' = site.si, 'stand'=site.stand)
#lapply(sitelist2, rmse.conc)
rmse.conc.table(site.gam.list)

lapply(site.gam.list, concurvity)

conc.list <- lapply(site.gam.list, concurvity, full=F)
conc.list$trasp_sl_el
conc.list$heatld

## it seems to me that heatload might be the new winner.. lowest concurvity values across the board, and also simpler compared to slope,asp,el model

## next, run models including tree intercepts..
re.tree.1.htld <- update(site.htld, . ~ . + s(unique_tree_id, bs = 're'))
job::job(import = c(bai.spmx.ids), {
  library(mgcv)
  re.tree.1.htld <- gam(bai~s(DIAMETER) + s(cr, k = 9) + 
                                     s(bal.pl.ratio) + s(ba.pl) + s(heatload) + s(unique_tree_id, bs = 're'), 
                                   family = 'Gamma'(link = log), 
                                   data = bai.spmx.ids, method = 'ML')
  saveRDS(re.tree.1.htld, 'data/model_objects.1/re.tree.1.htld.rds')
})

job::job(import = c(bai.spmx.ids), {
  library(mgcv)
  try(ccf.htld <- gam(bai~s(DIAMETER) + s(cr, k = 9) + 
                          s(bal.pl.ratio) + s(ccf.pl) + s(heatload) + s(unique.tree.f, bs = 're'), 
                        family = 'Gamma'(link = log), 
                        data = bai.spmx.ids, method = 'ML'))
  try(saveRDS(ccf.htld, 'data/model_objects.1/ccf.htld.rds'))
  

  try(purity.htld <- gam(bai~s(DIAMETER) + s(cr, k = 9) + 
                          s(bal.pl.ratio) + s(ba.pl) + s(heatload) + s(percent_LAOC) + 
                       s(unique.tree.f, bs = 're'), 
                        family = 'Gamma'(link = log), 
                        data = bai.spmx.ids, method = 'ML'))
  try(saveRDS(purity.htld, 'data/model_objects.1/purity.htld.rds'))

  try(shade.htld <- gam(bai~s(DIAMETER) + s(cr, k = 9) + 
                          s(bal.pl.ratio) + s(ba.pl) + s(heatload) + s(shade.tol.pl) + 
                      s(unique.tree.f, bs = 're'), 
                        family = 'Gamma'(link = log), 
                        data = bai.spmx.ids, method = 'ML'))
  try(saveRDS(shade.htld, 'data/model_objects.1/shade.htld.rds'))
})
