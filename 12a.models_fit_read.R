##### Read-in/fit model objects ##### 
require(mgcv)
require(tidyverse)

hab_loc_codes <- read_csv('data/habtypes.csv')
hab_loc_codes <- hab_loc_codes %>% select(SETTING_ID, habclass, locationcode)

bai.train <- readRDS('data/bai.train.4_21_22.rds')
bai.train <- left_join(bai.train, hab_loc_codes, by = "SETTING_ID")

bai.size <- bai.train %>% 
  select(stand, MEASUREMENT_NO, PLOT, unique_tree_id, 
         bai, DIAMETER, treeba, log.diam, log.bai, grwth.yr) %>% 
  filter(bai != 0)

bai.site <- bai.train %>% 
  select(SETTING_ID, stand, MEASUREMENT_NO, myear, PLOT, cluster, unique_tree_id, 
         bai, DIAMETER, treeba, log.diam, log.bai, grwth.yr, mean_si, aspect_deg, 
         heatload, slope_deg, elev_m, NF, CROWN_RATIO, tpa.pl.all, tpa.pl.cutoff,
         ba.pl, bal.pl, ccf.pl, qmd.pl.all, qmd.pl.cutoff, dq.pl.all, dq.pl.cutoff, tpa.cl.all, tpa.pl.cutoff,
         ba.cl, ccf.cl, bal.cl, dq.cl.all, dq.cl.cutoff, HabType, habclass,
         locationcode) %>% 
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


unique(bai.site$PLOT)


bai.site.ids <- bai.site %>% group_by(stand) %>% 
  mutate(unique.stand = group_indices()) %>% ungroup() %>% 
  mutate(unique.cluster = (1000*unique.stand)+cluster, unique.plot = (1000*unique.stand)+PLOT) %>% 
  mutate(unique.cluster.meas = as.factor(unique.cluster + MEASUREMENT_NO), 
         unique.plot.meas = as.factor(unique.plot + MEASUREMENT_NO), 
         unique.cluster.f = as.factor(unique.cluster), 
         unique.plot.f = as.factor(unique.plot), 
         unique.tree.f = as.factor(unique_tree_id))


bai.spmx <- bai.train %>% 
  select(SETTING_ID, stand, MEASUREMENT_NO, myear, PLOT, cluster, unique_tree_id, 
         bai, DIAMETER, treeba, log.diam, log.bai, grwth.yr, mean_si, aspect_deg, 
         heatload, slope_deg, elev_m, NF, CROWN_RATIO, tpa.pl.all, tpa.pl.cutoff,
         ba.pl, bal.pl, ccf.pl, qmd.pl.all, qmd.pl.cutoff, dq.pl.all, dq.pl.cutoff, tpa.cl.all, tpa.pl.cutoff,
         ba.cl, ccf.cl, bal.cl, dq.cl.all, dq.cl.cutoff, HabType, habclass,
         locationcode, larch.ba.fraction.pl, shade.tol.pl) %>% 
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

re.stand.1 <- readRDS('data/model_objects.1/re.stand.1.rds')
re.cluster.1 <- readRDS('data/model_objects.1/re.cluster.1.rds')
re.plot.1 <- readRDS('data/model_objects.1/re.plot.1.rds')
re.tree.1 <- readRDS('data/model_objects.1/re.tree.1.rds')

#larch fraction - selected with random
spmx.re.gam.lf <- readRDS('data/model_objects.1/spmx.re.gam.lf.rds')
#shade tol - selected with random
spmx.re.gam.st <- readRDS('data/model_objects.1/spmx.re.gam.st.rds')

#larch fraction - alternative with random 
spmx.gam.alt.lf <- readRDS('data/model_objects.1/spmx.gam.alt.lf.rds')
#shade tol - alternative with random
spmx.gam.alt.st <- readRDS('data/model_objects.1/spmx.gam.alt.st.rds')

#alt model - tree RE
re.tree.1alt <- readRDS('data/model_objects.1/re.tree.1alt.rds')

#species mix without random effects
spmx.gam.lf <- gam(bai~s(DIAMETER) + s(cr, k = 9) + s(bal.pl.ratio) + s(ccf.pl) + s(asp_sin, asp_cos) + s(slope_pct) + s(larch.ba.fraction.pl), family = 'Gamma'(link = log), data = bai.spmx.ids, method = 'ML', control = list(nthreads = 3))

spmx.gam.st <- gam(bai~s(DIAMETER) + s(cr, k = 9) + s(bal.pl.ratio) + s(ccf.pl) + s(asp_sin, asp_cos) + s(slope_pct) + s(shade.tol.pl), family = 'Gamma'(link = log), data = bai.spmx.ids, method = 'ML', control = list(nthreads = 3))

#site-selected
site5a.2 <- gam(bai~s(DIAMETER) + s(cr, k = 9) + s(bal.pl.ratio) + s(ccf.pl) + s(asp_sin, asp_cos) + s(slope_pct), family = 'Gamma'(link = log), data = bai.spmx.ids, method = 'ML')

#size-selected
gam.s1a <- gam(bai~s(DIAMETER), family = 'Gamma'(link = log), data = bai.spmx.ids, method = 'ML') 
#size-ba - not selected
gam.s2a <- gam(bai~s(treeba), family = 'Gamma'(link = log), data = bai.spmx.ids, method = 'ML') 

#null
gam.null <- gam(bai~1, family = 'Gamma'(link = log), data = bai.spmx.ids, method = 'ML')

#density-competition best
best.6a <- gam(bai~s(DIAMETER) + s(ccf.pl) + s(cr, k = 9), family = 'Gamma'(link = log), data = bai.spmx.ids, method = 'ML')

#dens-comp full selected
compd.pl4 <- gam(bai~s(DIAMETER) + s(cr, k = 9) + s(bal.pl.ratio) + s(ccf.pl), family = 'Gamma'(link = log), data = bai.spmx.ids, method = 'ML')

#dens-comp alternative
compd.ba1 <- gam(bai~s(DIAMETER) + s(cr, k = 9) + s(bal.pl.ratio) + s(ba.pl), family = 'Gamma'(link = log), data = bai.spmx.ids, method = 'ML')

# site - best
site.n5a <-update(compd.pl4, . ~ . + s(asp_sin, asp_cos))

#site - alternative
site.n5b <- readRDS('data/model_objects.1/site.n5b.rds')

#fvs full
fvs.full <- gam(bai~elev_m + I(elev_m^2) + sl_asp_sin + sl_asp_cos + slope_pct + I(slope_pct^2) + I(ccf.cl/100) + log.diam + I(bal.cl/100) + cr + I(cr^2) + I(DIAMETER^2) + I(bal.cl/(100*log(DIAMETER+1))) + factor(habclass), data = bai.spmx.ids, family = 'Gamma'(link = log), method = 'ML')
#fvs size comp
fvs.sz.cmp <- gam(bai~I(ccf.cl/100) + log.diam + I(bal.cl/100) + cr + I(cr^2) + I(DIAMETER^2) + I(bal.cl/(100*log(DIAMETER+1))) + factor(habclass), data = bai.spmx.ids, family = 'Gamma'(link = log), method = 'ML')
summary(fvs.sz.cmp)
#fvs size
fvs.size1 <- gam(bai~log.diam + I(DIAMETER^2), family = 'Gamma'(link = log), data = bai.spmx.ids, method = 'ML')
