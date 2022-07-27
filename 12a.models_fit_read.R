##### Read-in/fit model objects ##### 
source('12.gam_fns.R')

hab_loc_codes <- read_csv('data/habtypes.csv')
hab_loc_codes <- hab_loc_codes %>% select(SETTING_ID, habclass, locationcode)

bai.train <- readRDS('data/bai.train.7_14_22.rds')
bai.train <- left_join(bai.train, hab_loc_codes, by = "SETTING_ID")

bai.size <- bai.train %>% 
  select(stand, MEASUREMENT_NO, PLOT, unique_tree_id, 
         bai, DIAMETER, treeba, log.diam, log.bai, grwth.yr) %>% 
  filter(bai != 0)

bai.site <- bai.train %>% 
  select(SETTING_ID, stand, MEASUREMENT_NO, myear, PLOT, cluster, unique_tree_id, 
         bai, DIAMETER, treeba, log.diam, log.bai, grwth.yr, mean_si, aspect_deg, 
         heatload, slope_deg, elev_m, NF, CROWN_RATIO, tpa.pl.all, tpa.pl.cutoff,
         ba.pl, bal.pl, ccf.pl, ccf.nospp, qmd.pl.all, qmd.pl.cutoff, dq.pl.all, dq.pl.cutoff, tpa.cl.all, tpa.pl.cutoff,
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

# re.stand.1 <- readRDS('data/model_objects.1/re.stand.1.rds')
# re.cluster.1 <- readRDS('data/model_objects.1/re.cluster.1.rds')
# re.plot.1 <- readRDS('data/model_objects.1/re.plot.1.rds')

# Tree random effect base model ####
re.tree.1 <- readRDS('data/model_objects.1/re.tree.1.rds')

# re.tree.1 <- gam(bai~s(DIAMETER) + s(cr, k = 9) + s(bal.pl.ratio) + s(ccf.pl) +
#                    s(asp_sin, asp_cos) + s(slope_pct) + s(unique_tree_id, bs = 're'), 
#                  family = 'Gamma'(link = log), 
#                  data = bai.spmx.ids, method = 'ML')

## with gamm
re.tree.mm <- gamm(bai~s(DIAMETER) + s(cr, k = 9) + s(bal.pl.ratio) + s(ccf.pl) +
                     s(asp_sin, asp_cos) + s(slope_pct), random = list(unique_tree_id=~1), 
                   family = 'Gamma'(link = log), 
                   data = bai.spmx.ids, method = 'ML')
summary(re.tree.1)
summary(re.tree.mm$gam)

nosite.base <- readRDS('data/model_objects.1/nosite.base.RDS')
#   gam(bai~s(DIAMETER) + s(cr, k = 9) + s(bal.pl.ratio) + s(ccf.pl) + 
#                      s(unique_tree_id, bs = 're'),
#                    family = 'Gamma'(link = log),
#                    data = bai.spmx.ids, method = 'ML')
# saveRDS(nosite.base, 'data/model_objects.1/nosite.base.RDS')
# saveRDS(nosite.base, 'C:/Users/cm165878/Box/research_/nosite.base.RDS')

nocomp.base <- readRDS('data/model_objects.1/nocomp.base.RDS')
#   gam(bai~s(DIAMETER) + s(asp_sin, asp_cos) + s(slope_pct) + s(unique_tree_id, bs = 're'),
#                    family = 'Gamma'(link = log),
#                    data = bai.spmx.ids, method = 'ML')
# saveRDS(nocomp.base, 'data/model_objects.1/nocomp.base.RDS')
# saveRDS(nocomp.base, 'C:/Users/cm165878/Box/research_/nocomp.base.RDS')

nosize.base <- readRDS('data/model_objects.1/nosize.base.RDS')
  # gam(bai~s(cr, k = 9) + s(bal.pl.ratio) + s(ccf.pl) +
  #                    s(asp_sin, asp_cos) + s(slope_pct) + s(unique_tree_id, bs = 're'),
  #                  family = 'Gamma'(link = log),
  #                  data = bai.spmx.ids, method = 'ML')
# saveRDS(nosize.base, 'data/model_objects.1/nosize.base.RDS')
# saveRDS(nosize.base, 'C:/Users/cm165878/Box/research_/nosize.base.RDS')

#intercept only version (random effect)
int.only.base <- readRDS('data/model_objects.1/int.only.base.rds')

# int.only.base <-   gam(bai~s(unique_tree_id, bs = 're'),
#                        family = 'Gamma'(link = log),
#                        data = bai.spmx.ids, method = 'ML')
# saveRDS(int.only.base, 'data/model_objects.1/int.only.base.rds')

## swap out unique_tree_id for stand, cluster, plot ids

#larch fraction - selected with random
spmx.re.gam.lf <- readRDS('data/model_objects.1/spmx.re.gam.lf_7.11.rds')
spmx.re.gam.lf <- bam(bai~s(DIAMETER) + s(cr, k = 9) + s(bal.pl.ratio) + s(ccf.pl) +
      s(asp_sin, asp_cos) + s(slope_pct) + s(percent_LAOC) + s(unique_tree_id, bs = 're'),
    family = 'Gamma'(link = log), data = bai.spmx.ids, method = 'ML', discrete = TRUE, control = list(nthreads = 2))
saveRDS(spmx.re.gam.lf, 'data/model_objects.1/spmx.re.bam.lf_7.14.rds')

spmx.re.bam2.lf <- bam(bai~s(DIAMETER, bs='cr') + s(cr, k = 9, bs='cr') + s(bal.pl.ratio, bs='cr') + s(ccf.pl, bs='cr') +
                        te(asp_sin, asp_cos, bs='cr') + s(slope_pct, bs='cr') + s(percent_LAOC, bs='cr') + s(unique_tree_id, bs = 're'),
                      family = 'Gamma'(link = log), data = bai.spmx.ids, method = 'fREML', discrete=TRUE, control = list(nthreads = 2))

#shade tol - selected with random
spmx.re.gam.st <- readRDS('data/model_objects.1/spmx.re.gam.st.rds')
# spmx.re.gam.st <- gam(bai~s(DIAMETER) + s(cr, k = 9) + s(bal.pl.ratio) + s(ccf.pl) + 
#       s(asp_sin, asp_cos) + s(slope_pct) + s(shade.tol.pl) + s(unique_tree_id, bs = 're'), 
#     family = 'Gamma'(link = log), data = bai.spmx.ids, method = 'ML', control = list(nthreads = 3))

#larch fraction - alternative with random 
spmx.gam.alt.lf <- readRDS('data/model_objects.1/spmx.gam.alt.lf_7.14.rds')
spmx.gam.alt.lf <- gam(bai ~ s(DIAMETER) + s(cr, k = 9) + s(bal.pl.ratio) + s(ba.pl) +
  s(asp_sin, asp_cos) + s(unique.tree.f, bs = "re") + s(percent_LAOC),
  family = 'Gamma'(link = log), data = bai.spmx.ids, method = 'ML', control = list(nthreads = 2))
saveRDS(spmx.gam.alt.lf, 'data/model_objects.1/spmx.gam.alt.lf_7.14.rds')

#shade tol - alternative with random
spmx.gam.alt.st <- readRDS('data/model_objects.1/spmx.gam.alt.st_7.14.rds')

#ccf as spp.mixing
spmx.gam.alt.ccf <- readRDS('data/model_objects.1/spmx.gam.alt.ccf_7.14.rds')

# try(spmx.gam.alt.st <- gam(bai ~ s(DIAMETER) + s(cr, k = 9) + s(bal.pl.ratio) + s(ba.pl) +
#       s(asp_sin, asp_cos) + s(unique.tree.f, bs = "re") + s(shade.tol.pl),
#     family = 'Gamma'(link = log), data = bai.spmx.ids, method = 'ML', control = list(nthreads = 2)))
# try(saveRDS(spmx.gam.alt.st, 'data/model_objects.1/spmx.gam.alt.st_7.14.rds'))
# 
try(spmx.gam.alt.ccf <- gam(bai ~ s(DIAMETER) + s(cr, k = 9) + s(bal.pl.ratio) + s(ccf.pl) +
                         s(asp_sin, asp_cos) + s(unique.tree.f, bs = "re"),
                       family = 'Gamma'(link = log), data = bai.spmx.ids, method = 'ML', control = list(nthreads = 2)))
try(saveRDS(spmx.gam.alt.ccf, 'data/model_objects.1/spmx.gam.alt.ccf_7.14.rds'))

#alt model - tree RE
re.tree.1alt <- readRDS('data/model_objects.1/re.tree.1alt.rds')

re.tree.1ba <- gam(bai ~ s(DIAMETER) + s(cr, k = 9) + s(bal.pl.ratio) + s(ba.pl) +
      s(asp_sin, asp_cos) + s(unique.tree.f, bs = "re"),
      family = 'Gamma'(link = log), data = bai.spmx.ids, method = 'ML', control = list(nthreads = 2))
saveRDS(re.tree.1ba, 'data/model_objects.1/re.tree.1ba_7.14.rds')
#species mix without random effects
spmx.gam.lf <- readRDS('data/model_objects.1/spmx.gam.lf_7.11.rds')
# spmx.gam.lf <- gam(bai~s(DIAMETER) + s(cr, k = 9) + s(bal.pl.ratio) + s(ccf.pl) + s(asp_sin, asp_cos) + s(slope_pct) + s(percent_LAOC), family = 'Gamma'(link = log), data = bai.spmx.ids, method = 'ML', control = list(nthreads = 3))
# saveRDS(spmx.gam.lf, 'data/model_objects.1/spmx.gam.lf_7.11.rds')

spmx.gam.st <- readRDS('data/model_objects.1/spmx.gam.st.rds')
#   gam(bai~s(DIAMETER) + s(cr, k = 9) + s(bal.pl.ratio) + s(ccf.pl) + s(asp_sin, asp_cos) + s(slope_pct) + s(shade.tol.pl), family = 'Gamma'(link = log), data = bai.spmx.ids, method = 'ML', control = list(nthreads = 3))
# saveRDS(spmx.gam.st, 'data/model_objects.1/spmx.gam.st.rds')

#site-selected
site5a.2 <- readRDS('data/model_objects.1/site5a.2.rds')
#   gam(bai~s(DIAMETER) + s(cr, k = 9) + s(bal.pl.ratio) + s(ccf.pl) + s(asp_sin, asp_cos) + s(slope_pct), family = 'Gamma'(link = log), data = bai.spmx.ids, method = 'ML')
# saveRDS(site5a.2, 'data/model_objects.1/site5a.2.rds')

#size-selected
gam.s1a <- gam(bai~s(DIAMETER), family = 'Gamma'(link = log), data = bai.spmx.ids, method = 'ML') 
#size-ba - not selected
gam.s2a <- gam(bai~s(treeba), family = 'Gamma'(link = log), data = bai.spmx.ids, method = 'ML') 

#null
gam.null <- gam(bai~1, family = 'Gamma'(link = log), data = bai.spmx.ids, method = 'ML')

#density-competition best
best.6a <- readRDS('data/model_objects.1/best.6a.rds')
#   gam(bai~s(DIAMETER) + s(ccf.pl) + s(cr, k = 9), family = 'Gamma'(link = log), data = bai.spmx.ids, method = 'ML')
# saveRDS(best.6a, 'data/model_objects.1/best.6a.rds')

#dens-comp full selected
compd.pl4 <- readRDS('data/model_objects.1/compd.pl4.rds')
#   gam(bai~s(DIAMETER) + s(cr, k = 9) + s(bal.pl.ratio) + s(ccf.pl), family = 'Gamma'(link = log), data = bai.spmx.ids, method = 'ML')
# saveRDS(compd.pl4, 'data/model_objects.1/compd.pl4.rds')

#dens-comp alternative
compd.ba1 <- readRDS('data/model_objects.1/compd.ba1.rds')
#   gam(bai~s(DIAMETER) + s(cr, k = 9) + s(bal.pl.ratio) + s(ba.pl), family = 'Gamma'(link = log), data = bai.spmx.ids, method = 'ML')
# saveRDS(compd.ba1, 'data/model_objects.1/compd.ba1.rds')

# site - best
site.n5a <-readRDS('data/model_objects.1/site.n5a.rds')
#   update(compd.pl4, . ~ . + s(asp_sin, asp_cos))
# saveRDS(site.n5a, 'data/model_objects.1/site.n5a.rds')

#site - alternative
site.n5b <- readRDS('data/model_objects.1/site.n5b.rds')

#fvs full
fvs.full <- gam(bai~elev_m + I(elev_m^2) + sl_asp_sin + sl_asp_cos + slope_pct + I(slope_pct^2) + I(ccf.pl/100) + log.diam + I(bal.pl/100) + cr + I(cr^2) + I(DIAMETER^2) + I(bal.pl/(100*log(DIAMETER+1))) + factor(habclass), data = bai.spmx.ids, family = 'Gamma'(link = log), method = 'ML')
#fvs size comp
fvs.sz.cmp <- gam(bai~I(ccf.pl/100) + log.diam + I(bal.pl/100) + cr + I(cr^2) + I(DIAMETER^2) + I(bal.pl/(100*log(DIAMETER+1))) + factor(habclass), data = bai.spmx.ids, family = 'Gamma'(link = log), method = 'ML')

#fvs size
fvs.size1 <- gam(bai~log.diam + I(DIAMETER^2), family = 'Gamma'(link = log), data = bai.spmx.ids, method = 'ML')

#fvs re
fvs.full.re <- readRDS('data/model_objects.1/fvs.full.re.rds')
