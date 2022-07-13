### data and stuff (copied from 12 and 12a)
require(mgcv)
require(tidyverse)

trasp <- function(degrees_aspect){
  (1-cos((pi/180)*(degrees_aspect-30)))/2
}

hab_loc_codes <- read_csv('data/habtypes.csv')
hab_loc_codes <- hab_loc_codes %>% select(SETTING_ID, habclass, locationcode)

bai.train <- readRDS('data/bai.train.7_11_22.rds')
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


##### model fits

re.tree.p1a <- gamm(bai~ 1,
                    random=list(unique_tree_id=~1),
                    family = 'Gamma'(link = log),
                    data = bai.spmx.ids, method = 'ML')
save(re.tree.p1a,file=file.path(getwd(),"data","re.tree.p1a.Rdata"))
sum(residuals(re.tree.p1a$gam)^2)
AIC(re.tree.p1a$lme)
logLik(re.tree.p1a$lme)
rm(re.tree.p1a)

re.tree.p2a <- gamm(bai~ s(DIAMETER),
                    random=list(unique_tree_id=~1),
                  family = 'Gamma'(link = log),
                  data = bai.spmx.ids, method = 'ML')
save(re.tree.p2a,file=file.path(getwd(),"data","re.tree.p2a.Rdata"))
sum(residuals(re.tree.p2a$gam)^2)
AIC(re.tree.p2a$lme)
logLik(re.tree.p2a$lme)
rm(re.tree.p2a)

re.tree.p3a <- gamm(bai~ s(DIAMETER) + s(cr, k = 9) ,
                    random=list(unique_tree_id=~1),
                    family = 'Gamma'(link = log),
                    data = bai.spmx.ids, method = 'ML')
save(re.tree.p3a,file=file.path(getwd(),"data","re.tree.p3a.Rdata"))
rm(re.tree.p3a)

re.tree.p4a <- gamm(bai~ s(DIAMETER) + s(cr, k = 9) + s(bal.pl.ratio) ,
                    random=list(unique_tree_id=~1),
                    family = 'Gamma'(link = log),
                    data = bai.spmx.ids, method = 'ML')
save(re.tree.p4a,file=file.path(getwd(),"data","re.tree.p4a.Rdata"))
rm(re.tree.p4a)

re.tree.p5a <- gamm(bai~ s(DIAMETER) + s(cr, k = 9) + s(bal.pl.ratio) + s(ccf.pl),
                    random=list(unique_tree_id=~1),
                    family = 'Gamma'(link = log),
                    data = bai.spmx.ids, method = 'ML')
save(re.tree.p5a,file=file.path(getwd(),"data","re.tree.p5a.Rdata"))
rm(re.tree.p5a)

re.tree.p6a <- gamm(bai~ s(DIAMETER) + s(cr, k = 9) + s(bal.pl.ratio) + s(ccf.pl)+
                      s(asp_sin, asp_cos),
                    random=list(unique_tree_id=~1),
                    family = 'Gamma'(link = log),
                    data = bai.spmx.ids, method = 'ML')
save(re.tree.p6a,file=file.path(getwd(),"data","re.tree.p6a.Rdata"))
rm(re.tree.p6a)

re.tree.p7a <- gamm(bai~ s(DIAMETER) + s(cr, k = 9) + s(bal.pl.ratio) + s(ccf.pl)+
                      s(asp_sin, asp_cos)  + s(slope_pct),
                    random=list(unique_tree_id=~1),
                    family = 'Gamma'(link = log),
                    data = bai.spmx.ids, method = 'ML')
save(re.tree.p7a,file=file.path(getwd(),"data","re.tree.p7a.Rdata"))
rm(re.tree.p7a)

re.tree.p8a <- gamm(bai~ s(DIAMETER) + s(cr, k = 9) + s(bal.pl.ratio) + s(ccf.pl)+
                      s(asp_sin, asp_cos)+ s(slope_pct)+s(percent_LAOC),
                    random=list(unique_tree_id=~1),niterPQL = 80,
                    family = 'Gamma'(link = log),
                    data = bai.spmx.ids, method = 'ML')
save(re.tree.p8a,file=file.path(getwd(),"data","re.tree.p8a.Rdata"))
rm(re.tree.p8a)

re.tree.p9a <- gamm(bai~ s(DIAMETER) + s(cr, k = 9) + s(bal.pl.ratio) + s(ccf.pl)+
                      s(asp_sin, asp_cos)+ s(slope_pct)+s(shade.tol.pl),
                    random=list(unique_tree_id=~1),
                    family = 'Gamma'(link = log),
                    data = bai.spmx.ids, method = 'ML')
save(re.tree.p9a,file=file.path(getwd(),"data","re.tree.p9a.Rdata"))
rm(re.tree.p9a)


modls <- c(1:9)
resa <- matrix(nrow=0,ncol=8)
for (i in modls){
  modn <- load(file.path(getwd(),"data",paste("re.tree.p",i,"a.Rdata",sep="")))
  modl <- get(modn)
  summ <- summary(modl$gam)
  out <- c(i,modl$gam$df.null,modl$gam$df.residual,sum(summ$edf),
           sum(residuals(modl$gam)^2),
           summ$r.sq,AIC(modl$lme),logLik(modl$lme))
  resa <- rbind(resa,out)
  rm(list=c(modn)); rm(modl,summ)
}
colnames(resa) <- c("fit","df.null","df.res","edf",
                   "dev.res",
                   "rsq","aic","loglik")
resa

load(file=file.path(getwd(),"data","re.tree.p7a.Rdata"))
plot(re.tree.p7a$gam,select=1)


