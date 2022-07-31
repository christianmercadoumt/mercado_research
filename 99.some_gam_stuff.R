
### data and stuff (copied from 12 and 12a)
require(mgcv)
require(tidyverse)
library(lattice)

trasp <- function(degrees_aspect){
  (1-cos((pi/180)*(degrees_aspect-30)))/2
}

hab_loc_codes <- read_csv('data/habtypes.csv')
hab_loc_codes <- hab_loc_codes %>% select(SETTING_ID, habclass, locationcode)

bai.train <- readRDS('data/bai.train.7_11_22.rds')
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


# Nested random effects
re_test0 <- gamm(bai~ s(DIAMETER) + s(cr, k = 9) ,
                random=list(unique_tree_id=~1),
                family = 'Gamma'(link = log),
                data = bai.spmx.ids, method = 'ML')
bwplot(residuals(re_test0$gam,type="response") ~ bai.spmx.ids$unique.plot.f)
bwplot(residuals(re_test0$gam,type="response") ~ bai.spmx.ids$unique.cluster.f)
bwplot(residuals(re_test0$gam,type="response") ~ bai.spmx.ids$SETTING_ID)

re_test1 <- gamm(bai~ s(DIAMETER) + s(cr, k = 9) ,
                    random=list(unique.plot.f=~1,unique_tree_id=~1),
                    family = 'Gamma'(link = log),
                    data = bai.spmx.ids, method = 'ML')
bwplot(residuals(re_test1$gam,type="response") ~ bai.spmx.ids$unique.plot.f)
bwplot(residuals(re_test1$gam,type="response") ~ bai.spmx.ids$unique.cluster.f)
bwplot(residuals(re_test1$gam,type="response") ~ bai.spmx.ids$SETTING_ID)

AIC(re_test0$lme)
AIC(re_test1$lme)



# Predictions
load("data/re.tree.p7.Rdata")
re.tree.p7$call

some_values <- data.frame(DIAMETER=c(3,4,5),
                          cr=rep(.8,3),
                          bal.pl.ratio=rep(.6,3),
                          ccf.pl=rep(50,3),
                          asp_sin=rep(sin(pi*45/180),3),
                          asp_cos=rep(cos(pi*45/180),3),
                          slope_pct=rep(25,3))
some_values$unique_tree_id <- 3
predict(re.tree.p7,newdata=some_values,type="response")
predict(re.tree.p7,newdata=some_values,type="response",exclude="s(unique_tree_id)")

some_values$unique_tree_id <- 10
predict(re.tree.p7,newdata=some_values,type="response")
predict(re.tree.p7,newdata=some_values,type="response",exclude="s(unique_tree_id)")

# so...either set "unique_tree_id" to a value that is not in the original fit
#  and endure a "warning message", or use the exclude= approach



# Standard errors
some_values$unique_tree_id <- 3
link_level <- predict(re.tree.p7,newdata=some_values,type="link",se.fit=TRUE)
resp_level <- predict(re.tree.p7,newdata=some_values,type="response",se.fit=TRUE)

exp(link_level$fit)
resp_level$fit  # same! Good

link_level$se.fit*exp(link_level$fit)
resp_level$se.fit # same! Good. Look up "delta method" if you want to see why

# so...use type="response" in all your predict statements to get the proper
#      scale of predictions and standard errors (i.e. both in squared inch per year)




# Nested models and use of by statement
bai.spmx.ids$mixture <- with(bai.spmx.ids,
                             ifelse(percent_LAOC>.95,"WL",
                                    ifelse(percent_LAOC>.25 & percent_PSME>.25 &
                                             (percent_LAOC+percent_PSME>.8),"WLDF",
                                           ifelse(percent_LAOC>.25 & percent_PICO>.25 &
                                                    (percent_LAOC+percent_PICO>.8),"WLPL",
                                                  "other"))))
bai.spmx.ids$mixture <- as.factor(bai.spmx.ids$mixture)

just_mix <- droplevels(bai.spmx.ids[bai.spmx.ids$mixture!="other",])


# fit some pretty rudimentary models; one simple (global), the other less-so (by mixture)
global_fit <- gam(bai ~ s(DIAMETER) + 
                     s(cr, k = 9), 
                   family = Gamma(link = log), data = just_mix, method = "ML")
separate_fit <- gam(bai ~ s(DIAMETER,by=just_mix$mixture) + 
                       s(cr, k = 9,by=just_mix$mixture) ,
                     family = Gamma(link = log), data = just_mix, method = "ML")

# look at partials
par(mfcol=c(2,2))
plot(global_fit,select=1,main="global",ylim=c(-2,22))
plot(separate_fit,select=1,main="sep",ylim=c(-2,22))
plot(separate_fit,select=2,main="sep",ylim=c(-2,22))
plot(separate_fit,select=3,main="sep",ylim=c(-2,22))
par(mfcol=c(1,1))

# compare log-likelihoods, aic
logLik.gam(global_fit)
logLik.gam(separate_fit) # log likelihood is larger for separate fit, as it uses more parameters

AIC(global_fit)
AIC(separate_fit) # based on AIC, the increase in parameters is worth it; the penalty
                   # doesn't overwhelm improved fit - go with separate model


# likelihood ratio test
# Null hypothesis: global fit suffices (can restrict partial curves to a single
#                                       curve, as opposed to mixture-specific curves)
# Alt hypothesis: separate fit is needed (mixture-specific curves are important)
lr_test_stat <- -2*(logLik.gam(global_fit) - logLik.gam(separate_fit))
diff_df <- sum(separate_fit$edf) - sum(global_fit$edf) # separate fit uses 14.1 more df
1-pchisq(lr_test_stat,df=diff_df) # p-value
# if the p-value is small (it is here ~ 0), then we reject the null and conclude
#  that the alternative (separate fits) is needed
