## RESULTS ####

source('12.gam_fns.R')
#source('12a.models_fit_read.R')
library(gratia)
library(grid)
library(gridExtra)


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

# table from ex.abs####

# re.tree.1alt <- readRDS('data/model_objects.1/re.tree.1alt.rds')
# spmx.gam.alt.lf <- readRDS('data/model_objects.1/spmx.gam.alt.lf_7.11.rds')
# spmx.gam.alt.st <- readRDS('data/model_objects.1/spmx.gam.alt.st.rds')
# nosize.base <- readRDS('data/model_objects.1/nosize.base.RDS')
# nocomp.base <- readRDS('data/model_objects.1/nocomp.base.RDS')
# nosite.base <- readRDS('data/model_objects.1/nosite.base.RDS')
# size <- gam(bai~s(DIAMETER), family = 'Gamma'(link = log), data = bai.spmx.ids, method = 'ML') 
# compd.ba <- gam(bai~s(DIAMETER) + s(cr, k = 9) + s(bal.pl.ratio) + s(ba.pl), 
#                  family = 'Gamma'(link = log), data = bai.spmx.ids, method = 'ML')
# site.n5b <- update(compd.ba, . ~ . + s(asp_sin, asp_cos))
# 
# dev.expl <- function(model){(model$null.deviance-model$deviance)/model$null.deviance}
# 
# library(mgcv)
# 
# a <- summary(re.tree.1alt)
# b <- summary(spmx.gam.alt.lf)
# c <- summary(spmx.gam.alt.st)
# 
# re.edf <- edf(re.tree.1alt)
# lf.edf <- edf(spmx.gam.alt.lf)
# st.edf <- edf(spmx.gam.alt.st)
# base.edf <- sum(re.edf$edf)
# base.sz.edf <- re.edf$edf[1]
# base.dcomp.edf <- sum(re.edf$edf[c(2,3,4)])
# base.sit.edf <- sum(re.edf$edf[c(5,6)])
# a$chi.sq
# 
# lf.alledf <- sum(lf.edf$edf)
# lf.sz.edf <- lf.edf$edf[1]
# lf.dcomp.edf <- sum(lf.edf$edf[c(2,3,4)])
# lf.sit.edf <- sum(lf.edf$edf[c(5,6)])
# lf.lf <- lf.edf$edf[8]
# 
# st.alledf <- sum(st.edf$edf)
# st.sz.edf <- st.edf$edf[1]
# st.dcomp.edf <- sum(st.edf$edf[c(2,3,4)])
# st.sit.edf <- sum(st.edf$edf[c(5,6)])
# st.st <- st.edf$edf[8]
# 
# models <- c('Full model', 'Size', 'Comp.', 'Site', 'RE', 'WL ratio', 'Shade')
# EDF <- c(base.edf, base.sz.edf, base.dcomp.edf, base.sit.edf, edf(re.tree.1alt, 's(unique.tree.f)')$edf, lf.lf, st.st)
# 
# dev <- c(deviance(re.tree.1alt), deviance(re.tree.1alt)-c(deviance(nosize.base), deviance(nocomp.base), deviance(nosite.base)), deviance(spmx.gam.alt.lf), deviance(spmx.gam.alt.st))
# 
# dev <- 100*c(dev.expl(re.tree.1alt), dev.expl(size), dev.expl(compd.ba)-dev.expl(size), dev.expl(site.n5b)-dev.expl(compd.ba), dev.expl(re.tree.1alt)-dev.expl(site.n5b), dev.expl(spmx.gam.alt.lf), dev.expl(spmx.gam.alt.st))
# cm.sq <- 2.54^2
# rmse.mods <- c(my.rmse.2(re.tree.1alt)[[2]]*cm.sq, rep(NA, 4), my.rmse.2(spmx.gam.alt.lf)[[2]]*cm.sq, my.rmse.2(spmx.gam.alt.st)[[2]]*cm.sq)
# tb1 <- tibble(models, EDF, dev, rmse.mods) %>% rename('Model'= models, "EDF" = EDF,'Deviance Explained (%)'= dev, 'Model RMSE (cm\u00B2/yr.)' = rmse.mods)
# saveRDS(tb1, 'data/rmse_edf_dev_table.Rds')
tb1 <- readRDS('data/rmse_edf_dev_table.Rds')

kb1 <- tb1 %>% 
  kable(format = 'latex', booktabs = T, digits = c(1, 0, 2, 5), escape = T) %>% 
  kable_styling(latex_options = c('striped', 'hold_position'), full_width = T, font_size = 8, table.envir = 'float') %>% 
  group_rows('Species-mix', start_row = 6, end_row = 7) %>% 
  add_indent(c(2,3,4,5)) %>% 
  row_spec(1, bold = T) %>% 
  column_spec(1, width = '8em', latex_valign = 'm') %>% 
  column_spec(2, bold = ifelse(EDF > 1234, TRUE, FALSE), width = '4em', latex_valign = 'm') %>% 
  column_spec(3, bold = ifelse(dev > 80, T, F), width = '5em', latex_valign = 'm') %>% 
  column_spec(4, bold = T, width = '5em', latex_valign = 'm') %>% 
  gsub('NA', '', .)



# SIZE ####

model <- c('null', 'size', 'density.comp', 'site')

rmse.selected <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(gam.s1a)[[2]], NA, NA)
rmse.best1 <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(gam.s1a)[[2]],NA, NA)
fvs <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(fvs.size1)[[2]], NA, NA)

#tibble(model, rmse.selected, rmse.best1, fvs)

### Size partial response #### 

sm.est.size <- smooth_estimates(gam.s1a) %>% add_confint()

# gg.sz.diam <- ggplot(sm.est.size) + 
#   geom_ribbon(aes(ymin = exp(lower_ci), ymax = exp(upper_ci), x = DIAMETER), alpha = 0.2) + 
#   geom_line(aes(x = DIAMETER, y = exp(est)))
# dbh.size <- draw(gam.s1a, rug = F, fun = exp)

### Size RMSE table ####
model <- c('null', 'size', 'density.comp', 'site', 'random.tree', 'larch_fraction.random.tree', 'shade.tol.random.tree')

rmse.selected <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(gam.s1a)[[2]],NA, NA, NA,NA, NA)

rmse.sel.alt <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(gam.s1a)[[2]], rep(NA, 5))

rmse.best1 <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(gam.s1a)[[2]],rep(NA, 5))

fvs <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(fvs.size1)[[2]], rep(NA, 5))

rmse_size <- tibble(model, rmse.selected, rmse.sel.alt, rmse.best1, fvs)

### Density-competiton partial response ####

sm.est.dcomp <- smooth_estimates(compd.pl4) %>% add_confint()

# gg.dcomp.diam <- ggplot(sm.est.dcomp) + 
#   geom_ribbon(aes(ymin = exp(lower_ci), ymax = exp(upper_ci), x = DIAMETER), alpha = 0.2) + 
#   geom_line(aes(x = DIAMETER, y = exp(est)))
# gg.dcomp.cr <- ggplot(sm.est.dcomp) +
#   geom_ribbon(aes(ymin = exp(lower_ci), ymax = exp(upper_ci), x = cr), alpha = 0.2) + 
#   geom_line(aes(x = cr, y = exp(est)))
# gg.dcomp.bal <- ggplot(sm.est.dcomp) +
#   geom_ribbon(aes(ymin = exp(lower_ci), ymax = exp(upper_ci), x = bal.pl.ratio), alpha = 0.2) + 
#   geom_line(aes(x = bal.pl.ratio, y = exp(est)))
# gg.dcomp.ccf <- ggplot(sm.est.dcomp) +
#   geom_ribbon(aes(ymin = exp(lower_ci), ymax = exp(upper_ci), x = ccf.pl), alpha = 0.2) + 
#   geom_line(aes(x = ccf.pl, y = exp(est)))

# dbh.dcomp <- draw(compd.pl4, rug = F, fun = exp, select = 1)
# cr.dcomp <- draw(compd.pl4, rug = F, fun = exp, select = 2)
# bal.dcomp <- draw(compd.pl4, rug = F, fun = exp, select = 3)
# ccf.dcomp <- draw(compd.pl4, rug = F, fun = exp, select = 4)
# 
# dbh.dcomp.a <- draw(compd.ba1, rug = F, fun = exp, select = 1)
# cr.dcomp.a <- draw(compd.ba1, rug = F, fun = exp, select = 2)
# bal.dcomp.a <- draw(compd.ba1, rug = F, fun = exp, select = 3)
# ba.dcomp.a <- draw(compd.ba1, rug = F, fun = exp, select = 4)


### Density-competition RMSE ####
model <- c('null', 'size', 'density.comp', 'site', 'random.tree', 'larch_fraction.random.tree', 'shade.tol.random.tree')

rmse.selected <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(gam.s1a)[[2]], my.rmse.2(compd.pl4)[[2]], rep(NA, 4))

rmse.sel.alt <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(gam.s1a)[[2]], my.rmse.2(compd.ba1)[[2]],rep(NA, 4))

rmse.best1 <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(gam.s1a)[[2]], my.rmse.2(best.6a)[[2]], rep(NA, 4))

fvs <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(fvs.size1)[[2]], my.rmse.2(fvs.sz.cmp)[[2]], rep(NA, 4))

rmse_dcomp <- tibble(model, rmse.selected, rmse.sel.alt, rmse.best1, fvs)

### Site partial response ####
# 
# dbh.site <- draw(site5a.2, rug = F, fun = exp, select = 1)
# cr.site <- draw(site5a.2, rug = F, fun = exp, select = 2)
# bal.site <- draw(site5a.2, rug = F, fun = exp, select = 3)
# ccf.site <- draw(site5a.2, rug = F, fun = exp, select = 4)
# asp.site <- draw(site5a.2, rug = F, fun = exp, select = 5)
# sl.site <- draw(site5a.2, rug = F, fun = exp, select = 6)
# 
# dbh.site.a <- draw(site.n5b, rug = F, fun = exp, select = 1)
# cr.site.a <- draw(site.n5b, rug = F, fun = exp, select = 2)
# bal.site.a <- draw(site.n5b, rug = F, fun = exp, select = 3)
# ba.site.a <- draw(site.n5b, rug = F, fun = exp, select = 4)
# asp.site.a <- draw(site.n5b, rug = F, fun = exp, select = 5)

### Site rmse table - full base model ####
model <- c('null', 'size', 'density.comp', 'site', 'random.tree', 'larch_fraction.random.tree', 'shade.tol.random.tree')

rmse.selected <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(gam.s1a)[[2]], my.rmse.2(compd.pl4)[[2]], my.rmse.2(site5a.2)[[2]], rep(NA, 3))

rmse.sel.alt <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(gam.s1a)[[2]], my.rmse.2(compd.ba1)[[2]], my.rmse.2(site.n5b)[[2]], rep(NA, 3))

rmse.best1 <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(gam.s1a)[[2]], my.rmse.2(best.6a)[[2]], my.rmse.2(site.n5a)[[2]], NA, NA, NA)

fvs <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(fvs.size1)[[2]], my.rmse.2(fvs.sz.cmp)[[2]], my.rmse.2(fvs.full)[[2]], NA, NA, NA)

rmse_site <- tibble(model, rmse.selected, rmse.sel.alt, rmse.best1, fvs)


### RE models partial response ####

# dbh.re <- draw(re.tree.1, rug = F, fun = exp, select = 1)
# cr.re <- draw(re.tree.1, rug = F, fun = exp, select = 2)
# bal.re <- draw(re.tree.1, rug = F, fun = exp, select = 3)
# ccf.re <- draw(re.tree.1, rug = F, fun = exp, select = 4)
# asp.re <- draw(re.tree.1, rug = F, fun = exp, select = 5)
# sl.re <- draw(re.tree.1, rug = F, fun = exp, select = 6)
# re.re <- draw(re.tree.1, rug = F, fun = exp, select = 7)
# 
# dbh.re.a <- draw(re.tree.1alt, rug = F, fun = exp, select = 1)
# cr.re.a <- draw(re.tree.1alt, rug = F, fun = exp, select = 2)
# bal.re.a <- draw(re.tree.1alt, rug = F, fun = exp, select = 3)
# ba.re.a <- draw(re.tree.1alt, rug = F, fun = exp, select = 4)
# asp.re.a <- draw(re.tree.1alt, rug = F, fun = exp, select = 5)
# re.re.a <- draw(re.tree.1alt, rug = F, fun = exp, select = 6)

### RE models RMSE ####

model <- c('null', 'size', 'density.comp', 'site', 'random.tree', 'larch_fraction.random.tree', 'shade.tol.random.tree')

rmse.selected <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(gam.s1a)[[2]], my.rmse.2(compd.pl4)[[2]], my.rmse.2(site5a.2)[[2]], my.rmse.2(re.tree.1)[[2]], NA, NA)

rmse.sel.alt <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(gam.s1a)[[2]], my.rmse.2(compd.ba1)[[2]], my.rmse.2(site.n5b)[[2]], my.rmse.2(re.tree.1alt)[[2]], NA, NA)

rmse.best1 <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(gam.s1a)[[2]], my.rmse.2(best.6a)[[2]], my.rmse.2(site.n5a)[[2]], NA, NA, NA)

fvs <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(fvs.size1)[[2]], my.rmse.2(fvs.sz.cmp)[[2]], my.rmse.2(fvs.full)[[2]], NA, NA, NA)

rmse_re <- tibble(model, rmse.selected, rmse.sel.alt, rmse.best1, fvs)

### Spp. mix partial response ####

# dbh.lf <- draw(spmx.re.gam.lf, rug = F, fun = exp, select = 1)
# cr.lf <- draw(spmx.re.gam.lf, rug = F, fun = exp, select = 2)
# bal.lf <- draw(spmx.re.gam.lf, rug = F, fun = exp, select = 3)
# ccf.lf <- draw(spmx.re.gam.lf, rug = F, fun = exp, select = 4)
# asp.lf <- draw(spmx.re.gam.lf, rug = F, fun = exp, select = 5)
# sl.lf <- draw(spmx.re.gam.lf, rug = F, fun = exp, select = 6)
# re.lf <- draw(spmx.re.gam.lf, rug = F, fun = exp, select = 7)
# lf.lf <- draw(spmx.re.gam.lf, rug = F, fun = exp, select = 8)
# 
# dbh.st <- draw(spmx.re.gam.st, rug = F, fun = exp, select = 1)
# cr.st <- draw(spmx.re.gam.st, rug = F, fun = exp, select = 2)
# bal.st <- draw(spmx.re.gam.st, rug = F, fun = exp, select = 3)
# ccf.st <- draw(spmx.re.gam.st, rug = F, fun = exp, select = 4)
# asp.st <- draw(spmx.re.gam.st, rug = F, fun = exp, select = 5)
# sl.st <- draw(spmx.re.gam.st, rug = F, fun = exp, select = 6)
# re.st <- draw(spmx.re.gam.st, rug = F, fun = exp, select = 7)
# st.st <- draw(spmx.re.gam.st, rug = F, fun = exp, select = 8)


# dbh.lf.a <- draw(spmx.gam.alt.lf, rug = F, fun = exp, select = 1)
# cr.lf.a <- draw(spmx.gam.alt.lf, rug = F, fun = exp, select = 2)
# bal.lf.a <- draw(spmx.gam.alt.lf, rug = F, fun = exp, select = 3)
# ba.lf.a <- draw(spmx.gam.alt.lf, rug = F, fun = exp, select = 4)
# asp.lf.a <- draw(spmx.gam.alt.lf, rug = F, fun = exp, select = 5)
# re.lf.a <- draw(spmx.gam.alt.lf, rug = F, fun = exp, select = 6)
# lf.lf.a <- draw(spmx.gam.alt.lf, rug = F, fun = exp, select = 7)
# 
# dbh.st.a <- draw(spmx.gam.alt.st, rug = F, fun = exp, select = 1)
# cr.st.a <- draw(spmx.gam.alt.st, rug = F, fun = exp, select = 2)
# bal.st.a <- draw(spmx.gam.alt.st, rug = F, fun = exp, select = 3)
# ba.st.a <- draw(spmx.gam.alt.st, rug = F, fun = exp, select = 4)
# asp.st.a <- draw(spmx.gam.alt.st, rug = F, fun = exp, select = 5)
# re.st.a <- draw(spmx.gam.alt.st, rug = F, fun = exp, select = 6)
# st.st.a <- draw(spmx.gam.alt.st, rug = F, fun = exp, select = 7) 

lf.spmx <- smooth_estimates(spmx.re.gam.lf) %>% add_confint()
st.spmx <- smooth_estimates(spmx.re.gam.st) %>% add_confint()
    
### spp. mix RMSE ####

model <- c('Null', 'Size', 'Density/Comp', 'Site', 'Random tree', 'Larch Fraction w/ RE', 'Shade tolerance w/ RE')

rmse.selected <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(gam.s1a)[[2]], my.rmse.2(compd.pl4)[[2]], my.rmse.2(site5a.2)[[2]], my.rmse.2(re.tree.1)[[2]], my.rmse.2(spmx.re.gam.lf)[[2]], my.rmse.2(spmx.re.gam.st)[[2]])

rmse.sel.alt <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(gam.s1a)[[2]], my.rmse.2(compd.ba1)[[2]], my.rmse.2(site.n5b)[[2]], my.rmse.2(re.tree.1alt)[[2]], my.rmse.2(spmx.gam.alt.lf)[[2]], my.rmse.2(spmx.gam.alt.st)[[2]])

rmse.best1 <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(gam.s1a)[[2]], my.rmse.2(best.6a)[[2]], my.rmse.2(site.n5a)[[2]], NA, NA, NA)

fvs <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(fvs.size1)[[2]], my.rmse.2(fvs.sz.cmp)[[2]], my.rmse.2(fvs.full)[[2]], NA, NA, NA)

rmse_spp_mix <- tibble(model, rmse.selected, rmse.sel.alt, rmse.best1, fvs)


### FVS visuals #### 

ggplot(data = bai.spmx.ids, aes(DIAMETER, log.bai)) + 
  stat_function(data = subset(bai.site.ids, habclass == 2), 
                fun = function(x){dcof[[1]] + dcof[[4]] + log(x)*dcof[[2]] + dcof[[3]]*x^2}) + 
  stat_function(data = subset(bai.site.ids, habclass == 3), 
                fun = function(x){dcof[[1]] + dcof[[5]] + log(x)*dcof[[2]] + dcof[[3]]*x^2}) + 
  stat_function(data = subset(bai.site.ids, habclass == 5), 
                fun = function(x){dcof[[1]] + dcof[[6]] + log(x)*dcof[[2]] + dcof[[3]]*x^2}) +
  geom_point(data = bai.spmx.ids, aes(DIAMETER, log.bai), alpha = 0.2)

## extended abstract things ####

#larch fraction
lf.smest <- smooth_estimates(spmx.re.gam.lf, smooth = 's(larch.ba.fraction.pl)') %>% add_confint()

st.smest <- smooth_estimates(spmx.re.gam.st, smooth = 's(shade.tol.pl)') %>% add_confint()

f1a <- ggplot(lf.smest, aes(x = larch.ba.fraction.pl, y = cm.sq*exp(est)-cm.sq)) + 
  geom_ribbon(alpha = 0.2, aes(ymin = cm.sq*exp(lower_ci)-cm.sq, ymax = cm.sq*exp(upper_ci)-cm.sq, x = larch.ba.fraction.pl)) + 
  geom_line() +
  labs(x = 'Larch proportion', y = NULL) +
  scale_y_continuous(breaks = seq(-2, 8, by = 2)) + geom_hline(yintercept = 0, linetype = 'dashed')
#shade tolerance
f1b <- ggplot(st.smest, aes(x = shade.tol.pl, y = cm.sq*exp(est)-cm.sq)) +
  geom_ribbon(alpha = 0.2, aes(ymin = cm.sq*exp(lower_ci)-cm.sq, ymax = cm.sq*exp(upper_ci)-cm.sq, x = shade.tol.pl)) + 
  geom_line() +
  labs(x = 'Shade intolerance', y = NULL) +
  scale_y_continuous(breaks = seq(-2, 8, by = 2)) +
  scale_x_continuous(n.breaks = 5) + geom_hline(yintercept = 0, linetype = 'dashed')

a <- summary(re.tree.1)
b <- summary(spmx.re.gam.lf)
c <- summary(spmx.re.gam.st)
d <- summary(site.n5a)
RMSE <- c(my.rmse.2(site.n5a)[[2]], my.rmse.2(re.tree.1)[[2]], my.rmse.2(spmx.re.gam.lf)[[2]], my.rmse.2(spmx.re.gam.st)[[2]])
deviance.explained <- 100*c(d$dev.expl, a$dev.expl, b$dev.expl, c$dev.expl)
models <- c('Base model', 'Base model w/ RE', 'Larch proportion', 'Shade-tolerance')
