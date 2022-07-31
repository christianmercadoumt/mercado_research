library(xtable)
library(tidyverse)
source('12.gam_fns.R')

cm.sq <- 2.54^2
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

#Table of # of plots, # of trees, # of observations, other conditions ####

num.pl <- bai.spmx.ids %>% group_by(stand) %>% 
  summarize(number.plots = n_distinct(PLOT), 
            number.trees = n_distinct(unique_tree_id), 
            number.obs = n(), 
            n.meas = 1+n_distinct(MEASUREMENT_NO))

xtable(num.pl)

bai.spmx.ids %>% filter(stand == 1401) %>% summarize(unique(PLOT))
# Table of all variables considered, by group, and how they were calculated (?)####
# Initial size variables considered were tree diameter at breast 
# height (DBH) and tree basal area ($BA_t$). 
# Density and competition variables considered were: plot basal area (BA), 
# basal area larger than the 
# subject tree (BAL; expressed as a ratio of plot basal area), 
# plot quadratic mean diameter (QMD), ratio of tree DBH to QMD ($D_q$), and trees per acre (tpa).
# Site variables initially considered were: slope, aspect, elevation, 
# mean site index (CITE), and heatload

sizevars <- c('Diameter at breast height (DBH)', 'Tree basal area')
denscompvars <- c('Plot basal area (BA)', 
                  'Ratio of basal area larger than the subject tree to plot basal area (BAL)',
                  'Plot quadratic mean diameter (QMD)', 
                  'Ratio of DBH to QMD (D_q)', 
                  'Trees per acre (TPA)')
sitevars <- c('Slope', 'Aspect', 'Elevation', 'Mean site index (SI)', 'Heatload')



#table of shade intolerance values ####
shade <- read_csv('shadetolerance.small.csv')
shade <- shade %>% mutate(id = c(1:nrow(shade)))

mytbl <- tibble(COMMON_NAME = character(),
                'GENUS' = character(),
                'SPECIES' = character(),
                'Shade Tolerance Index' = numeric(),
                'SPCD' = numeric(),
                'id' = numeric())

for(i in 1:nrow(shade)){
  a <- filter(shade, id== i)
  mytbl <- rbind(mytbl, a)
}

shade <- mytbl[c(2, 4, 6, 9, 11, 13, 14, 16, 19, 22),]
shade <- shade %>% select(GENUS, SPECIES, `Shade Tolerance Index`) %>% 
  rename(c('Genus' = GENUS, 'Species' = SPECIES, 
           'Shade Tolerance' = `Shade Tolerance Index`)) %>% 
  arrange(`Shade Tolerance`)
shadetoltbl <- xtable(shade)

shadetoltbl <- shade %>%
  kable(format = 'latex', booktabs = T) %>% #caption = 'Shade tolerance for western conifers (ref:caption2)',
  kable_styling(latex_options = c('striped', 'HOLD_position'), full_width = F) %>%
  column_spec(c(1,2), italic = TRUE)



# Table of RMSE and Dev. expl. values through model selection process #### 

#need to rework so that it shows RMSEs for each step leading up to base model

# this is what i used in the abstract
re.tree.1 <- readRDS('data/model_objects.1/re.tree.1ba_7.14.rds')
# spmx.re.gam.lf <- readRDS('data/model_objects.1/spmx.re.gam.lf.rds')
# spmx.re.gam.st <- readRDS('data/model_objects.1/spmx.re.gam.st.rds')
# nosize.base <- readRDS('data/model_objects.1/nosize.base.RDS')
# nocomp.base <- readRDS('data/model_objects.1/nocomp.base.RDS')
# nosite.base <- readRDS('data/model_objects.1/nosite.base.RDS')
size <- gam(bai~s(DIAMETER), family = 'Gamma'(link = log), 
            data = bai.spmx.ids, method = 'ML') 
compd <- gam(bai~s(DIAMETER) + s(bal.pl.ratio) + s(cr, k = 9) + 
               s(ba.pl), family = 'Gamma'(link = log), 
             data = bai.spmx.ids, method = 'ML') 
site <- gam(bai~s(DIAMETER) + s(bal.pl.ratio) + s(cr, k = 9) + 
              s(ba.pl) + s(asp_cos, asp_sin), 
            family = 'Gamma'(link = log), data = bai.spmx.ids, method = 'ML') 

a <- summary(re.tree.1)
# b <- readRDS('data/model_objects.1/summary_spmx.re.gam.lf.rds')
# c <- readRDS('data/model_objects.1/summary_spmx.re.gam.st.rds')

no.re.model <- readRDS('data/model_objects.1/site5a.2.rds')



dev.expl <- function(model){(model$null.deviance-model$deviance)/model$null.deviance}

re.edf <- edf(re.tree.1)
# lf.edf <- edf(spmx.re.gam.lf)
# st.edf <- edf(spmx.re.gam.st)

base.edf <- sum(re.edf$edf)
base.sz.edf <- re.edf$edf[1]
base.dcomp.edf <- sum(re.edf$edf[c(2,3,4)])
base.sit.edf <- sum(re.edf$edf[5])
a$chi.sq
re.edf$edf



sum(size$edf)
sum(compd$edf)
sum(site$edf)


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

smooth <- c('DBH', 'crown ratio', 'BAL', 
            'BA', 'aspect', 'tree random intercept', 'full model')#, 'WL ratio', 'Shade')
EDF <- rbind(edf(re.tree.1)[,2], sum(edf(re.tree.1)[,2]))

#EDF <- c(base.edf, base.sz.edf, base.dcomp.edf, base.sit.edf, edf(re.tree.1, 's(unique.tree.f)')$edf)#, lf.lf, st.st)

#dev <- c(deviance(re.tree.1), deviance(re.tree.1)-c(deviance(nosize.base), deviance(nocomp.base), deviance(nosite.base)), deviance(spmx.re.gam.lf), deviance(spmx.re.gam.st))

#dev <- 100*c(dev.expl(re.tree.1), dev.expl(size), dev.expl(compd)-dev.expl(size), dev.expl(site)-dev.expl(compd), dev.expl(re.tree.1)-dev.expl(site))#, dev.expl(spmx.re.gam.lf), dev.expl(spmx.re.gam.st))
dev <- c(dev.expl(size),
         dev.expl(compd), NA, NA,
         dev.expl(site), NA,
         dev.expl(re.tree.1))*100

rmse.mods <- c(my.rmse.2(size)[[2]]*cm.sq, 
               my.rmse.2(compd)[[2]]*cm.sq, NA, NA, 
               my.rmse.2(site)[[2]]*cm.sq, NA,
               my.rmse.2(re.tree.1)[[2]]*cm.sq)#, my.rmse.2(spmx.re.gam.lf)[[2]]*cm.sq, my.rmse.2(spmx.re.gam.st)[[2]]*cm.sq)

tb1 <- tibble(smooth, EDF, dev, rmse.mods) #%>% rename('Smooth'= smooth, "EDF" = EDF,'Deviance Explained (%)'= dev, 'Model RMSE (cm^2/yr.)' = rmse.mods)

xtable(tb1)
library(kableExtra)

kb1 <- tb1 %>% 
  kable(format = 'latex', booktabs = T, digits = c(1, 0, 2, 5)) %>% 
  kable_styling(latex_options = c('striped', 'hold_position'), full_width = F) %>% 
  add_indent(c(2,3,4,5)) %>% 
  row_spec(7, bold = T) %>% 
  group_rows('Size', 1, 1) %>% 
  group_rows('Competition & density', 2, 4) %>% 
  group_rows('Site', 5, 5) %>% 
  column_spec(1, width = '8em', latex_valign = 'm') %>% 
  column_spec(2, width = '4em', latex_valign = 'm') %>% 
  column_spec(3, width = '5em', latex_valign = 'm') %>% 
  column_spec(4, width = '5em', latex_valign = 'm') %>% 
  gsub('NA', '', .)

# \begin{table}[!h]
# \label{tab:basemod}
# \centering
# \begin{tabular}{>{\raggedright\arraybackslash}m{8em}>{\raggedleft\arraybackslash}m{4em}>{\raggedleft\arraybackslash}m{5em}>{\raggedleft\arraybackslash}m{5em}}
# \toprule
# Model & EDF & Deviance Explained (\%) & Model RMSE ($cm^2/yr.$)\\
# \midrule
# \hspace{1em}Size & 8 & 30.82 & \textbf{}\\
# \hspace{1em}\cellcolor{gray!6}{Comp.} & \cellcolor{gray!6}{18} & \cellcolor{gray!6}{32.05} & \textbf{\cellcolor{gray!6}{}}\\
# \hspace{1em}Site & 26 & 4.52 & \textbf{}\\
# \hspace{1em}\cellcolor{gray!6}{RE} & {\cellcolor{gray!6}{1337}} & \cellcolor{gray!6}{17.52} & \textbf{\cellcolor{gray!6}{}}\\
# \textbf{\cellcolor{gray!6}{Full model}} & \textbf{\textbf{\cellcolor{gray!6}{1389}}} & \textbf{\textbf{\cellcolor{gray!6}{84.91}}} & \textbf{\textbf{\cellcolor{gray!6}{3.32142}}}\\
# \bottomrule
# \end{tabular}
# \end{table}

shade %>%
  kable(format = 'latex', booktabs = T) %>% #caption = 'Shade tolerance for western conifers (ref:caption2)',
  kable_styling(latex_options = c('striped', 'HOLD_position'), full_width = F) %>%
  column_spec(c(1,2), italic = TRUE)
#tb1 <- readRDS('data/rmse_edf_dev_table.Rds')
tb1 %>% filter()
# 


# RMSE, dev, edf for ccf, purity, shade tol. ####

library(kableExtra)
basemodel <- readRDS('data/model_objects.1/re.tree.1ba_7.14.rds') # change to 'data/model_objects.1/re.tree.1ba_7.14.rds'
ccf.mod <- readRDS('data/model_objects.1/spmx.gam.alt.ccf_7.14.rds') #shade tolerance is in here when it shouldnt be
purity <- readRDS('data/model_objects.1/spmx.gam.alt.lf_7.14.rds')
shade.intol <- readRDS('data/model_objects.1/spmx.gam.alt.st_7.14.rds')

hold.data <- readRDS('data/bai.hold.7_14_22.rds')
hab_loc_codes <- read_csv('data/habtypes.csv')
hab_loc_codes <- hab_loc_codes %>% select(SETTING_ID, habclass, locationcode)
bai.hold <- left_join(hold.data, hab_loc_codes, by = "SETTING_ID")
test.data <- bai.hold %>% 
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
  filter(bai != 0) %>% 
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

levels(shade.intol$model$unique.tree.f)

## THESE RMSE values are based on predictions that DO NOT INCLUDE random intercepts because 
## the test data set has completely different trees in it. Would it make more sense just to fit something 
## with a random effect at the stand level?
rmse_values <- c(my.rmse.3(basemodel, test.data)*2.54^2,
                 my.rmse.3(ccf.mod, test.data)*2.54^2, 
                 my.rmse.3(shade.intol, test.data)*2.54^2, 
                 my.rmse.3(purity, test.data)*2.54^2)

rmse_values.df <- c(my.rmse.2(basemodel)[[2]],
                  my.rmse.2(ccf.mod)[[2]],
                  my.rmse.2(shade.intol)[[2]],
                  my.rmse.2(purity)[[2]])*cm.sq


a <- summary(basemodel)
b <- summary(ccf.mod)
c <- summary(purity)
d <- summary(shade.intol)
dev.expl <- c(a$dev.expl, b$dev.expl, c$dev.expl, d$dev.expl)*100

r.sqr <- c(a$r.sq, b$r.sq, c$r.sq, d$r.sq)

model <- c('Base', 'CCF', 'Purity', 'Shade intol.')
options(pillar.sigfig = 4)
(mod.vals.tbl <- tibble(model, rmse_values, rmse_values.df, dev.expl, r.sqr))

mod.vals.tbl %>% 
  kable(format = 'latex', booktabs = T, digits = c(1, 0, 2, 5)) %>% 
  kable_styling(latex_options = c('striped', 'hold_position'), full_width = F)

#CCF params####

ccf_coef <- read_csv('data/ccf_species_coefficients.csv', show_col_types = F)

ccf_coef <- ccf_coef %>% select(SPECIES_SYMBOL, r1, r2, r3, r4, r5) %>% rename(SPECIES_SYMBOL = 'Species code')

ccf_coef %>% 
  kable(format = 'latex', booktabs = T) %>% 
  kable_styling(latex_options = c('striped', 'hold_position'), full_width = F)

pgpdata <- read_csv('data/pgp_data_all.csv')

unique(pgpdata$SPECIES_SYMBOL)

#mixture data summary#####

#function defining mixtures
spp.fun <- function(data.set, spp_min, spp_maximum, combined_min){
  a <- data.set %>% filter(percent_LAOC<spp_maximum & percent_LAOC>spp_min, 
                           percent_PSME<spp_maximum & percent_PSME>spp_min, 
                           (percent_LAOC+percent_PSME)>combined_min) %>% 
    mutate(other.pct = percent_PSME)
  b <- data.set %>% filter(percent_LAOC<spp_maximum & percent_LAOC>spp_min, 
                           percent_ABLA<spp_maximum & percent_ABLA>spp_min, 
                           (percent_LAOC+percent_ABLA)>combined_min) %>% 
    mutate(other.pct = percent_ABLA)
  c <- data.set %>% filter(percent_LAOC<spp_maximum & percent_LAOC>spp_min, 
                           percent_ABGR<spp_maximum & percent_ABGR>spp_min, 
                           (percent_LAOC+percent_ABGR)>combined_min) %>% 
    mutate(other.pct = percent_ABGR)
  d <- data.set %>% filter(percent_LAOC<spp_maximum & percent_LAOC>spp_min, 
                           percent_PIEN<spp_maximum & percent_PIEN>spp_min, 
                           (percent_LAOC+percent_PIEN)>combined_min) %>% 
    mutate(other.pct = percent_PIEN)
  e <- data.set %>% filter(percent_LAOC<spp_maximum & percent_LAOC>spp_min, 
                           percent_PICO<spp_maximum & percent_PICO>spp_min, 
                           (percent_LAOC+percent_PICO)>combined_min) %>% 
    mutate(other.pct = percent_PICO)
  f <- data.set %>% filter(percent_LAOC<spp_maximum & percent_LAOC>spp_min, 
                           percent_PIPO<spp_maximum & percent_PIPO>spp_min, 
                           (percent_LAOC+percent_PIPO)>combined_min) %>% 
    mutate(other.pct = percent_PIPO)
  g <- data.set %>% filter(percent_LAOC>0.9) %>% 
    mutate(other.pct = percent_LAOC)
  return(list('psme'=a, 'abla'=b, 'abgr'=c, 'pien'=d, 'pico'=e, 'pipo'=f, 'laoc' = g))
}

#actually define mixtures
(mixtures <- spp.fun(bai.spmx.ids, 0.2, .65, 0.7))

#add names to mixtures
mix.w.names <- map(names(mixtures), ~mixtures[[.x]] %>% mutate(mixtype = .x))
#combine data in list
mixtures.all <- bind_rows(mix.w.names)
#create factor
o.mixtures.all <- mixtures.all %>% 
  select(SETTING_ID, stand, PLOT, bai, DIAMETER, 
         cr, bal.pl.ratio, ba.pl, asp_sin, asp_cos, 
         mixtype, unique_tree_id) %>% 
  filter(mixtype %in% c('psme', 'pien', 'pico', 'laoc')) %>% 
  mutate(mixtype = as.factor(mixtype)) %>% 
  mutate(omixtype = ordered(mixtype, levels = c('laoc', 'pico', 'psme', 'pien')))

atable <- o.mixtures.all %>% group_by(omixtype) %>% 
  summarize(
    across(bai:ba.pl, 
           list(min = min, 
                max = max, 
                sd = sd, 
                mean = mean, 
                num.obs = length), 
           .names = "{.col}.{.fn}"), num.indivduals = n_distinct(unique_tree_id))

atable1 <- data.frame(t(as.data.frame(atable[-1])))
colnames(atable1) <- c('stat', 'laoc', 'pico', 'psme', 'pien')
atable2 <- atable1 %>% rownames_to_column()
(sum.table <- atable2 %>% rename(stat = rowname))
