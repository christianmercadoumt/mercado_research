## RESULTS ####

source('12.gam_fns.R')
source('12a.models_fit_read.R')
library(gratia)
library(grid)
library(gridExtra)

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
