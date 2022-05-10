#### RESULTS ####

source('12a.models_fit_read.R')

## SIZE

model <- c('null', 'size', 'density.comp', 'site')

rmse.selected <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(gam.s1a)[[2]], NA, NA)
rmse.best1 <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(gam.s1a)[[2]],NA, NA)
fvs <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(fvs.size1)[[2]], NA, NA)

tibble(model, rmse.selected, rmse.best1, fvs)

### Size partial response


### Size RMSE table 
model <- c('null', 'size', 'density.comp', 'site', 'random.tree', 'larch_fraction.random.tree', 'shade.tol.random.tree')

rmse.selected <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(gam.s1a)[[2]],NA, NA, NA,NA, NA)

rmse.sel.alt <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(gam.s1a)[[2]], rep(NA, 5))

rmse.best1 <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(gam.s1a)[[2]],rep(NA, 5))

fvs <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(fvs.size1)[[2]], rep(NA, 5))

rmse_size <- tibble(model, rmse.selected, rmse.sel.alt, rmse.best1, fvs)

### Density-competiton partial response


### Density-competition RMSE
model <- c('null', 'size', 'density.comp', 'site', 'random.tree', 'larch_fraction.random.tree', 'shade.tol.random.tree')

rmse.selected <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(gam.s1a)[[2]], my.rmse.2(compd.pl4)[[2]], rep(NA, 4))

rmse.sel.alt <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(gam.s1a)[[2]], my.rmse.2(compd.ba1)[[2]],rep(NA, 4))

rmse.best1 <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(gam.s1a)[[2]], my.rmse.2(best.6a)[[2]], rep(NA, 4))

fvs <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(fvs.size1)[[2]], my.rmse.2(fvs.sz.cmp)[[2]], rep(NA, 4))

rmse_dcomp <- tibble(model, rmse.selected, rmse.sel.alt, rmse.best1, fvs)

### Site partial response


### Site rmse table - full base model
model <- c('null', 'size', 'density.comp', 'site', 'random.tree', 'larch_fraction.random.tree', 'shade.tol.random.tree')

rmse.selected <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(gam.s1a)[[2]], my.rmse.2(compd.pl4)[[2]], my.rmse.2(site5a.2)[[2]], rep(NA, 3))

rmse.sel.alt <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(gam.s1a)[[2]], my.rmse.2(compd.ba1)[[2]], my.rmse.2(site.n5b)[[2]], rep(NA, 3))

rmse.best1 <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(gam.s1a)[[2]], my.rmse.2(best.6a)[[2]], my.rmse.2(site.n5a)[[2]], NA, NA, NA)

fvs <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(fvs.size1)[[2]], my.rmse.2(fvs.sz.cmp)[[2]], my.rmse.2(fvs.full)[[2]], NA, NA, NA)

rmse_site <- tibble(model, rmse.selected, rmse.sel.alt, rmse.best1, fvs)


### Spp. mix partial response


### spp. mix RMSE

model <- c('null', 'size', 'density.comp', 'site', 'random.tree', 'larch_fraction.random.tree', 'shade.tol.random.tree')

rmse.selected <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(gam.s1a)[[2]], my.rmse.2(compd.pl4)[[2]], my.rmse.2(site5a.2)[[2]], my.rmse.2(re.tree.1)[[2]], my.rmse.2(spmx.re.gam.lf)[[2]], my.rmse.2(spmx.re.gam.st)[[2]])

rmse.sel.alt <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(gam.s1a)[[2]], my.rmse.2(compd.ba1)[[2]], my.rmse.2(site.n5b)[[2]], my.rmse.2(re.tree.1alt)[[2]], my.rmse.2(spmx.gam.alt.lf)[[2]], my.rmse.2(spmx.gam.alt.st)[[2]])

rmse.best1 <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(gam.s1a)[[2]], my.rmse.2(best.6a)[[2]], my.rmse.2(site.n5a)[[2]], NA, NA, NA)

fvs <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(fvs.size1)[[2]], my.rmse.2(fvs.sz.cmp)[[2]], my.rmse.2(fvs.full)[[2]], NA, NA, NA)

rmse_spp_mix <- tibble(model, rmse.selected, rmse.sel.alt, rmse.best1, fvs)







#####NOTE: FIGURE OUT how to plot curves without plot.gam - use ggplot


