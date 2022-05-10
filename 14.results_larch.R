#### RESULTS ####

source('12a.models_fit_read.R')

## SIZE

model <- c('null', 'size', 'density.comp', 'site')

rmse.selected <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(gam.s1a)[[2]], NA, NA)
rmse.best1 <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(gam.s1a)[[2]],NA, NA)
fvs <- c(my.rmse.2(gam.null)[[2]], my.rmse.2(fvs.size1)[[2]], NA, NA)

tibble(model, rmse.selected, rmse.best1, fvs)

### Size partial response




#####NOTE: FIGURE OUT how to plot curves without plot.gam - use ggplot


