### Some functions for gams
require(mgcv)
#### TRASP - Topographic solar radiation index. This linearizes aspect. 0 means north-northeast aspect, 1 means south-southwest aspect. (Roberts and Cooper, 1989, page90-) 'Concepts and Techniques of Vegetation Mapping.' - In 'Land Classifications Based on Vegetation'

trasp <- function(degrees_aspect){
  (1-cos((pi/180)*(degrees_aspect-30)))/2
}

#standard rmse, and rmse.df
my.rmse.2 <- function(gam){
  res <- residuals.gam(gam, type = 'response')
  res.sq <- res^2
  rmse <- sqrt(mean(res.sq))
  rmse.df <- sqrt(sum(res.sq)/(gam$df.residual))
  return(c('rmse' = rmse, 'rmse.df' = rmse.df))
}

#output which returns list (based on list of gam objects) of summary, rmse, and concurvity for each input gam object
gam.output <- function(x){
  x <- list('summary' = summary.gam(x), 'rmse' = my.rmse.2(x), 'concurvity' = concurvity(x))
}

#my.rmse.xval <- function(gam, testdata){
#  prd <- predict.gam(gam, newdata = testdata, type = 'response')
#  data.t <- testdata$bai
#  rmse <- sqrt(mean((prd-data.t)^2))
#  return(rmse)
#}

#gam.things <- function(gamobject, choose.fn){


#  plot.1 <- plot.gam(gamobject, scheme = 1, pages = 1, scale = 0)
#  rmse <- my.rmse.2(gamobject)
#  gamcheck <- function(x){
#    par(mfrow = c(2,2))
#    gam.check(x)
#  }
#  gamcheck <- gamcheck(gamobject)
  
#  if('plot' %in% choose.fn) {
#    plot.1} else {
#      return('no plot')}
#  if('rmse' %in% choose.fn) {
#    rmse} else {
#      return('no rmse')}
#  if('gamcheck' %in% choose.fn) {
#    return(gamcheck)} else {
#      return('no gam check')}
#  
#}

#gam.things(gamobject = c(gam.asp1, gam.asp2), choose.fn = 'plot')