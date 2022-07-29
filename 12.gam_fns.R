### Some functions for gams
require(mgcv)
require(tidyverse)

cm.sq <- 2.54^2
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

my.rmse.3 <- function(model, testdata){
  error <- testdata$bai-as.vector(predict.gam(model, newdata = testdata, type = 'response', exclude = 's(unique.tree.f)'))
  mse <- (sum(error^2))/length(error)
  rmse <- sqrt(mse)
  return(rmse)
}

#output which returns list (based on list of gam objects) of summary, rmse, and concurvity for each input gam object
gam.output <- function(x){
  x <- list('summary' = summary.gam(x), 'rmse' = my.rmse.2(x), 'concurvity' = concurvity(x))
}

rmse.conc <- function(x){
  rmse.df <- my.rmse.2(x)[[2]]
  concurve <- concurvity(x)
  concurve.test <- ifelse(any(concurve[3,]>=0.8), 'high-concurvity', 'low-concurvity') 
  rmse.conc.ls <- list('rmse.df' = rmse.df, 
                       'concurvity' = concurve, 
                       'concurvity.test' = concurve.test)
  
  return(rmse.conc.ls)
}

## PRE-REQUISITE to using this function - recommended that there is a list of gam objects with names
## without names for gam objects in list, there won't be names in the table
rmse.conc.table <- function(gam.list){
  newlist <- lapply(gam.list, rmse.conc) #get rmse, concurvity, and concurvity eval
  rmse.vec <- vector() #empy vector
  conc.vec <- vector() #empty vector
  
  for(i in 1:length(newlist)){
    rmse.vec[i] <- newlist[[i]][[1]]
    conc.vec[i] <- newlist[[i]][[3]]
  }
  tibble('model' = names(newlist), 'rmse' = rmse.vec, 'concurvity.eval' = conc.vec) %>% 
    arrange(rmse) %>% 
    mutate(rmse.rank = row_number())
}

#compare partial response curves between two gams - best used when gams have same vars
gam.visual.comp <- function(gam1, gam2){
  for(i in 1:length(term_names(spmx.re.gam.lf))){
    par(mfrow = c(1,2))
    plot.gam(gam1, rug = F, residuals = F, ylim = c(-4, 2.5), scheme = 1, select = i, main = deparse(quote(gam1)))
    plot.gam(gam2, rug = F, residuals = F, ylim = c(-4, 2.5), scheme = 1, select = i, main = deparse(quote(gam2)))
  }
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