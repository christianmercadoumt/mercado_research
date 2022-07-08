library(tidyverse)

#larch.fraction.plot <- function(forestdata){
 # a <- forestdata %>% 
  #  mutate(SPECIES_SYMBOL = as.factor(SPECIES_SYMBOL)) %>% 
  #  group_by(SETTING_ID, PLOT, MEASUREMENT_NO) %>% 
  #  mutate(larch.ba.fraction.pl = (sum(BASAL_AREA_EQUIV[SPECIES_SYMBOL == "LAOC" & TPA_EQUIV != 0], na.rm = T))/(sum(treeba[TPA_EQUIV != 0], na.rm = T))) %>% 
  #  ungroup()
#  return(a)
#}

larch.fraction.plot <- function(forestdata){
  a <- forestdata %>% 
    mutate(SPECIES_SYMBOL = as.factor(SPECIES_SYMBOL)) %>% 
    group_by(SETTING_ID, PLOT, MEASUREMENT_NO) %>% 
    mutate(larch.ba.fraction.pl = (sum(BASAL_AREA_EQUIV.pl[SPECIES_SYMBOL == "LAOC" & TPA_EQUIV != 0], 
                                       na.rm = T))/(sum(BASAL_AREA_EQUIV.pl[TPA_EQUIV != 0], na.rm = T))) %>% 
    ungroup()
  return(a)
}


#### ADD SPECIES PERCENTS FOR EVERY SPECIES - added 7/7/22
spp.frac <- function(forestdata, species){

  a <- forestdata %>% 
    mutate(SPECIES_SYMBOL = as.factor(SPECIES_SYMBOL)) %>% 
    group_by(SETTING_ID, PLOT, MEASUREMENT_NO) %>% 
    mutate('percent_{species}' := round(((sum(BASAL_AREA_EQUIV.pl[SPECIES_SYMBOL == species & TPA_EQUIV != 0], 
                                       na.rm = T))/(sum(BASAL_AREA_EQUIV.pl[TPA_EQUIV != 0], na.rm = T))),5)) %>% 
    ungroup()
  return(a)
}

spp.frac.all <- function(data){
  main.spp <- c('LAOC', 'PSME', 'PIEN', 'ABLA', 'PICO', 'PIPO', 'ABGR')
  a <- data
  for(i in main.spp){
    a <- a %>% spp.frac(i)
  }
  a <- a %>% mutate(percent_other = round((1-(percent_LAOC + percent_PSME + 
                                                percent_PIEN + percent_ABLA + 
                                                percent_PICO + percent_PIPO + 
                                                percent_ABGR)),3))
  return(a)
}




# spp.frac <- function(forestdata, species){
#   a <- forestdata %>% 
#     mutate(SPECIES_SYMBOL = as.factor(SPECIES_SYMBOL)) %>% 
#     group_by(SETTING_ID, PLOT, MEASUREMENT_NO) %>% 
#     mutate(!!species := (sum(BASAL_AREA_EQUIV.pl[SPECIES_SYMBOL == species & TPA_EQUIV != 0], 
#                                        na.rm = T))/(sum(BASAL_AREA_EQUIV.pl[TPA_EQUIV != 0], na.rm = T))) %>% 
#     ungroup()
#   return(a)
# }

#this is an example of how to name something with an argument
# meanofcol <- function(df, col) {
#   mutate(df, "Mean of {{col}}" := mean({{col}}))
# }
# meanofcol(iris, Petal.Width)

dom.spp.ba <- function(forestdata){
  a <- forestdata %>% 
    group_by(SETTING_ID, PLOT, MEASUREMENT_NO, SPECIES_SYMBOL) %>%
    mutate(spp.ba.pl = sum(BASAL_AREA_EQUIV.pl[TPA_EQUIV != 0], na.rm = T)) %>% 
    ungroup() %>% 
    group_by(SETTING_ID, PLOT, MEASUREMENT_NO) %>% 
    mutate(dom.spp.pl.ba = SPECIES_SYMBOL[which.max(spp.ba.pl)]) %>% 
    ungroup()
  return(a)
}
## sum of BA equivalents
## TPA larch fraction - larch fraction variable that differentiates itself from shade-tolerance. 

#arch.fraction.clu <- function(forestdata){
#  a <- forestdata %>% 
#    mutate(SPECIES_SYMBOL = as.factor(SPECIES_SYMBOL)) %>% 
#    group_by(SETTING_ID, cluster, MEASUREMENT_NO) %>% 
#    mutate(larch.ba.fraction.cl = (sum(treeba[SPECIES_SYMBOL == "LAOC" & TPA_EQUIV != 0], na.rm = T))/(sum(treeba[TPA_EQUIV != 0], na.rm = T))) %>% 
#    ungroup()
#  return(a)
#}

larch.fraction.clu <- function(forestdata){
  a <- forestdata %>% 
    mutate(SPECIES_SYMBOL = as.factor(SPECIES_SYMBOL)) %>% 
    group_by(SETTING_ID, cluster, MEASUREMENT_NO) %>% 
    mutate(larch.ba.fraction.cl = (sum(BASAL_AREA_EQUIV.cl[SPECIES_SYMBOL == "LAOC" & TPA_EQUIV != 0], na.rm = T))/(sum(BASAL_AREA_EQUIV.cl[TPA_EQUIV != 0], na.rm = T))) %>% 
    ungroup()
  return(a)
}

larch.frac.tpa <- function(forestdata){
  a <- forestdata %>% 
    mutate(SPECIES_SYMBOL = as.factor(SPECIES_SYMBOL)) %>% 
    group_by(SETTING_ID, PLOT, MEASUREMENT_NO) %>% 
    mutate(larch.tpa.fraction.pl = (sum(TPA_EQUIV.pl[SPECIES_SYMBOL == "LAOC" & TPA_EQUIV != 0], na.rm = T))/(sum(TPA_EQUIV.pl[TPA_EQUIV != 0], na.rm = T))) %>% 
    ungroup()
  return(a)
}

shade <- readRDS("data/shade.rds")

#shade.tolerance.plot <- function(forestdata, na.rm = TRUE){
#  a <- forestdata %>% 
#    mutate(SPECIES_SYMBOL = as.factor(SPECIES_SYMBOL)) %>%
#    left_join(x = ., y = shade, by = "SPECIES_SYMBOL") %>% 
#    group_by(SETTING_ID, PLOT, MEASUREMENT_NO) %>% 
#    mutate(shade.tol.pl = sum(treeba[TPA_EQUIV != 0]*inv.shade, na.rm = na.rm)/sum(treeba[TPA_EQUIV != 0], na.rm = na.rm)) %>% 
#    select(!inv.shade) %>% 
#    ungroup()
#  return(a)
#}

#shade.tolerance.cluster <- function(forestdata, na.rm = TRUE){
#  a <- forestdata %>% 
#    mutate(SPECIES_SYMBOL = as.factor(SPECIES_SYMBOL)) %>%
#    left_join(x = ., y = shade, by = "SPECIES_SYMBOL") %>% 
#    group_by(SETTING_ID, cluster, MEASUREMENT_NO) %>% 
#    mutate(shade.tol.cl = sum(treeba[TPA_EQUIV != 0]*inv.shade, na.rm = na.rm)/sum(treeba[TPA_EQUIV != 0], na.rm = na.rm)) %>% 
#    select(!inv.shade) %>% 
#    ungroup()
#  return(a)
#}

shade.tolerance.plot.cluster <- function(forestdata, na.rm = TRUE){
  a <- forestdata %>% 
    mutate(SPECIES_SYMBOL = as.factor(SPECIES_SYMBOL)) %>%
    left_join(x = ., y = shade, by = "SPECIES_SYMBOL") %>% 
    group_by(SETTING_ID, cluster, MEASUREMENT_NO) %>% 
    mutate(shade.tol.cl = sum(BASAL_AREA_EQUIV.cl[TPA_EQUIV != 0]*inv.shade[TPA_EQUIV != 0], na.rm = na.rm)/sum(BASAL_AREA_EQUIV.cl[TPA_EQUIV != 0], na.rm = na.rm)) %>% 
    ungroup() %>% 
    group_by(SETTING_ID, PLOT, MEASUREMENT_NO) %>% 
    mutate(shade.tol.pl = sum(BASAL_AREA_EQUIV.pl[TPA_EQUIV != 0]*inv.shade[TPA_EQUIV != 0], na.rm = na.rm)/sum(BASAL_AREA_EQUIV.pl[TPA_EQUIV != 0], na.rm = na.rm)) %>% 
    select(!inv.shade) %>% 
    ungroup()
  return(a)
}