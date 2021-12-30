library(tidyverse)

larch.fraction.plot <- function(forestdata){
  a <- forestdata %>% 
    mutate(SPECIES_SYMBOL = as.factor(SPECIES_SYMBOL)) %>% 
    group_by(SETTING_ID, PLOT, MEASUREMENT_NO) %>% 
    mutate(larch.ba.fraction = (sum(treeba[SPECIES_SYMBOL == "LAOC"]))/(sum(treeba))) %>% 
    ungroup()
  return(a)
}


larch.fraction.clu <- function(forestdata){
  a <- forestdata %>% 
    mutate(SPECIES_SYMBOL = as.factor(SPECIES_SYMBOL)) %>% 
    group_by(SETTING_ID, cluster, MEASUREMENT_NO) %>% 
    mutate(larch.ba.fraction.cl = (sum(treeba[SPECIES_SYMBOL == "LAOC"]))/(sum(treeba))) %>% 
    ungroup()
  return(a)
}