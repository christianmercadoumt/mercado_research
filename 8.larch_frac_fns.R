library(tidyverse)

larch.fraction.plot <- function(forestdata){
  a <- forestdata %>% 
    mutate(SPECIES_SYMBOL = as.factor(SPECIES_SYMBOL)) %>% 
    group_by(SETTING_ID, PLOT, MEASUREMENT_NO) %>% 
    mutate(larch.ba.fraction.pl = (sum(treeba[SPECIES_SYMBOL == "LAOC"], na.rm = T))/(sum(treeba, na.rm = T))) %>% 
    ungroup()
  return(a)
}


larch.fraction.clu <- function(forestdata){
  a <- forestdata %>% 
    mutate(SPECIES_SYMBOL = as.factor(SPECIES_SYMBOL)) %>% 
    group_by(SETTING_ID, cluster, MEASUREMENT_NO) %>% 
    mutate(larch.ba.fraction.cl = (sum(treeba[SPECIES_SYMBOL == "LAOC"], na.rm = T))/(sum(treeba, na.rm = T))) %>% 
    ungroup()
  return(a)
}

shade <- readRDS("C:/git/research/mercado_research/data/shade.rds")

shade.tolerance.plot <- function(forestdata, na.rm = TRUE){
  a <- forestdata %>% 
    mutate(SPECIES_SYMBOL = as.factor(SPECIES_SYMBOL)) %>%
    left_join(x = ., y = shade, by = "SPECIES_SYMBOL") %>% 
    group_by(SETTING_ID, PLOT, MEASUREMENT_NO) %>% 
    mutate(shade.tol.pl = sum(treeba*inv.shade, na.rm = na.rm)/sum(treeba, na.rm = na.rm)) %>% 
    select(!inv.shade) %>% 
    ungroup()
  return(a)
}

shade.tolerance.cluster <- function(forestdata, na.rm = TRUE){
  a <- forestdata %>% 
    mutate(SPECIES_SYMBOL = as.factor(SPECIES_SYMBOL)) %>%
    left_join(x = ., y = shade, by = "SPECIES_SYMBOL") %>% 
    group_by(SETTING_ID, cluster, MEASUREMENT_NO) %>% 
    mutate(shade.tol.cl = sum(treeba*inv.shade, na.rm = na.rm)/sum(treeba, na.rm = na.rm)) %>% 
    select(!inv.shade) %>% 
    ungroup()
  return(a)
}

shade.tolerance.plot.cluster <- function(forestdata, na.rm = TRUE){
  a <- forestdata %>% 
    mutate(SPECIES_SYMBOL = as.factor(SPECIES_SYMBOL)) %>%
    left_join(x = ., y = shade, by = "SPECIES_SYMBOL") %>% 
    group_by(SETTING_ID, cluster, MEASUREMENT_NO) %>% 
    mutate(shade.tol.cl = sum(treeba*inv.shade, na.rm = na.rm)/sum(treeba, na.rm = na.rm)) %>% 
    ungroup() %>% 
    group_by(SETTING_ID, PLOT, MEASUREMENT_NO) %>% 
    mutate(shade.tol.pl = sum(treeba*inv.shade, na.rm = na.rm)/sum(treeba, na.rm = na.rm)) %>% 
    select(!inv.shade) %>% 
    ungroup()
  return(a)
}