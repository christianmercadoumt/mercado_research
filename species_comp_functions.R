## Function that provides a table displaying the proportion of each species for each cluster

#NOTE: forestdata should be the product of the readformat... for whichever forests data you're pulling from.

species_comp_all <- function(forestdata){
  a <- forestdata %>% 
    group_by(SETTING_ID, MEASUREMENT_NO, cluster, SPECIES_SYMBOL) %>% 
    summarise(species_count = n())
  
  a <- a %>% 
    group_by(SETTING_ID, MEASUREMENT_NO, cluster) %>% 
    mutate(sum = sum(species_count))
  
  a <- a %>% 
    mutate(composition = species_count/sum)
  
  a %>% 
    select(SETTING_ID, MEASUREMENT_NO, cluster, SPECIES_SYMBOL, composition)
}
#test
species_comp_all(kt1)

## Species comp for large trees only (excluding subplot data)

species_comp_largetrees <- function(forestdata){
  a <- forestdata %>% filter(is.na(SUBPLOT)) %>% 
    group_by(SETTING_ID, MEASUREMENT_NO, cluster, SPECIES_SYMBOL) %>% 
    summarise(species_count = n())
  
  a <- a %>% 
    group_by(SETTING_ID, MEASUREMENT_NO, cluster) %>% 
    mutate(sum = sum(species_count))
  
  a <- a %>% 
    mutate(composition = species_count/sum)
  
  a %>% 
    select(SETTING_ID, MEASUREMENT_NO, cluster, SPECIES_SYMBOL, composition)
}
#test
species_comp_largetrees(kt1)

## Species comp for larg trees only, by crown class

species_comp_lt_crown <- function(forestdata){
  a <- forestdata %>% 
    filter(is.na(SUBPLOT)) %>% 
    group_by(SETTING_ID, MEASUREMENT_NO, cluster, SPECIES_SYMBOL, CROWN_CLASS) %>% 
    summarise(species_count = n())
  
  a <- a %>% 
    group_by(SETTING_ID, MEASUREMENT_NO, cluster, CROWN_CLASS) %>% 
    mutate(sum = sum(species_count))
  
  a <- a %>% 
    mutate(composition = species_count/sum)
  
  a %>% 
    select(SETTING_ID, MEASUREMENT_NO, cluster, SPECIES_SYMBOL, CROWN_CLASS, composition)
}

#test
species_comp_lt_crown(kt1)

## Species comp - BA
#NOTE: 

species_comp_all_ba <- function(forestdata){
  a <- forestdata %>% 
    group_by(SETTING_ID, MEASUREMENT_NO, cluster, SPECIES_SYMBOL) %>% 
    summarise(BA = sum(BASAL_AREA_EQUIV))
}
a <- a %>% group_by(SETTING_ID, MEASUREMENT_NO, cluster) %>%
    mutate(composition = BA/sum(BA))
  