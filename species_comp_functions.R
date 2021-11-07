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
    mutate(cluster_BA = sum(BASAL_AREA_EQUIV))
      #BA = sum(BASAL_AREA_EQUIV))
  return(a)
}

species_comp_all_ba(kt1)
a <- a %>% group_by(SETTING_ID, MEASUREMENT_NO, cluster) %>%
    mutate(composition = BA/sum(BA))




#### IN PROGRESS 
#Function that creates a tibble which includes the percent of cluster BA/acre that each unique species occpuies (at a given measurement/year). Large trees only.

species_comp_largetrees_ba <- function(forestdata){
  a <- forestdata %>% filter(is.na(SUBPLOT)) %>% 
    group_by(SETTING_ID, MEASUREMENT_NO, myear, cluster, SPECIES_SYMBOL) %>% 
    summarise(species_BA = sum(BASAL_AREA_EQUIV)) %>%
    mutate(tot_ba = sum(species_BA), percent_BA = round((species_BA/tot_ba)*100, 2))
  
  return(a)
  
}
#test
dd <- species_comp_largetrees_ba(kt1)


# Plotting percent composition of Larch vs. Other species within a cluster at a given measurement and for each stand (in this case, for kootenai data.). This allows us to see composition over time. 

#PURPOSE - To display composition of larch for every stand of interest, for each cluster, over time.

#NOTE: working to make this prettier
#NOTE: OMIT the measurement number 0 - why is it there?
#NOTE: How can this better capture what I'm trying to do?

ggplot(dd, aes(x = cluster, y = percent_BA, fill = as.factor(SPECIES_SYMBOL == "LAOC"))) + geom_bar(position = "stack", stat = "identity") + facet_grid(vars(SETTING_ID), vars(MEASUREMENT_NO)) + theme(legend.position = "none")

# test the lolo. Perhaps repeat steps above. OR create a dataset which joins the two forests' data and then run the fuctions/plotting on that dataset, but somehow make a distinction between lolo and kootenai. 
species_comp_largetrees_ba(lolo1)
  