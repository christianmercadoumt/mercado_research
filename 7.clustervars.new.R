#variable calculations - cluster level

#Revisit
variables.cluster <- function(forestdata, na.rm = TRUE){

  a <- forestdata %>% 
    filter(LIVE_DEAD == 'L') %>% #Live trees only
    group_by(SETTING_ID, cluster, MEASUREMENT_NO) %>% #Group by cluster
    mutate(tpa.cl = mean(tpa.pl), #average of plot tpas for all plots in cluster
           ba.cl = mean(ba.pl), # average ba fo all plots in cluster
           ccf.cl = sum(ccf.tree*TPA_EQUIV.cl, na.rm = na.rm), # ccf value for cluster
           bal.cl = map_dbl(BASAL_AREA_EQUIV.cl,
                            ~sum(BASAL_AREA_EQUIV.cl[BASAL_AREA_EQUIV.cl>.x], na.rm = na.rm))) %>% # BAL cluster level
    mutate(dq.cl = DIAMETER/sqrt(576*(ba.cl)/(tpa.cl)/pi)) %>%  # DBH:QMD ratio
    ungroup()
  return(a)
}

