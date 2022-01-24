#variable calculations - cluster level

#Revisit
variables.cluster <- function(forestdata, na.rm = TRUE){

  a <- forestdata %>% 
    filter(LIVE_DEAD == 'L') %>% #Live trees only
    group_by(SETTING_ID, cluster, MEASUREMENT_NO) %>% #Group by cluster
    mutate(tpa.cl.all = mean(tpa.pl.all), #average of plot tpas for all plots in cluster
           tpa.cl.cutoff = mean(tpa.pl.cutoff), #average of plot tpas with 4.5 cutoff
           ba.cl = mean(ba.pl), # average ba fo all plots in cluster
           ccf.cl = sum(ccf.tree*TPA_EQUIV.cl, na.rm = na.rm), # ccf value for cluster
           bal.cl = map_dbl(BASAL_AREA_EQUIV.cl,
                            ~sum(BASAL_AREA_EQUIV.cl[BASAL_AREA_EQUIV.cl>.x], na.rm = na.rm))) %>% # BAL cluster level
    mutate(dq.cl.all = DIAMETER/sqrt(576*(ba.cl)/(tpa.cl.all)/pi),
           dq.cl.cutoff = DIAMETER/sqrt(576*(ba.cl)/(tpa.cl.cutoff)/pi)) %>%  # DBH:QMD ratio
    ungroup()
  return(a)
}

