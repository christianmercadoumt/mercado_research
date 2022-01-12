#variable calculations - cluster level
ccf_coef <- read_csv('data/ccf_species_coefficients.csv')

# What happens in the species join (line 9) if a species is not in ccf_coef (eg TSME)?
#  does it delete the record, or keep it but assign it NA coefficeints?

variables.cluster <- function(forestdata, na.rm = TRUE){
  a <- forestdata %>% #call data
    left_join(x = ., y = ccf_coef, by = "SPECIES_SYMBOL") %>%  #This step adds the ccf coefficients from FVS-IE literature for the purpose of calculating CCF
    group_by(SETTING_ID, cluster, MEASUREMENT_NO) %>% #group to the plot level for plot-level calculations
    mutate(ba.pl = sum(BASAL_AREA_EQUIV, na.rm = na.rm), #plot basal area calculations - sum of BA over plot
           tpa.pl = n()*20, #plot tpa - (number of large trees) x (expansion factor)
           bal.pl = map_dbl(BASAL_AREA_EQUIV, ~sum(BASAL_AREA_EQUIV[BASAL_AREA_EQUIV>.x], na.rm = na.rm)), #plot-level BAL - used basal area/acre here. This step performs the specified calculation across each record within the plot grouping. Thus, this step sums the basal areas for trees based on the condition that they are greater than the basal area of the observed record
           dq.pl = DIAMETER/sqrt(sum(DIAMETER^2, na.rm = na.rm)/sum(!is.na(DIAMETER))), #DBH:QMD ratio calculated
           ccf.tree = case_when(
             (SPECIES_SYMBOL %in% c('POTR5', 'BEPA', 'TABR2', 'ACGL') & DIAMETER >= 1.0) ~ r1 + (r2*DIAMETER) + (r3*(DIAMETER^2)),
             (SPECIES_SYMBOL %in% c('POTR5', 'BEPA', 'TABR2', 'ACGL') & DIAMETER < 1.0) ~ r4*DIAMETER^(r5),
             DIAMETER >= 10.0 ~ r1 + (r2*DIAMETER) + (r3*(DIAMETER^2)),
             DIAMETER < 10.0 ~ r4*DIAMETER^(r5))) %>% #This step adds tree crown competition factor based on a series of conditionals which determine which equation/coefficients to use - this step could probably be simplified with custom funcions
    mutate(ccf.pl = sum(ccf.tree, na.rm = na.rm)) %>% #sum of ccf to get plot-level ccf value. 
    select(!c(r1, r2, r3, r4, r5, ccf.tree, spcode)) %>% 
    ungroup() #housekeeping step
  return(a)
}



test_case <- pgp_data1[pgp_data1$SETTING_ID=="01140109010001" & 
                         pgp_data1$MEASUREMENT_NO==1 &
                         pgp_data1$cluster==100,]
variables.cluster(test_case)[1,20:24]

#  DA: either the input must be pre-filtered to live trees, or the function
#  should add a "LIVE" filter
sum(test_case$BASAL_AREA_EQUIV,na.rm=T)
sum(test_case$TPA_EQUIV,na.rm=T)
sum(test_case$BASAL_AREA_EQUIV[test_case$LIVE_DEAD=="L"],na.rm=T)
sum(test_case$TPA_EQUIV[test_case$LIVE_DEAD=="L"],na.rm=T)

#  DA: for QMD, replace the current expression with sqrt(576*(ba.pl)/(tpa.pl)/pi) because
#  that will allow for trees with different TPA_EQUIV

#  DA: also, sum of ccf should be sum(ccf.tree*TPA_EQUIV,... to account for sampling intensity

# DA: also, may need to bring in the sample design info here. If a measurement
#  in 1990 used a min diameter of 1.0" and then a measurement in 1995 used
#  a min diameter of 3.0", the BA total may appear to decline when in fact this
#  is simply a result of changing thresholds.