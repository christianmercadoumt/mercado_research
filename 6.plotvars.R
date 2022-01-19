#variable calculations - plot level
ccf_coef <- read_csv('data/ccf_species_coefficients.csv')

variables.plot <- function(forestdata, na.rm = TRUE){
  a <- forestdata %>% #call data
    filter(LIVE_DEAD == 'L') %>% #Live trees only
    left_join(x = ., y = ccf_coef, by = "SPECIES_SYMBOL") %>%  #This step adds the ccf coefficients from FVS-IE literature for the purpose of calculating CCF
    group_by(SETTING_ID, PLOT, MEASUREMENT_NO) %>% #group to the plot level for plot-level calculations
    mutate(ba.pl = sum(BASAL_AREA_EQUIV.pl, na.rm = na.rm), #plot basal area calculations - sum of BA over plot
           tpa.pl = sum(TPA_EQUIV.pl, na.rm = na.rm), #plot tpa - (number of large trees) x (expansion factor)
           bal.pl = map_dbl(BASAL_AREA_EQUIV.pl, ~sum(BASAL_AREA_EQUIV.pl[BASAL_AREA_EQUIV.pl>.x], na.rm = na.rm)), #plot-level BAL - used basal area/acre here. This step performs the specified calculation across each record within the plot grouping. Thus, this step sums the basal areas for trees based on the condition that they are greater than the basal area of the observed record
           ccf.tree = case_when(
             (SPECIES_SYMBOL %in% c('POTR5', 'BEPA', 'TABR2', 'ACGL') & DIAMETER >= 1.0) ~ r1 + (r2*DIAMETER) + (r3*(DIAMETER^2)),
             (SPECIES_SYMBOL %in% c('POTR5', 'BEPA', 'TABR2', 'ACGL') & DIAMETER < 1.0) ~ r4*DIAMETER^(r5),
             DIAMETER >= 10.0 ~ r1 + (r2*DIAMETER) + (r3*(DIAMETER^2)),
             DIAMETER < 10.0 ~ r4*DIAMETER^(r5))) %>% #This step adds tree crown competition factor based on a series of conditionals which determine which equation/coefficients to use - this step could probably be simplified with custom funcions
    mutate(ccf.pl = sum(ccf.tree*TPA_EQUIV.pl, na.rm = na.rm),#sum of ccf to get plot-level ccf value. 
           dq.pl = DIAMETER/sqrt(576*(ba.pl)/(tpa.pl)/pi)) %>% #DBH:QMD ratio
    select(!c(r1, r2, r3, r4, r5, spcode)) %>% 
    ungroup() #housekeeping step
  return(a)
}




## Need to - Account for sampling design - diameter thresholds - see below


# DA: also, may need to bring in the sample design info here. If a measurement
#  in 1990 used a min diameter of 1.0" and then a measurement in 1995 used
#  a min diameter of 3.0", the BA total may appear to decline when in fact this
#  is simply a result of changing thresholds.


### CM Notes:
### bal.pl includes any tree that has a diameter - i.e., it includes subplot trees
### 