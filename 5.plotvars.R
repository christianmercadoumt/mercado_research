#variable calculations - plot level
ccf_coef <- read_csv('data/ccf_species_coefficients.csv')

variables.plot <- function(forestdata, na.rm = TRUE){
  a <- forestdata %>% #call data
    left_join(x = ., y = ccf_coef, by = "SPECIES_SYMBOL") %>%  #This step adds the ccf coefficients from FVS-IE literature for the purpose of calculating CCF
    group_by(SETTING_ID, PLOT, MEASUREMENT_NO) %>% #group to the plot level for plot-level calculations
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
