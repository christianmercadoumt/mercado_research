##Create and separate out hold-out data set for model validation. 
source('9.bai_data.v2.R')

#set seed for reproducibility
set.seed(301)

#Identify sample plots within separate data frame
sample.data <- bai.data %>% group_by(SETTING_ID) %>% #call data and group to settings
  summarize(PLOT = unique(PLOT)) %>% #identify plots
  slice_sample(n = 2) %>%  #then sample 2 plots for each setting id
  mutate(sample = "sample") %>% ungroup() #add variable identifying samples

bai.data.full <- left_join(bai.data, sample.data) #add sample variable to full data set based on matching setting id and plot. NA's produced for non-matches.

#Check equality - make sure it works..
a <- bai.data.full %>% filter(sample == 'sample') %>% group_by(SETTING_ID, PLOT, sample) %>% summarize()
all_equal(sample.data,a)

#separate data sets
bai.hold <- bai.data.full %>% filter(sample == 'sample')
bai.train <- bai.data.full %>% filter(is.na(sample))

#save RDS files - change date every time this is run
# saveRDS(bai.hold, 'data/bai.hold.7_7_22.rds') 
# saveRDS(bai.train, 'data/bai.train.7_7_22.rds') 
#7/7/22 - added function and calculations for percent of each species - modified the following: 8.larch_frac_fns.R, 9.bai_data.v2.R
#7522 - added ccf variable that takes species identity out of the equation (used average of all parameters (r1-r5))