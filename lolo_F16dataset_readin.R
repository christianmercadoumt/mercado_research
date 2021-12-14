require(tidyverse)

# read each file using read.csv to address read/class errors
lolo_f16_raw <- read.csv("data/other/FSVeg_data_F16.csv",
                       colClasses = c("numeric",rep("character",2),"numeric","character",
                                      rep("numeric",2),rep("character",2), rep("numeric",3),
                                      rep("character",2), rep("numeric",5),"character", rep("numeric",3),
                                      rep("character",11)))

#cleaning up dates, adding column with proper date format, then year
lolo16 <- mutate(lolo_f16_raw, mdate = as.Date(MEASUREMENT_DATE, format = "%m/%d/%Y"))
lolo16 <- dplyr::mutate(lolo16, myear = format(mdate, format = "%Y"))

# add cluster variable
lolo16 <- dplyr::mutate(lolo16, cluster = 100*floor(PLOT/100))

# select just the interesting variables
lolo16 <- dplyr::select(lolo16, SETTING_ID, mdate, myear, MEASUREMENT_NO, 
                     PURPOSE_CODE, cluster, PLOT, SUBPLOT, LIVE_DEAD, SPECIES_SYMBOL, 
                     DIAMETER, TPA_EQUIV, BASAL_AREA_EQUIV, HEIGHT, CROWN_RATIO, 
                     CROWN_CLASS, AGE, RADIAL_GROWTH, HEIGHT_GROWTH)
