require(tidyverse)

# read each file using read.csv to address read/class errors
kt_all_raw <- read.csv("data/other/FSVeg_data_F14.csv",
                        colClasses = c("numeric",rep("character",2),"numeric","character",
                                     rep("numeric",2),rep("character",2), rep("numeric",3),
                                     rep("character",2), rep("numeric",5),"character", rep("numeric",3),
                                     rep("character",11)))

#cleaning up dates, adding column with proper date format, then year
kt1 <- mutate(kt_all_raw, mdate = as.Date(MEASUREMENT_DATE, format = "%m/%d/%Y"))
kt1 <- dplyr::mutate(kt1, myear = format(mdate, format = "%Y"))

# add cluster variable
kt1 <- dplyr::mutate(kt1, cluster = 100*floor(PLOT/100))

# select just the interesting variables
kt1 <- dplyr::select(kt1, SETTING_ID, mdate, myear, MEASUREMENT_NO, 
                       PURPOSE_CODE, cluster, PLOT, SUBPLOT, LIVE_DEAD, SPECIES_SYMBOL, 
                       DIAMETER, TPA_EQUIV, BASAL_AREA_EQUIV, HEIGHT, CROWN_RATIO, 
                       CROWN_CLASS, AGE, RADIAL_GROWTH, HEIGHT_GROWTH)

