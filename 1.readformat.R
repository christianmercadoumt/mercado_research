# get files in directory
lolo_data <- fs::dir_ls("data/lolo")

# read each file using read.csv to address read/class errors
lolo_all_raw <- NULL
for (csvfile in lolo_data){
  newdat <- read.csv(csvfile,row.names=1,
                     colClasses=c("numeric",rep("character",3),"numeric","character",
                                  rep("numeric",2),rep("character",2),
                                  rep("numeric",3),rep("character",2),
                                  rep("numeric",5),"character",
                                  rep("numeric",3),rep("character",11)))
  lolo_all_raw <- rbind(lolo_all_raw,newdat)
}

#cleaning up dates, adding column with proper date format, then year
lolo1 <- mutate(lolo_all_raw, mdate = as.Date(MEASUREMENT_DATE, format = "%m/%d/%Y"))
lolo1 <- mutate(lolo1, myear = format(mdate, format = "%Y"))

# add cluster variable
lolo1 <- mutate(lolo1, cluster = 100*floor(PLOT/100))

# select just the interesting variables
lolo1 <- select(lolo1, SETTING_ID, mdate, myear, MEASUREMENT_NO, 
                    PURPOSE_CODE, cluster, PLOT, SUBPLOT, LIVE_DEAD, SPECIES_SYMBOL, 
                    DIAMETER, TPA_EQUIV, BASAL_AREA_EQUIV, HEIGHT, CROWN_RATIO, 
                    CROWN_CLASS, AGE, RADIAL_GROWTH, HEIGHT_GROWTH)



