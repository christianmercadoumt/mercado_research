#require(tidyverse)

# get files in directory
#lolo_data <- fs::dir_ls("data/lolo")

# read each file using read.csv to address read/class errors
#lolo_all_raw <- NULL
#for (csvfile in lolo_data){
 # newdat <- read.csv(csvfile, row.names = 1,
  #                   colClasses = c("numeric",rep("character",3),"numeric","character",
   #                                rep("numeric",2),rep("character",2),
    #                               rep("numeric",3),rep("character",2),
     #                              rep("numeric",5),"character",
      #                             rep("numeric",3),rep("character",11)))

  #lolo_all_raw <- rbind(lolo_all_raw,newdat)
#}

#cleaning up dates, adding column with proper date format, then year
#lolo1 <- dplyr::mutate(lolo_all_raw, mdate = as.Date(MEASUREMENT_DATE, format = "%m/%d/%Y"))
#lolo1 <- dplyr::mutate(lolo1, myear = format(mdate, format = "%Y"))

# add cluster variable
#lolo1 <- dplyr::mutate(lolo1, cluster = 100*floor(PLOT/100))

# select just the interesting variables
#lolo1 <- dplyr::select(lolo1, SETTING_ID, mdate, myear, MEASUREMENT_NO, 
 #                   PURPOSE_CODE, cluster, PLOT, SUBPLOT, LIVE_DEAD, SPECIES_SYMBOL, 
  #                  DIAMETER, TPA_EQUIV, BASAL_AREA_EQUIV, HEIGHT, CROWN_RATIO, 
   #                 CROWN_CLASS, AGE, RADIAL_GROWTH, HEIGHT_GROWTH)

#col_names = c("x","SET_CN", "SETTING_ID","MEASUREMENT_DATE",
 #             "MEASUREMENT_NO","PURPOSE_CODE","PLOT","SUBPLOT",
  #            "PLOT_CN","TREE_CN","TAG_ID","AZIMUTH",
   #           "DISTANCE","LIVE_DEAD","SPECIES_SYMBOL","DIAMETER",
    #          "TPA_EQUIV","BASAL_AREA_EQUIV","HEIGHT","CROWN_RATIO",
     #         "CROWN_CLASS","AGE","RADIAL_GROWTH","HEIGHT_GROWTH",
      #        "SNAG_DECAY_CLASS","REMARKS","DISTURB_CATEGORY1",
       #       "DISTURB_AGENT1","DISTURB_AGENT_SEV1","DISTURB_CATEGORY2",
        #      "DISTURB_AGENT2","DISTURB_AGENT_SEV2","DISTURB_CATEGORY3",
         #     "DISTURB_AGENT3","DISTURB_AGENT_SEV3"),

