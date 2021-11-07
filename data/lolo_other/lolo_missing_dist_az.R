# get files in directory
lolo_data <- fs::dir_ls("data/lolo",glob="*.csv")

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

head(lolo_all_raw[is.na(lolo_all_raw$TAG_ID) &  is.na(lolo_all_raw$SUBPLOT),])
lolo_all_raw[lolo_all_raw$SETTING_ID=="01160413020023" & lolo_all_raw$MEASUREMENT_NO==1 & lolo_all_raw$PLOT==103,]

# add a last measurement indicator
last <- aggregate(lolo_all_raw$MEASUREMENT_NO,
                  by=list(SETTING_ID=lolo_all_raw$SETTING_ID,
                          PURPOSE_CODE=lolo_all_raw$PURPOSE_CODE),
                  max)
names(last)[3] <- "MEASUREMENT_NO"
last$last <- TRUE
lolo_all_raw0 <- merge(lolo_all_raw,
                       last,
                       all=T)

# restrict to missing d/az
miss_loc <- lolo_all_raw0[(is.na(lolo_all_raw0$AZIMUTH) | is.na(lolo_all_raw0$DISTANCE)) &
                           is.na(lolo_all_raw0$SUBPLOT) & # don't care about microplot trees
                           !is.na(lolo_all_raw0$TAG_ID) &  # missing tag ids used to indicate empty plots
                           (lolo_all_raw0$PURPOSE_CODE %in% c("CP","TP")),]
dim(miss_loc)
head(miss_loc)

miss_out <- miss_loc[,c(1,2,3,5,6,10:12,4,8,9,13:15)]
write.csv(miss_out,file="Lolo_PGP_all_meas_plots_meas_3_4_times_missingAD.csv",
          row.names = F)

table(miss_loc$SETTING_ID,miss_loc$MEASUREMENT_NO)
table(miss_loc$SETTING_ID[!is.na(miss_loc$last)],miss_loc$PLOT[!is.na(miss_loc$last)])
summary(miss_loc)

head(miss_loc[miss_loc$SETTING_ID=="01160714020002",])
lolo_all_raw[lolo_all_raw$SETTING_ID=="01160714020002" & lolo_all_raw$TAG_ID==10 & lolo_all_raw$PLOT==201,]
tail(miss_loc[miss_loc$SETTING_ID=="01160714020002",])
lolo_all_raw[lolo_all_raw$SETTING_ID=="01160714020002" & lolo_all_raw$TAG_ID==149 & lolo_all_raw$PLOT==103,]

head(miss_loc[miss_loc$SETTING_ID=="01160347060011" & miss_loc$MEASUREMENT_NO==4,])
lolo_all_raw[lolo_all_raw$SETTING_ID=="01160347060011" & !is.na(lolo_all_raw$TAG_ID) & lolo_all_raw$TAG_ID==191 & lolo_all_raw$PLOT==101,]
lolo_all_raw[lolo_all_raw$SETTING_ID=="01160347060011" & !is.na(lolo_all_raw$TAG_ID) & lolo_all_raw$TAG_ID==1 & lolo_all_raw$PLOT==303,]


head(lolo_all_raw[lolo_all_raw$SETTING_ID=="01160347060011",])