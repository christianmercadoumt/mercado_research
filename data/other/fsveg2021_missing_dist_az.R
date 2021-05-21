# get files in directory
new_data <- fs::dir_ls("data/other",glob="*.csv")

# read each file using read.csv to address read/class errors
all_raw <- NULL
for (csvfile in new_data){
  newdat <- read.csv(csvfile,#row.names=1,
                     colClasses=c("numeric",rep("character",2),"numeric","character",
                                  rep("numeric",2),rep("character",2),
                                  rep("numeric",3),rep("character",2),
                                  rep("numeric",5),"character",
                                  rep("numeric",3),rep("character",11)))
  all_raw <- rbind(all_raw,newdat)
}

head(all_raw[is.na(all_raw$TAG_ID) &  is.na(all_raw$SUBPLOT),])
all_raw[all_raw$SETTING_ID=="01160413020023" & all_raw$MEASUREMENT_NO==1 & all_raw$PLOT==103,]

# add a last measurement indicator
last <- aggregate(all_raw$MEASUREMENT_NO,
                  by=list(SETTING_ID=all_raw$SETTING_ID,
                          PURPOSE_CODE=all_raw$PURPOSE_CODE),
                  max)
names(last)[3] <- "MEASUREMENT_NO"
last$last <- TRUE
all_raw0 <- merge(all_raw,
                       last,
                       all=T)

# restrict to missing d/az
miss_loc <- all_raw0[(is.na(all_raw0$AZIMUTH) | is.na(all_raw0$DISTANCE)) &
                           is.na(all_raw0$SUBPLOT) & # don't care about microplot trees
                           !is.na(all_raw0$TAG_ID) &  # missing tag ids used to indicate empty plots
                          !is.na(all_raw0$last) &
                           (all_raw0$PURPOSE_CODE %in% c("CP","TP")),]
dim(miss_loc)
head(miss_loc)

miss_out <- miss_loc[,c(1,2,3,5,6,10:12,4,8,9,13:15)]
write.csv(miss_out[substring(miss_out$SETTING_ID,1,4)=="0116",],
          file="PGP_2021_missingAD_LoloNF.csv",
          row.names = F)

write.csv(miss_out[substring(miss_out$SETTING_ID,1,4)=="0114",],
          file="PGP_2021_missingAD_KootenaiNF.csv",
          row.names = F)

write.csv(miss_out[substring(miss_out$SETTING_ID,1,4)=="0104",],
          file="PGP_2021_missingAD_IPNFNF.csv",
          row.names = F)
