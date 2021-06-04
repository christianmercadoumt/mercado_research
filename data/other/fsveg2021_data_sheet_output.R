#which_setting <- "01160415010059"

# get files in directory
#new_data <- fs::dir_ls("data/other",regexp="FSVeg_data_F..\\.csv"); new_data
new_data <- fs::dir_ls(".",regexp="FSVeg_data_F..\\.csv")

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

# head(all_raw[is.na(all_raw$TAG_ID) &  is.na(all_raw$SUBPLOT),])
# all_raw[all_raw$SETTING_ID=="01160413020023" & all_raw$MEASUREMENT_NO==1 & all_raw$PLOT==103,]

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

# restrict to focal setting and last measurement
focal <- all_raw0[all_raw0$SETTING_ID==which_setting &
                    !is.na(all_raw0$TAG_ID) &  # missing tag ids used to indicate empty plots 
                    !is.na(all_raw0$last),]
focal$SUBP <- ifelse(is.na(focal$SUBPLOT),"",as.character(focal$SUBPLOT))
#focal <- focal[order(focal$PLOT,focal$SUBP,focal$TAG_ID),]
focal <- focal[order(focal$PLOT,focal$SUBP,focal$AZIMUTH,focal$DISTANCE),]



table(focal$SPECIES_SYMBOL,useNA = c("ifany"))
table(focal$LIVE_DEAD,useNA = c("ifany"))
table(focal$CROWN_CLASS,useNA = c("ifany"))
table(focal$SNAG_DECAY_CLASS,useNA = c("ifany"))
table(focal$REMARKS,useNA = c("ifany"))


table(focal$DISTURB_CATEGORY1,useNA = c("ifany"))
table(focal$DISTURB_AGENT1,useNA = c("ifany"))
table(focal$DISTURB_AGENT_SEV1,useNA = c("ifany"))
table(focal$DISTURB_CATEGORY2,useNA = c("ifany"))
table(focal$DISTURB_AGENT2,useNA = c("ifany"))
table(focal$DISTURB_AGENT_SEV2,useNA = c("ifany"))
table(focal$DISTURB_CATEGORY3,useNA = c("ifany"))
table(focal$DISTURB_AGENT3,useNA = c("ifany"))
table(focal$DISTURB_AGENT_SEV3,useNA = c("ifany"))

focal$combo1 <- with(focal,paste(ifelse(DISTURB_CATEGORY1=="","",paste(DISTURB_CATEGORY1,DISTURB_AGENT1,DISTURB_AGENT_SEV1,sep="-"))))
focal$combo2 <- with(focal,paste(ifelse(DISTURB_CATEGORY2=="","",paste(DISTURB_CATEGORY2,DISTURB_AGENT2,DISTURB_AGENT_SEV2,sep="-"))))
focal$combo3 <- with(focal,paste(ifelse(DISTURB_CATEGORY3=="","",paste(DISTURB_CATEGORY3,DISTURB_AGENT3,DISTURB_AGENT_SEV3,sep="-"))))
focal$PRV_DAM <- with(focal,paste(combo1,combo2,combo3,sep=" "))
focal$PRV_DAM <- trimws(focal$PRV_DAM)
table(focal$PRV_DAM,useNA = c("ifany"))

focal$TREE_COUNT <- ""
f2 <- focal[,c("PLOT","SUBP","TAG_ID","AZIMUTH","DISTANCE","TREE_COUNT","SPECIES_SYMBOL","LIVE_DEAD",
         "DIAMETER","HEIGHT","CROWN_RATIO","CROWN_CLASS","SNAG_DECAY_CLASS","REMARKS","PRV_DAM")]

# add some blank rows
insertRows <- function(existingDF, newrow, r, blanks=10) {
  existingDF[seq(r+blanks,nrow(existingDF)+blanks),] <- existingDF[seq(r,nrow(existingDF)),]
  for (i in seq(r,r+blanks-1)){
    existingDF[i,] <- newrow
  }
  existingDF
}

(plts <- sort(unique(f2$PLOT)))
for (pl in plts){
  newrow <- f2[1,]
  newrow[1,!is.na(newrow)] <- NA
  newrow$PLOT <- pl
  tally <- sum(f2$PLOT<=pl)+1
  f2 <- insertRows(f2,newrow,tally)
}

write.csv(f2,
          #file=file.path("data","other",paste("last",which_setting,".csv",sep="")),
          file=paste("last",which_setting,".csv",sep=""),
          na="",row.names = F)

# coversheet info

# common damages
focal$combo1 <- with(focal,paste(ifelse(DISTURB_CATEGORY1=="","",paste(DISTURB_CATEGORY1,DISTURB_AGENT1,sep="-"))))
focal$combo2 <- with(focal,paste(ifelse(DISTURB_CATEGORY2=="","",paste(DISTURB_CATEGORY2,DISTURB_AGENT2,sep="-"))))
focal$combo3 <- with(focal,paste(ifelse(DISTURB_CATEGORY3=="","",paste(DISTURB_CATEGORY3,DISTURB_AGENT3,sep="-"))))
focal$cluster <- round(focal$PLOT,-2)
damg <- data.frame(cluster=rep(focal$cluster,3),DAM=c(focal$combo1,focal$combo2,focal$combo3))
damg <- damg[damg$DAM!="",]
with(damg,table(DAM,cluster))

# sample design
des <- read.csv(#file.path("data","other","Sample_design_info.csv"),
                file.path("Sample_design_info.csv"),
                   colClasses=c(rep("character",3),"numeric","character","numeric",
                                "character","numeric","character","numeric","numeric"))
keep_settting <- unique(focal[,c("SETTING_ID","PURPOSE_CODE","MEASUREMENT_NO","cluster")])
des_keep <- merge(des,keep_settting)

cluster_summary <- data.frame(SETTING_ID=which_setting,
                              Cluster=unique(des_keep$cluster))
cluster_summary$date <- NA
cluster_summary$micro_radius <- cluster_summary$subplot_min_DBH <- cluster_summary$subplot_radius <- NA
for (i in 1:nrow(cluster_summary)){
  large <- des_keep[des_keep$cluster==cluster_summary$Cluster[i] & des_keep$REMARKS=="LARGE TREE PLOT",]
  small <- des_keep[des_keep$cluster==cluster_summary$Cluster[i] & des_keep$REMARKS=="SMALL TREE PLOT",]
  cluster_summary$date[i] <- large$MEASUREMENT_DATE
  cluster_summary$subplot_radius[i] <- round(sqrt(43560/large$EXP_F/pi),1)
  cluster_summary$micro_radius[i] <- round(sqrt(43560/small$EXP_F/pi),1)
  cluster_summary$subplot_min_DBH[i] <- large$MINV
}
cluster_summary


# 
# 
# write.csv(cluster_summary,
#           file=file.path("data","other",paste("cover",which_setting,".csv",sep="")),
#           row.names = F)
# write.csv(with(damg,table(DAM,cluster)),append=TRUE,
#           file=file.path("data","other",paste("cover",which_setting,".csv",sep="")),
#           row.names = F)


# gps available?
pts <- read.csv(#file.path("data","other","waypoints_30mar2021.csv"),
  file.path("waypoints_30mar2021.csv"),
  colClasses=c(rep("character",3),rep("numeric",2),"character"))
pts <- pts[pts$Stand.Number==which_setting,1:5]
