# read the data and get some new variables
source("1.readformat.R")
head(lolo1)

# drop the SE and CI purpose code data - these are done in "off" years  
#  and seem to involve variable radius plot sampling; I think they
#  represent other data coll efforts --> need to check tho
lolo1 <- lolo1[!(lolo1$PURPOSE_CODE %in% c("SE","CI","RR")),]
table(lolo1$PURPOSE_CODE) # need to figure out what "RR" is = "root rot" survey

# check whether all installations had 3 plots per cluster; info is needed because 
#  TPA is recorded as 20 tr/ac not 20/3 tr/ac
plot_structure <- aggregate(lolo1$PLOT,
                            by=list(SETTING_ID=lolo1$SETTING_ID,
                                    PURPOSE_CODE=lolo1$PURPOSE_CODE,
                                    myear=lolo1$myear,
                                    cluster=lolo1$cluster),
                            FUN=function(x){length(unique(x))})
unique(plot_structure$x)


# check whether all plots had 3 subplots
lolo1a <- lolo1[!is.na(lolo1$SUBPLOT),]
subplot_structure <- aggregate(lolo1a$SUBPLOT,
                               by=list(SETTING_ID=lolo1a$SETTING_ID,
                                       PURPOSE_CODE=lolo1a$PURPOSE_CODE,
                                       cluster=lolo1a$cluster,
                                       PLOT=lolo1a$PLOT),
                               FUN=function(x){length(unique(x))})
unique(subplot_structure$x)


# just live trees
lolo1_live <- lolo1[!is.na(lolo1$TPA_EQUIV) & lolo1$LIVE_DEAD=="L",]
unique(lolo1_live$TPA_EQUIV)

# all other small tree plots appear to 300th acre
unique(lolo1_live$TPA_EQUIV[lolo1_live$PURPOSE_CODE!="RR" & !is.na(lolo1_live$SUBPLOT)]) %% 300
unique(lolo1_live$TPA_EQUIV[lolo1_live$PURPOSE_CODE!="RR" & is.na(lolo1_live$SUBPLOT)]) 

# fix tpa to account for multiplicity of plots, subplots per cluster
lolo1_live$TPA_EQUIV <- with(lolo1_live, TPA_EQUIV/ifelse(is.na(SUBPLOT),3,9))

# obtain basal area equivalents
lolo1_live$BA_EQUIV <- with(lolo1_live,
                            ifelse(!is.na(DIAMETER),DIAMETER,0)^2 * 
                              (pi/576)* TPA_EQUIV )
lolo1_live$PSME_BA_EQUIV <- with(lolo1_live,BA_EQUIV*(SPECIES_SYMBOL=="PSME"))

# sum to the cluster level
stand_char <- aggregate(lolo1_live[,c("TPA_EQUIV","BA_EQUIV","PSME_BA_EQUIV")],
                        by=list(SETTING_ID=lolo1_live$SETTING_ID,
                                myear=lolo1_live$myear,
                                PURPOSE_CODE=lolo1_live$PURPOSE_CODE,
                                cluster=lolo1_live$cluster),
                        sum)
stand_char$DBH_Q <- with(stand_char, sqrt(576*BA_EQUIV/TPA_EQUIV/pi) )

stand_char$PCT_PSME <- with(stand_char, PSME_BA_EQUIV/BA_EQUIV)*100

stand_char <- stand_char[order(stand_char$SETTING_ID,
                               stand_char$cluster,
                               stand_char$myear),]

library(lattice)
xyplot(log10(TPA_EQUIV) ~ log10(DBH_Q), data=stand_char,
       groups=paste(SETTING_ID,cluster),type=c("p","l"),
       col=ifelse(stand_char$cluster==100,"darkgreen","blue"),alpha=.5)

xyplot(log10(TPA_EQUIV) ~ log10(DBH_Q) | cluster==100, data=stand_char,
       groups=paste(SETTING_ID,cluster),type=c("p","l","g"),
       col=grey(.5),alpha=.5)

xyplot(log10(TPA_EQUIV) ~ log10(DBH_Q) | equal.count(PSME_BA_EQUIV,4), data=stand_char,
       groups=paste(SETTING_ID,cluster),type=c("p","l","g"),
       col=grey(.5),alpha=.5)
