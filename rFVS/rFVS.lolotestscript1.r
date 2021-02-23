# 1. set directories
fvs_r_code <- "c:/dlra/projects/OpenFVS/rFVS/R"  # location of OpenFVS R scripts
fvs_bin <- "c:/dlra/projects/OpenFVS/trunk/bin"  # location of OpenFVS binaries
key.dir <- file.path(getwd(),"rfvs_code")        # location of key files that we'll create
lolo.data <- file.path(getwd(),"loloNF")         # location of lolo NF pgp data


# 2. source FVS R functions 
for (rfile in dir (fvs_r_code)){
  source (paste(fvs_r_code,rfile,sep="/"))
}

fetchTrees <- function (captureYears){
  curYear <- fvsGetEventMonitorVariables("year")
  if (is.na(match(curYear,captureYears))) NULL else
    fvsGetTreeAttrs(c("plot","id","species",
                      "tpa","plotsize",
                      "dbh","ht",
                      "cratio","crwdth"))
}

# 3. load the appropriate FVS variant and species codes
fvsLoad("qFVSie", bin=fvs_bin) # loads ie varient
spp <- as.data.frame(fvsGetSpeciesCodes())
spp$fvs_num <- as.numeric(row.names(spp))


# 4. obtain and format tree data 
load(file.path(lolo.data,"lolo_stands.Rdata"))
testset <- lolo[lolo$SETTING_ID=="01160304010002" &
                  lolo$MEASYEAR==1987 &
                  lolo$PURPOSE_CODE=="CP" &
                  lolo$LIVE_DEAD=="L",]
testset$TPA_EQUIV_cl <- ifelse(testset$TPA_EQUIV==20,
                            20/3, # if large tree, then cluster-level trees/ac is 20/3 for the cluster
                            testset$TPA_EQUIV/9) # if small tree, then cluster level is 300/9
testset <- merge(testset,spp,
                 by.x="SPECIES_SYMBOL",
                 by.y="plant",
                 all.x=TRUE)
FVS.Tree.Data <- data.frame(plot=testset$PLOT,
                            tree=testset$TAG_ID,
                            count=testset$TPA_EQUIV_cl,
                            species=testset$fvs, 
                            dbh=testset$DIAMETER,
                            hist=1, # set to 1 to indicate alive
                            height=testset$HEIGHT,
                            crown.ratio=testset$CROWN_RATIO)
FVS.Tree.Data$age <- "" # add empty ages because these are missing

##From essential FVS:##
#Trees smaller than 4.5 feet in height should be assigned
#a small, but nonzero, diameter (for example, an estimated bud width, or 0.1 inch).
#This diameter will not be incremented until projected
#height becomes greater than 4.5 feet. DBH must be recorded if the tree is to be projected;
#records with blank or zero DBH values are ignored. 
FVS.Tree.Data$dbh[is.na(FVS.Tree.Data$dbh) | FVS.Tree.Data$dbh==0 ] <- 0.1

#Make sure that your crown ratios are out of 100 (ie 30 not .30)


# 5. set FVS parameters
## stand name
stdname<- paste(testset$SETTING_ID[1],testset$PURPOSE_CODE[1],sep="-")  

## stand characteristics
NFcode <- 116  #Lolo
habtype <- 670   #Stand habitat code
age <- 0         #Stand age in years, or 0 for unknown
aspect <- 360    #Stand Aspect in degrees #
slope <- 30      #Stand Slope in percent#
elev <- 5200
elev <- round(elev/100)  #Stand Elevation must be in hundreds of feet#

## cruise design
BAF <- 0   #set it to 0, no BAF
FRP <- 1   #set to 1 because trees/ac factors are given with data
DiamCO <- 999 # set to big because trees/ac factors are given
Plotcount <- 1 # treat the 3 plots in a cluster as a single large plot
nonstock <- 0        #Non-Stockable Plots#
propstockstand <- 1  #Prop of stand considered stockable#

## Timing parameters ###
year <- 1987   ##Inventory Year (INVYEAR)##
numcycle<- 5   ##Number of Cycles to be projected (NUMCYCLE)##
cyclelen <- 10 ## Cycle length in years ##
output_years <- c(1992,2000)  # ask for additional output years

## set FVS general options##
NOAUTOES <- TRUE  #Suppress simulated natural Ingrowth? (TRUE/FALSE)#
STATS <- TRUE     #should output table have statistical desription of input data#
RANNSEED <- 55329 #set to some integer for repeatable stochastic output 
NOTRIPLE <- TRUE  #Prevent tripling of tree records?#
NOCALIB <- TRUE   #allow for self-calibration, or not

# 6. make the keyword file (set to where your make.keyword.r file is located)
key.filename <- "testkeyfile.key"
keyfile <- file.path(key.dir,key.filename)
source(file.path(key.dir,"make.keyword.pgp.r"))

# 7. call the keyword file to initiate FVS, then run
fvsSetCmdLine(paste("--keywordfile=",keyfile,sep=""))

tree_list_years <- sort(c(1987,output_years))
tree_list_command <- paste("fetchTrees(c(",paste(tree_list_years,collapse=","),"))",sep="")
fvsInteractRun(AfterEM1=tree_list_command,
               SimEnd=fvsGetSummary) -> output

# 8.lookat the output
class(output) 
length(output) 
output[[1]] 

sum(output[[1]]$AfterEM1$tpa);sum(testset$TPA_EQUIV_cl)
sum(output[[2]]$AfterEM1$tpa)
sum(output[[3]]$AfterEM1$tpa)

names(output)
treelist1992 <- output[[2]]$AfterEM1
with(treelist1992,
     plot(dbh,ht,col=species))
