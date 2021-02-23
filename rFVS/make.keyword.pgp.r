# set up output key file
options(width = 90)
lprnt <- function(x,wdth=10){format(x,width=wdth,justify="l")}
rprnt <- function(x,wdth=10){format(x,width=wdth,justify="r")}
newline <- function(x){cat(x,file=keyfile,append=T,sep="",fill=T)}

# start the key file
write(lprnt("SCREEN"),file=keyfile)
# output options
if (STATS){ newline(lprnt("STATS"))}
newline(c(lprnt("TREELIST"),rprnt(0)))
# simulator options
if (NOAUTOES){ newline(lprnt("NOAUTOES"))}
if (NOTRIPLE){ newline(lprnt("NOTRIPLE"))}
if (NOCALIB){ newline(lprnt("NOCALIB"))}
newline(c(lprnt("RANNSEED"),rprnt(RANNSEED)))
# stand info
newline(lprnt("STDIDENT"))
if (is.na(stdname)){
  newline(lprnt("NoName"))} else { 
    newline(lprnt(stdname,26))}
newline(c(lprnt("STDINFO"),rprnt(NFcode),rprnt(habtype),
          rprnt(age),rprnt(aspect),rprnt(slope),rprnt(elev)))
# data design parameters
newline(c(lprnt("DESIGN"),rprnt(BAF),rprnt(FRP),
          rprnt(DiamCO),rprnt(Plotcount),rprnt(nonstock),
          rprnt(1),rprnt(propstockstand)))
# growth intervals options
newline(c(lprnt("INVYEAR"),rprnt(year)))
newline(c(lprnt("NUMCYCLE"),rprnt(numcycle)))
#newline(c(lprnt("TIMEINT"),rprnt(0),rprnt(cyclelen)))
for (i in 1:length(output_years)){
  newline(c(lprnt("CYCLEAT"),rprnt(output_years[i])))
}
# tree data input
newline(lprnt("TREEFMT"))
  fmt <- "(I4,I7,F9.0,I1,A3,F4.1,F3.1,2F3.0,F4.1,I3,6I2,2I1,I2,2I3,2I1,F3.0)"
newline(lprnt(fmt,nchar(fmt)))
newline(lprnt(" "))
newline(c(lprnt("TREEDATA"),rprnt(15)))
for (tree in 1:nrow(FVS.Tree.Data)){
  tree.dat <- FVS.Tree.Data[tree,]
  tree.dat$ht <- ifelse(is.na(tree.dat$height)," ",round(tree.dat$height,0))
  tree.dat$cr <- ifelse(is.na(tree.dat$crown.ratio)," ",round(tree.dat$crown.ratio,0))
  tree.dat$cr <- ifelse(tree.dat$cr!=" " & tree.dat$cr<10,1,tree.dat$cr)
  newline(c(rprnt(tree.dat$plot,4),
            rprnt(tree.dat$tree,7),
            rprnt(tree.dat$count,9),
            rprnt(tree.dat$hist,1),
            rprnt(tree.dat$species,3),
            rprnt(10*round(tree.dat$dbh,1),4),
            
            rprnt(" ",3),
            #rprnt(10*round(tree.dat$dgr,1),3),  # diameter growth (10*in) OR DROP
            
            rprnt(tree.dat$ht,3),
            rprnt(" ",3),  # height to top kill (ft)
            
            rprnt(" ",4),
            #rprnt(tree.dat$htgr,4),  # height increment (10*ft) OR DROP
            
            rprnt(tree.dat$cr,3),
            
            rprnt(" ",6),
            #rprnt(tree.dat$dam1,2),rprnt(tree.dat$sev1,2),
            #rprnt(tree.dat$dam2,2),rprnt(tree.dat$sev2,2),
            #rprnt(tree.dat$dam3,2),rprnt(tree.dat$sev3,2),
            
            rprnt(" ",1),  # value class integer
            rprnt(" ",1),  # prescription code integer
            
            rprnt(" ",8),
            #rprnt(tree.dat$slope,2), # slope percent integer
            #rprnt(tree.dat$aspect,3), # aspect azimuth integer
            #rprnt(tree.dat$habitat,3), # habitat code integer
            
            rprnt(" ",1),  # plot topo class integer
            rprnt(" ",1),  # plot site prep code integer
            rprnt(tree.dat$age,3) #treeage)
  ))
}
newline(lprnt(-999,4))
# finish
newline(lprnt("PROCESS"))
newline(lprnt("STOP"))
