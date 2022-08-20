load("data/ingy_clusters_googleearth.Rdata")
head(ingy_clusters)

# set the elevation contours
summary(ingy_clusters$elev_m)
elevs <- seq(800,1800,by=200)

# rotate the aspects so that cos(east)=1; cos(west)=-1; sin(north)=1; sin(south)=-1
rotate_aspect <- function(azimuth_degrees){
  new_asp <- ifelse(azimuth_degrees<=90,90-azimuth_degrees,
                    ifelse(azimuth_degrees>270,
                           450-azimuth_degrees,
                           -(azimuth_degrees-90)))
  new_asp
}
ingy_clusters$rotated.aspect <- rotate_aspect(ingy_clusters$aspect_deg)

#heatramp <- heat.colors(31,rev=T)
#ingy_clusters$my_cols <- heatramp[1+round(30*with(ingy_clusters,(heatload-min(heatload))/max(heatload)))]
#with(ingy_clusters,plot(heatload,heatload,col=my_cols))

ingy_clusters$my_cols <- 1

png(filename="17.figures_saved/aspect_elev.png",
    width=4,height=4,units="in",res=600,pointsize = 10)
with(ingy_clusters,{
  x <- (max(elevs)-elev_m)*cos(pi*rotated.aspect/180)
  y <- (max(elevs)-elev_m)*sin(pi*rotated.aspect/180)
  plot(x,y,asp=1,axes=F,xlab="",ylab="",type="n",
       xlim=1.1*(max(elevs)-min(elevs))*c(-1,1),
       ylim=1.1*(max(elevs)-min(elevs))*c(-1,1))
  symbols(rep(0,length(elevs)),rep(0,length(elevs)),circles=max(elevs)-elevs,inches=F,add=T,fg="grey")
  abline(h=0,col="grey")
  abline(v=0,col="grey")
  #text(rep(0,length(elevs)),max(elevs)-elevs,elevs,
  #    col=grey(.2),cex=.5,adj=c(-.1,1.2))
  text(cos(pi*(-135)/180)*(max(elevs)-elevs),
       sin(pi*(-135)/180)*(max(elevs)-elevs),
       elevs,col=grey(.2),cex=.5)
  text(1.05*(max(elevs)-min(elevs))*c(1,0,-1,0),1.05*(max(elevs)-min(elevs))*c(0,-1,0,1),
       c("E","S","W","N"),
      col=grey(.2),cex=.75)
  points(x,y,col=my_cols)
})
dev.off()
ingy_clusters[ingy_clusters$my_cols==2,]





load("data/ingy_settings_si.Rdata")
head(ingy_si)

combo <- merge(ingy_si[ingy_si$SPECIES_SYMBOL=="LAOC",],
               ingy_clusters)
library(lattice)
xyplot(heatload ~ mean_si, data=combo,
       type=c("p","smooth"))

library(tidyverse)

ggplot(combo, aes())

