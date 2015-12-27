# library(ggmap)
# 
# chn = get_map(location="houston", zoom=14)
# chnMap = ggmap(chn, extent = "device")
# 
# HoustonMap <- qmap("houston", zoom = 14, color = "bw", legend = "topleft")
library(ggplot2)
library(maptools)
mydat <- readShapePoly('../maps/bou2_4p.shp')
mydat$NAME = iconv(mydat$NAME, from = "GBK")

mymap = ggplot() +
    geom_polygon(data = fortify(mydat), aes(x = long, y = lat, group = id), colour = "black",fill = "lightgreen") +
    theme_grey()
print(mymap + coord_map())

# random data
long_samp = runif(100, 120, n = 1000)
lat_samp = runif(30, 35, n=1000)
val = rnorm(1000, 100, 20)
val_dat = data.frame(long=long_samp, lat=lat_samp, val=val)

mymap2 = ggplot() +
    geom_polygon(data = fortify(mydat), aes(x = long, y = lat, group = id), colour = "black",fill = "lightgreen") +
    geom_point(data=val_dat, aes(x=long, y=lat, color=val))
print(mymap2 + coord_map())

# subsample one day
oneday = join_dat[join_dat$date=="2015-9-1", ]

mymap3 = ggplot() +
    geom_polygon(data = fortify(mydat), aes(x = long, y = lat, group = id), colour = "black",fill = "lightgreen") +
    geom_point(data=oneday, aes(x=GPSLontitude, y=GPSLatitude, color=pm25)) + 
    coord_map()
print(mymap3)

# contour plot
x<-seq(1,11,.03)                    # note finer grid
y<-seq(1,11,.03)
xyz.func<-function(x,y) {-10.4+6.53*x+6.53*y-0.167*x^2-0.167*y^2+0.0500*x*y}
gg <- expand.grid(x=x,y=y)
gg$z <- with(gg,xyz.func(x,y))      # need long format for ggplot
# library(ggplot2)
library(RColorBrewer)               #for brewer.pal()
# create label and reformat
brks <- cut(gg$z,breaks=seq(0,100,len=6)) # (start,end]
brks <- gsub(","," - ",brks,fixed=TRUE)   # (start - end]
gg$brks <- gsub("\\(|\\]","",brks)        # start - end

contour_plot = ggplot(gg,aes(x,y))
contour_plot = contour_plot + geom_tile(aes(fill=brks)) # alpha=z
contour_plot = contour_plot +  scale_fill_manual("Z",values=brewer.pal(6,"YlOrRd")) # override default value
contour_plot = contour_plot +  scale_x_continuous(expand=c(0,0)) # remove grey background
contour_plot = contour_plot +  scale_y_continuous(expand=c(0,0))
contour_plot = contour_plot +  coord_fixed()
contour_plot
