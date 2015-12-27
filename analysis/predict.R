# create a sampling grid
# library(raster) # crop
# x.grid = seq(mydat@bbox[1,1], mydat@bbox[1,2], by = 0.1)
# y.grid = seq(mydat@bbox[2,1], mydat@bbox[2,2], by = 0.1)
# y2 = rep(y.grid, rep.int(length(x.grid), length(y.grid)))
# x2 = rep(x.grid, times = ceiling(length(y2)/length(x.grid)))
# samp_grid = SpatialPoints(data.frame(x = x2, y = y2))

# create a sampling grid
## spatial component
sps = spsample(mydat, 5000, type="regular")
## temporarl component
tms = seq(as.Date("2015-11-29"), as.Date("2015-12-3"), length.out = 5)
## combine
grid.st = STF(sps, tms)

# visualize a random function on the map
xyz.func<-function(x,y) {
    x = (x - min(x))/(max(x) - min(x)) * 11 + 1
    y = (y - min(y))/(max(y) - min(y)) * 11 + 1
    -10.4+6.53*x+6.53*y-0.167*x^2-0.167*y^2+0.0500*x*y
}
visual_sps = data.frame(long = sps$x1, lat = sps$x2, val = xyz.func(sps$x1, sps$x2))
brks <- cut(visual_sps$val,breaks=9) # (start,end]
brks <- gsub(","," - ",brks,fixed=TRUE)   # (start - end]
visual_sps$brks <- gsub("\\(|\\]","",brks)        # start - end

library(RColorBrewer)
vis_contour = ggplot() +
    geom_polygon(data = fortify(mydat), aes(x = long, y = lat, group = id), colour = "black",fill = "white") +
    geom_tile(data = visual_sps, aes(x = long, y = lat, fill=brks, alpha=0.3)) +
    scale_fill_manual("Z",values=brewer.pal(9,"YlOrRd")) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    coord_map()
vis_contour

# get fitted values
pred = krigeST(pm25 ~ 1, data=pmDF, modelList = SimplesumMetric_Vgm, newdata=grid.st)
stplot(pred) 
class(pred)
str(pred)
# create factor manually
brks <- cut(pred@data$var1.pred, breaks=pm25_cuts_stplot)
pred_cp = pred
pred_cp@data$var1.pred = brks
stplot(pred_cp, animate = 0.5)

# visualize fitted value
time.split = function(pred_df){
    df_whole = cbind(as.data.frame(pred_df@sp@coords), pred_df@data)
    names(df_whole) = c("long", "lat", "val")
    brks <- cut(df_whole$val, breaks=pm25_cuts, labels=pm25_labels)
    df_whole$brks <- brks
    # padding_df = data.frame(long=rep(0, 6), lat=rep(0,6), val = rep(0,6), brks = factor(1:6, labels=pm25_labels))
    df_lst = list()
    cuk_size = nrow(df_whole) / length(pred_df@endTime)
    for (i in 1:length(pred_df@endTime)){
        df_lst[[i]] = df_whole[ (1+cuk_size*(i-1)) : (cuk_size*i), ]
        attr(df_lst[[i]], "Date") = (as.Date(pred_df@endTime) - 1)[i]
    }
    df_lst
}
pred_lst = time.split(pred)

oneday = join_dat[join_dat$date=="2015-9-1", ]

viso_gen = function(base_plot, showOBS = TRUE, write_png=FALSE){
    eval(base_plot)
    viso = function(pred_df){
        vis = base_plot + 
            geom_point(data = pred_df, aes(x = long, y = lat, col=brks), alpha=0.4, size=4, shape=15L) +
            ggtitle(attr(pred_df, "Date")) 
        if (showOBS)
            vis = vis+ geom_point(data=oneday, aes(x=GPSLontitude, y=GPSLatitude), colour = "black", shape=3) 
        if (write_png){
            library(png)
            # png(filename=paste(attr(pred_df, "Date"),".png", sep=""), width=800, height=600)
            jpeg(filename=paste(attr(pred_df, "Date"),".jpeg", sep=""), width=800, height=600)
            print(vis)
            dev.off()
        }
    }
    viso
}

pm25_col = brewer.pal(6,"YlOrRd")
names(pm25_col) = pm25_labels


 
base_plot = ggplot() + 
    geom_polygon(data = fortify(mydat), aes(x = long, y = lat, group = id), colour = "black",fill = "white") +
    scale_colour_manual("PM 2.5",values=pm25_col) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    coord_map()

viso_base = viso_gen(base_plot, write_png=TRUE)
lapply(pred_lst[1:5], viso_base)


