driver = function(start_date, end_date){
    ## training model
    samp_dat = join_dat[(join_dat$date > as.Date(start_date)) & (join_dat$date < as.Date(end_date)), ]
    
    
    obs_loc = unique(samp_dat[, c("GPSLontitude", "GPSLatitude")])
    obs_loc = obs_loc[order(obs_loc$GPSLontitude, obs_loc$GPSLatitude), ]
    pmSP = SpatialPoints(obs_loc)
    pmTM = sort(unique(samp_dat$date))
    pmDF_train = TidyDat(samp_dat, pmSP, pmTM,"pm25")
    
    avg_impute = mean(pmDF_train@data$pm25, na.rm=T)
    pmDF_train@data$pm25[is.na(pmDF_train@data$pm25)] = avg_impute
    
    var = variogramST(pm25~1, data=pmDF_train, assumeRegular=F, na.omit=T, progress = FALSE) 
    pars.l = c(sill.s=0.001, range.s=0.001, nugget.s=0.001, sill.t=0.001, range.t=1, nugget.t=0.001, sill.st=0.001, range.st=10, nugget.st=0.001, anis=0)
    SimplesumMetric <- vgmST("simpleSumMetric",space = vgm(1200, "Exp", 30, 0.1),
                             time = vgm(500,"Spl", 10, 0.1), 
                             joint = vgm(1,"Gau", 500, 0), nugget=1000, stAni=1) 
    SimplesumMetric_Vgm <- fit.StVariogram(var, SimplesumMetric,method = "L-BFGS-B",lower=pars.l, fit.method = 6)
 
    ## predict on sample grid
    sps = spsample(mydat, 5000, type="regular")
    names(obs_loc) = c("x1","x2")
    sps@coords = rbind(sps@coords, as.matrix(obs_loc))
    
    tms_whole = seq(as.Date(start_date), as.Date(end_date), by=1)
    ind = ceiling(seq.int(0, length(tms_whole), length.out=7))
    # ind = ceiling(seq.int(0, length(tms_whole), length.out=15))
    tms = list()
    for (i in 1:(length(ind)-1)){
        tms[[i]] = tms_whole[(1+ind[i]) : ind[i+1]]
    }
    
    pred_lst = list()
    for (i in 1:length(tms)){
        grid.st = STF(sps, tms[[i]])
        
        pred = krigeST(pm25 ~ 1, data=pmDF_train, modelList = SimplesumMetric_Vgm, newdata=grid.st)
        pred_lst[[i]] = pred
        
        pred_lst = time.split(pred)
        
        ## draw plot
        base_plot = ggplot() + 
            geom_polygon(data = fortify(mydat), aes(x = long, y = lat, group = id), colour = "black",fill = "white") +
            scale_colour_manual("PM 2.5",values=pm25_col) +
            scale_x_continuous(expand=c(0,0)) +
            scale_y_continuous(expand=c(0,0)) +
            coord_map()
        
        viso_base = viso_gen(base_plot, write_png=TRUE)
        lapply(pred_lst, viso_base)
    }
    pred_lst
}

nov = driver("2015-11-1", "2015-11-30")
oct = driver("2015-10-1", "2015-10-31")
sept = driver("2015-9-1", "2015-9-30") #
aug = driver("2015-8-1", "2015-8-31") #
Jul = driver("2015-7-1", "2015-7-31") #
jun = driver("2015-6-1", "2015-6-30") #
may = driver("2015-5-1", "2015-5-31")

apr = driver("2015-4-1", "2015-4-30") #
mar = driver("2015-3-1", "2015-3-31")
feb = driver("2015-2-1", "2015-2-28") #
jan = driver("2015-1-1", "2015-1-31") #

seq(as.Date("2015-11-1"), as.Date("2015-12-1"), by=1)

join_dat[(join_dat$date > as.Date(start_date)) & (join_dat$date < as.Date(end_date)), ]

str(nov[[1]])
