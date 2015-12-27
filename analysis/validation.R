
# tidy up data
TidyDat = function(dat, sp, tm, var_id){
    # handle data set in st format
    long_dat1 = melt(dat, id.vars = c("GPSLontitude", "GPSLatitude", "date"))
    wide_dat1 = dcast(long_dat1, GPSLontitude + GPSLatitude + variable ~ date, value.var="value")
    long_dat2 = melt(wide_dat1, id.vars = c("GPSLontitude", "GPSLatitude", "variable"), variable.name = "date")
    st_dat = dcast(long_dat2, date + GPSLontitude + GPSLatitude ~ variable, value.var="value")
    # the above manipulation messes the data type !!!
    st_dat$date = as.Date(st_dat$date)
    for (colm in c("co","pm10","so2","pm25","no2")){
        st_dat[, colm] = as.numeric(st_dat[, colm])
    }
    dat = st_dat[, var_id, drop=FALSE]
    pmDF_train = STFDF(sp=sp, time=tm, data=dat)
    pmDF_train
}

# take a sample of data and try to fit kriging model: one month
# eval_dat_whole = join_dat[(join_dat$date > as.Date("2015-8-31")) & (join_dat$date < as.Date("2015-12-1")), ]

eval_dat_whole = join_dat[(join_dat$date > as.Date("2015-11-1")) & (join_dat$date < as.Date("2015-12-1")), ]

# sample location of observertory
# use all time of observation
obs_loc = unique(eval_dat_whole[, c("GPSLontitude", "GPSLatitude")])
obs_loc = obs_loc[order(obs_loc$GPSLontitude, obs_loc$GPSLatitude), ]
sample_ind_perm = sample(nrow(obs_loc), nrow(obs_loc))
inds = ceiling(seq(0, nrow(obs_loc), length.out = 11))
obs_sample_ind = list()
for (i in 1:10){
    obs_sample_ind[[i]] = sample_ind_perm[(1 + inds[i]): inds[i+1]]
}




OneFold = function(obs_sample_ind){
    # obs_sample_ind = sample(nrow(obs_loc), floor(0.8 * nrow(obs_loc)))
    obs_loc_train = obs_loc[-obs_sample_ind, ]
    obs_loc_test = obs_loc[obs_sample_ind, ]
    eval_train = merge(eval_dat_whole, obs_loc_train, by = c("GPSLontitude", "GPSLatitude"))
    eval_test = merge(eval_dat_whole, obs_loc_test, by = c("GPSLontitude", "GPSLatitude"))
    pmSP_train = SpatialPoints(obs_loc_train)
    pmSP_test = SpatialPoints(obs_loc_test)
    pmTM_train = sort(unique(eval_dat_whole$date)) 
    pmTM_test = sort(unique(eval_dat_whole$date)) 
    test_grid = STF(pmSP_test, pmTM_test)
    
    pmDF_train = TidyDat(eval_train, pmSP_train, pmTM_train,"pm25")
    pmDF_test = TidyDat(eval_test, pmSP_test, pmTM_test, "pm25")
    
    var = variogramST(pm25~1, data=pmDF_train, assumeRegular=F, na.omit=T, progress = FALSE) 
    StAni = estiStAni(var, c(-100, 10), spatialVgm=vgm(1.6,"Gau", 30, 0.1), temporalVgm=vgm(1,"Spl", 10, 0.1))
    
    pars.l = c(sill.s=0.001, range.s=0.001, nugget.s=0.001, sill.t=0.001, range.t=1, nugget.t=0.001, sill.st=0.001, range.st=10, nugget.st=0.001, anis=0)
    pars.u = c(sill.s=10000, range.s=10, nugget.s=1000, sill.t=10000, range.t=15, nugget.t=1000, sill.st=1000, range.st=1000, nugget.st=1000, anis=1000)
    
    SimplesumMetric <- vgmST("simpleSumMetric",space = vgm(1200, "Gau", 30, 0.1),
                             time = vgm(500,"Spl", 10, 0.1), 
                             joint = vgm(1,"Gau", 500, 0), nugget=1000, stAni=1) 
    
    
    SimplesumMetric_Vgm <- fit.StVariogram(var, SimplesumMetric,method = "L-BFGS-B",lower=pars.l, upper=pars.u,fit.method = 8)
    
    # separable <- vgmST("separable", space = vgm(1.6,"Sph", 30, 0.1), time = vgm(1,"Sph", 10, 0.1), sill=1200)
    # separable_Vgm <- fit.StVariogram(var, separable, fit.method = 6)
    
    pred_test = krigeST(pm25 ~ 1, data=pmDF_train, modelList = SimplesumMetric_Vgm, newdata=pmDF_test)
    
    ref = mean(abs(unlist(pmDF_test@data) - mean(unlist(pmDF_train@data))))
    mod = mean(abs(unlist((pred_test@data - pmDF_test@data))), na.rm=T)
    
    library(caret)
    fac_ref <- cut(pmDF_test@data[,], breaks=pm25_cuts, labels=pm25_labels)
    fac_prd = cut(pred_test@data[,], breaks=pm25_cuts, labels=pm25_labels)
    con_mat = confusionMatrix(fac_prd, fac_ref)
    list(con_mat, ref, mod, pmDF_test, pred_test)
}


mad = lapply(obs_sample_ind, OneFold)
dif = sapply(mad, function(x) x[[2]] - x[[3]])  # big is good
t.test(dif)


library(parallel)
mad = mclapply(obs_sample_ind, OneFold, mc.preschedule = FALSE, mc.cores = 2)
mad

# cross validation: MAD is 20
mean(abs(eval_dat_whole$pm25 - mean(eval_dat_whole$pm25)))

# residual
res = OneFold(obs_sample_ind[[2]])

res[[1]]
res[[2]] # reference
res[[3]] # model

pred_res_df = res[[4]]
obs_res_df = res[[3]]

pred_res_df@data = cbind(pred = pred_res_df@data, true = obs_res_df@data, res = pred_res_df@data - obs_res_df@data)
names(pred_res_df@data) = c("pred", "true", "res")
pred_res_df@data$res = abs(pred_res_df@data$res)
str(pred_res_df)


time.split_res = function(pred_df){
    df_whole = cbind(as.data.frame(pred_df@sp@coords), pred_df@data)
    names(df_whole) = c("long", "lat", "pred", "true", "res")
    df_lst = list()
    cuk_size = nrow(df_whole) / length(pred_df@endTime)
    for (i in 1:length(pred_df@endTime)){
        df_lst[[i]] = df_whole[ (1+cuk_size*(i-1)) : (cuk_size*i), ]
    }
    df_lst
}

pred_res_lst = time.split_res(pred_res_df)

viso_gen_res = function(base_plot, showOBS = TRUE){
    eval(base_plot)
    viso = function(pred_df){
        vis = base_plot + geom_point(data = pred_df, aes(x = long, y = lat, size=res))
        if (showOBS)
            vis = vis+ geom_point(data=oneday, aes(x=GPSLontitude, y=GPSLatitude), colour = "black", shape=3) 
        
        vis
    }
    viso
}

viso_res_base = viso_gen_res(base_plot)
lapply(pred_res_lst, viso_res_base)

base_plot + geom_point(data = pred_res_lst[[1]], aes(x = long, y = lat, size=res))


head(with(join_dat, join_dat[(date > as.Date("2015-8-1")) & (date < as.Date("2015-12-1")) & pm25 >250,] ))






