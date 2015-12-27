library("gstat")
library("spacetime")
library("sp")
library("reshape2")
library("RColorBrewer")

# Chinese classification
pm25_cuts = c(0, 35, 75, 115, 150, 250, 500)
pm25_labels = c("A: 0-35", "B: 35-75", "C: 75-115", "D: 115:150", "E: 150-250", "F: 250-500")
pm25_cuts_stplot = c(0, 0.01, 35, 75, 115, 150, 250, 500)
pm25_col = brewer.pal(6,"YlOrRd")
names(pm25_col) = pm25_labels
# take a sample of data and try to fit kriging model: one month
samp_dat = join_dat[(join_dat$date > as.Date("2015-10-31")) & (join_dat$date < as.Date("2015-12-1")), ]

# create a spatial data frame
# coordinates(samp_dat) = ~GPSLontitude+GPSLatitude

# location of observertory, time of observation
obs_loc = unique(samp_dat[, c("GPSLontitude", "GPSLatitude")])
obs_loc = obs_loc[order(obs_loc$GPSLontitude, obs_loc$GPSLatitude), ]
pmSP = SpatialPoints(obs_loc)
pmTM = sort(unique(samp_dat$date))

# handle data set in st format
long_dat1 = melt(samp_dat, id.vars = c("GPSLontitude", "GPSLatitude", "date"))
wide_dat1 = dcast(long_dat1, GPSLontitude + GPSLatitude + variable ~ date, value.var="value")
long_dat2 = melt(wide_dat1, id.vars = c("GPSLontitude", "GPSLatitude", "variable"), variable.name = "date")
st_dat = dcast(long_dat2, date + GPSLontitude + GPSLatitude ~ variable, value.var="value")
# the above manipulation messes the data type !!!
st_dat$date = as.Date(st_dat$date)
for (colm in c("co","pm10","so2","pm25","no2")){
    st_dat[, colm] = as.numeric(st_dat[, colm])
}
nrow(st_dat)

# create kriging data set for pm2.5
pm25_dat = st_dat[, "pm25", drop=FALSE]
pmDF = STFDF(sp=pmSP, time=pmTM, data=pm25_dat)
stplot(pmDF, at=pm25_cuts)

# emprical variogram
var = variogramST(pm25~1,data=pmDF,tunit="days",assumeRegular=F,na.omit=T) 
plot(var, map=FALSE)
plot(var, map=TRUE)
plot(var,wireframe=TRUE) 
StAni = estiStAni(var, c(-100, 10), spatialVgm = vgm(1.6,"Sph", 30, 0.1), temporalVgm = vgm(1,"Sph", 10, 0.1))

# lower and upper bounds
pars.l = c(sill.s = 0.001, range.s = 0.001, nugget.s = 0.001,sill.t = 0.001, range.t = 1, nugget.t = 0.001,sill.st = 0.001, range.st = 10, nugget.st = 0.001, anis = 0)
pars.u = c(sill.s = 200, range.s = 6000, nugget.s = 100,sill.t = 200, range.t = 5000, nugget.t = 100,sill.st = 200, range.st = 5000, nugget.st = 100,anis = 2) 

# model summary
summary.StVariogramModel = function(x){
    message(deparse(substitute(x)))
    message(attr(x, "MSE"))
    print(x)
    plot(var, x, map=FALSE)
}

# separable model
separable <- vgmST("separable", space = vgm(1.6,"Sph", 30, 0.1), time = vgm(1,"Sph", 10, 0.1), sill=1200)
separable_Vgm8 <- fit.StVariogram(var, separable, fit.method = 8)
separable_Vgm7 <- fit.StVariogram(var, separable, fit.method = 7, stAni = StAni, method = "L-BFGS-B", control = list(parscale=c(1,10,1,1,1)), lower = rep(0.0001,7))
separable_Vgm6 <- fit.StVariogram(var, separable, fit.method = 6) # default: no weights

lapply(list(separable_Vgm6, separable_Vgm7,separable_Vgm8), summary)

# product sum model
prodSumModel <- vgmST("productSum", space = vgm(10, "Exp", 200, 1), time = vgm(10, "Sph", 2, 1), k = 2)
ps_Vgm8 = fit.StVariogram(var, prodSumModel, fit.method = 8)
ps_Vgm7 = fit.StVariogram(var, prodSumModel, fit.method = 7, stAni = StAni, method = "L-BFGS-B", control = list(parscale=c(1,10,1,1,0.1,1,10)), lower = rep(0.0001,7))
ps_Vgm6 = fit.StVariogram(var, prodSumModel, fit.method = 6, method = "L-BFGS-B", control = list(parscale=c(1,10,1,1,0.1,1,10)), lower = rep(0.0001,7))

lapply(list(ps_Vgm6,ps_Vgm7,ps_Vgm8), summary)

# Decision: to use unweighted least square
# more complicated model: sum metric family
SimplesumMetric <- vgmST("simpleSumMetric",space = vgm(1200,"Sph", 30, 0.1),
                         time = vgm(500,"Sph", 10, 0.1), 
                         joint = vgm(1,"Sph", 500, 0), nugget=1000, stAni=StAni) 
plot(var, SimplesumMetric, map=FALSE)
SimplesumMetric_Vgm <- fit.StVariogram(var, SimplesumMetric,method = "L-BFGS-B",lower=pars.l)
summary(SimplesumMetric_Vgm)
# fully fledged sum metric does not work: optimization failed
## NOT RUN:
sumMetric <- vgmST("sumMetric", space = SimplesumMetric_Vgm$space,
                   time = SimplesumMetric_Vgm$time, 
                   joint = SimplesumMetric_Vgm$joint, stAni=103.5) 
plot(var, sumMetric, map=FALSE)
scl = c(1, 0.1, 1, 1, 0.1, 1, 1, 0.1, 1, 1)
# sm_Vgm = fit.StVariogram(var, sumMetric, fit.method = 6, method = "L-BFGS-B", lower = pars.l, upper = pars.u)
# summary(sm_Vgm)


