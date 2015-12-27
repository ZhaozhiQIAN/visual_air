#  数值单位：μg/m3(CO为mg/m3)
aqi_dat = read.csv("../houbao/export.csv", header = TRUE, stringsAsFactors = FALSE)
head(aqi_dat)

valid_date = grepl("[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}", aqi_dat$date)
sum(valid_date)
aqi_dat = aqi_dat[valid_date, ]

aqi_dat$date = as.Date(aqi_dat$date)
head(aqi_dat$date)
class(aqi_dat$date)

# city = sapply(aqi_dat$city, function(x) substr(x, start = 1, stop = nchar(x)-1))
# head(city)
# aqi_dat$city = city

head(aqi_dat)
table(aqi_dat$city)
length(unique(aqi_dat$city)) # 248 different cities

# city location
loc_dat = read.csv("../China.Cities.Location.Linux.csv", header = TRUE, stringsAsFactors = FALSE)
less_city = sapply(loc_dat$city, function(x) substr(x, start = 1, stop = nchar(x)-1))
loc_dat$full_city = loc_dat$city
loc_dat$city = less_city
loc_dat = loc_dat[!duplicated(loc_dat$city), ]
head(loc_dat)

# join
join_dat = merge(aqi_dat, loc_dat, by = "city")
table(join_dat$city)

# remove duplication
dupr = duplicated(join_dat[, c("GPSLontitude", "GPSLatitude", "date", "city")])
sum(dupr)
join_dat = join_dat[!dupr, ]
