dat = read.csv("Crash_Reporting_-_Drivers_Data.csv")

#Data acquired from: https://www.google.com/url?q=https://data.montgomerycountymd.gov/Public-Safety/Crash-Reporting-Drivers-Data/mmzv-x632&sa=D&ust=1520987955715000&usg=AFQjCNH6vBW0gz1EvMtIFGbHHhljqkzTZg
#Date: 3/13/18

#The data has `r nrow(data)` rows and `r ncol(data)` columns.
sum(!complete.cases(data), na.rm = TRUE)
dat$timesStamp <- as.POSIXct(strptime(dat$Crash.Date.Time, tz = "America/New_York"
                                      ,format = "%m/%d/%Y %I:%M:%S %p"))

# road.type = sort(unique(dat$Route.Type),na.last = TRUE)
# color = c(rainbow(10), "White")
# road.color = data.frame(road.type, color)

severity = sort(unique(dat$Injury.Severity))
#color_s = c("#ff0000", "Black", "#3f0000", "#7a0000", "#b20000")
color_s = c("Red", "Black", "Pink", "Orange", "Blue")
inj.color = data.frame(severity,color_s)

# dat = merge(dat,road.color,all.x = TRUE, by.x = "Route.Type", by.y = "road.type")
dat = merge(dat,inj.color, by.x = "Injury.Severity", by.y = "severity")

# plot(dat$Latitude, dat$Longitude, xlim = c(38.8,39.5), ylim = c(-77.5, -76.8), col = dat$color)

# Due to R Studio having issues with the colors set above, this color vector is to account for that to create the legend.
color_corrected = c("Red", "Black", "Green", "Green", "Blue")

par(mar = c(3,3,3,3))
plot(dat$Latitude, dat$Longitude, xlim = c(38.8,39.5), ylim = c(-77.5, -76.8), col = dat$color_s, pch = 16)
legend("topright", legend = c("Fatal Injury", "No Apparent Injury", "Possible Injury", "Suspected Minor Injury", "Suspected Serious Injury"), col = color_corrected, pch = c(16, 16), cex = 0.8)

# par(mar = c(3,15,3,3))
# barplot(table(color_s), col = color_s, horiz = T, las = 2, ylab = "", axes = FALSE, names.arg = severity)

table(dat$color_s)
table(color_s)

light_tctype = data.frame(dat$Light, dat$Traffic.Control)
tc = light_tctype[light_tctype$dat.Traffic.Control == "TRAFFIC SIGNAL",]
tc_counts = c(length(tc[tc$dat.Light == "DAYLIGHT",]$dat.Light), length(tc[tc$dat.Light == "DARK LIGHTS ON",]$dat.Light), length(tc[tc$dat.Light == "DARK NO LIGHTS",]$dat.Light),length(tc[tc$dat.Light == "DARK -- UNKNOWN LIGHTING",]$dat.Light), length(tc$dat.Light) - (light_tc + dark_tc + dark_tc2 + dark_tc3))
tc_total = length(tc$dat.Light)

nc = light_tctype[light_tctype$dat.Traffic.Control == "NO CONTROLS",]
nc_counts = c(length(nc[nc$dat.Light == "DAYLIGHT",]$dat.Light),length(nc[nc$dat.Light == "DARK LIGHTS ON",]$dat.Light),length(nc[nc$dat.Light == "DARK NO LIGHTS",]$dat.Light),length(nc[nc$dat.Light == "DARK -- UNKNOWN LIGHTING",]$dat.Light),length(nc$dat.Light) - (light_nc + dark_nc + dark_nc2 + dark_nc3))
nc_total = length(nc$dat.Light)

par(mfcol = (c(1,2)))
barplot(c(tc_counts[1]/tc_total, tc_counts[2]/tc_total, tc_counts[3]/tc_total, tc_counts[4]/tc_total, tc_counts[5]/tc_total), names.arg = c("Daylight", "Dark - Lights On", "Dark - No Lights", "Dark - Unkown", "Other"), main = "Number of Accidents at Traffic Lights", ylim = c(0, 0.75), col = rainbow(5))
barplot(c(nc_counts[1]/nc_total, nc_counts[2]/nc_total, nc_counts[3]/nc_total, nc_counts[4]/nc_total, nc_counts[5]/nc_total), names.arg = c("Daylight", "Dark - Lights On", "Dark - No Lights", "Dark - Unkown", "Other"), main = "Number of Accidents with No Signals", ylim = c(0,0.75), col = rainbow(5))

par(mfcol = (c(1,1)))

# Check data for off-road accidents, most should be parking lots

off_road = dat[(dat$Road.Name == ""),]
road_count = length(dat$Road.Name)
off_road_prop = length(off_road$Road.Name)/road_count
pLots = off_road[grep("PARKING LOT", off_road$Off.Road.Description),]
pLots2 = off_road[grep("parking lot", off_road$Off.Road.Description),]
p_l = sum(length(pLots$Report.Number), length(pLots2$Report.Number))
p_l_prop = p_l/road_count
other_prop = off_road_prop - p_l_prop

par(mar = c(3,3,3,3))
barplot(c(off_road_prop, p_l_prop, other_prop), names.arg = c("Off Road", "Parking Lots", "Others"), ylim = c(0,0.15), main = "Proportion of Off-Road Accidents and Parking Lot Accidents", col = "#5555FF")

dat$timeStamp = strptime(as.character(dat$Crash.Date.Time), "%m/%d/%Y %I:%M:%S %p", tz="EST")
timeDat = dat[(dat$timeStamp$year + 1900) != 2018,]
months = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
par(mar=c(3,15,3,3))
barplot(table(timeDat$timeStamp$mon), main="Crashes Per Month (2015 - 2017)", col="firebrick", xlab = "Months", names.arg = months, horiz = TRUE, las = 1)
