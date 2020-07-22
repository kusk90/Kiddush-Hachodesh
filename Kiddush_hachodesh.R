library(shiny)
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(circlize)
library(qdap)

setwd("C:/Users/i53813/OneDrive - Verisk Analytics/Documents/R")
kh_tables = read.csv("kh_tables.csv")

circos.initialize( factors = 1, xlim = c(1,360) )

circos.trackPlotRegion(factors = 1, ylim = c(1,10), bg.col = "grey", panel.fun = function(x, y) {
  circos.axis()
})
#04/07/2020
d1 = as.Date('3/22/1178',format='%m/%d/%Y')

num_mod = c(360,60,60,60)
base_sun = c(7,3,32,0)    # position of sun at ikur
base_apo = c(86,45,8,0)   # position of apogee at ikur
base_moon = c(31,14,43,0) # position of moon at ikur
base_epi = c(84,28,42,0)  # position of moon on epicycle at ikur
base_rosh = c(180,57,28,0)# position of north sun and moon axes meeting point at ikur
sun = c(0,59,8,19.8)      # mean movement of sun per day
apo = c(0,0,0,9)         # mean movement of apogee per day
moon = c(13,10,35,1.8)    # mean movement of moon per day
epi = c(13,3,53,55.8)     # mean movement of moon on epicycle per day
rosh = c(0,3,11,0)        # movement of sun and moon axes meeting point


d2 = as.Date('3/31/2020',format='%m/%d/%Y')
days = as.numeric(d2-d1)


# convert position to seconds and vice versa
to_sec = function(move)  {
  move_c = move[1]*3600 + move[2]*60 + move[3] + move[4]/60
  return(move_c)
}

from_sec = function(move_c)  {
  move = c(floor((move_c/3600) %% 360), floor((move_c/60) %% 60), floor(move_c %% 60), (move_c*60) %% 60)
  return(move)
}

days=29

#kh = function(userdate) {
userdate = "04/07/2020"
  d1 = as.Date('3/22/1178',format='%m/%d/%Y')
  d2 = as.Date(userdate,format='%m/%d/%Y')
  days = as.numeric(d2-d1)
###### sun #######

mean_sun = (to_sec(base_sun) + to_sec(sun)*days) %>% from_sec()
circos.trackPoints(factor = 1, x=mean_sun[1], y=5, col = "yellow", pch = 16, cex = 5)

mean_apo = (to_sec(base_apo) + to_sec(apo)*days) %>% from_sec()
circos.trackPoints(factor = 1, x=mean_apo[1], y=9, col = "black", pch = 16, cex = 1)

maslul = round((to_sec(mean_sun) - to_sec(mean_apo))/3600,0)
maslul = if_else(maslul < 0, maslul + 360, maslul)
mana1 = lookup(maslul,kh_tables$degree,kh_tables$table1)
true_sun = (to_sec(mean_sun) + mana1*60) %>% from_sec()
circos.trackPoints(factor = 1, x=mean_sun[1], y=5, col = "gray", pch = 16, cex = 5)
circos.trackPoints(factor = 1, x=true_sun[1], y=5, col = "yellow", pch = 16, cex = 5)

######### moon #########

mean_moon = (to_sec(base_moon) + to_sec(moon)*days) %>% from_sec()
circos.trackPoints(factor = 1, x=mean_moon[1], y=5, col = "blue", pch = 21, cex = 3)

mean_epi = (to_sec(base_epi) + to_sec(epi)*days) %>% from_sec()
circos.trackPoints(factor = 1, x=mean_epi[1], y=9, col = "blue", pch = 21, cex = 1)

adj1 = lookup(mean_sun[1],kh_tables$degree,kh_tables$moon_corr)
mean_moon_adj = (to_sec(mean_moon) + adj1*60) %>% from_sec()
dist2 = min(round((to_sec(mean_moon_adj) - to_sec(mean_sun))/3600*2,0),360) 
dist2 = if_else(dist2 < 0, dist2 + 360, dist2)
adj2 = lookup(dist2,kh_tables$degree,kh_tables$table2)
corr_epi = (to_sec(mean_epi) + adj2*3600) %>% from_sec()
mana2 = lookup(round(to_sec(corr_epi)/3600,0),kh_tables$degree,kh_tables$table3)
true_moon = (to_sec(mean_moon_adj) + mana2*60) %>% from_sec()
circos.trackPoints(factor = 1, x=mean_moon[1], y=5, col = "gray", pch = 16, cex = 5)
circos.trackPoints(factor = 1, x=true_moon[1], y=5, col = "blue", pch = 21, cex = 3)

########## rochav (latitude) ###########
mean_rosh = (3600*360 - (to_sec(base_rosh) + to_sec(rosh)*days)) %>% from_sec()
mean_zanav = mean_rosh - c(180,0,0,0)
mean_zanav = if_else(mean_zanav < 0, mean_zanav + 360, mean_zanav)
circos.trackPoints(factor = 1, x=mean_rosh[1], y=9, col = "green", pch = 16, cex = 1)
circos.trackPoints(factor = 1, x=mean_zanav[1], y=9, col = "red", pch = 16, cex = 1)

maslul_lat = round(to_sec(true_moon)/3600 - to_sec(mean_rosh)/3600,0)
maslul_lat = if_else(maslul_width < 0, maslul_width + 360, maslul_width)
lat_dir = case_when(maslul_lat == 0 ~ 'X',
                    maslul_lat == 180 ~ 'X',
                    maslul_lat < 180 ~ 'N',
                    maslul_lat > 180 ~ 'S')
lat1 = lookup(maslul_lat,kh_tables$degree,kh_tables$table4)

####### Orech (longitude) and rochav (latitude) #######

long1 = to_sec(true_moon) - to_sec(true_sun)
long1 = if_else(long1 < 0, long1 + 360*3600, long1) %>% from_sec()
vis1 = if_else(between(true_moon[1],90,270), case_when(long1[1] < 9 ~ 'NO',
                long1[1] < 15 ~ 'MAYBE',
                long1[1] >= 15 ~ 'YES'),
              case_when(long1[1] < 10 ~ 'NO',
                        long1[1] < 24 ~ 'MAYBE',
                        long1[1] >= 24 ~ 'YES'))
dif_long = lookup(true_moon[1],kh_tables$degree,kh_tables$table5a)
dif_lat = lookup(true_moon[1],kh_tables$degree,kh_tables$table5b)
long2 = (to_sec(long1) + dif_long*60) %>% from_sec()

lat2 = if_else(lat_dir == 'N', (lat1*60 - dif_lat*60),
                    (lat1*60 + dif_lat*60)) %>% from_sec()
adj3 = lookup(true_moon[1],kh_tables$degree,kh_tables$table6)           #maagul hayareach
long3 = if_else(lat_dir == 'N', 
                if_else(between(true_moon[1],90,270), to_sec(long2) + to_sec(lat2)*adj3,
                        to_sec(long2) - to_sec(lat2)*adj3),
                if_else(between(true_moon[1],90,270), to_sec(long2) - to_sec(lat2)*adj3,
                        to_sec(long2) + to_sec(lat2)*adj3)  ) %>% from_sec()

adj4 = lookup(true_moon[1],kh_tables$degree,kh_tables$table7)
long4 = (to_sec(long3) + to_sec(long3)*adj4) %>% from_sec()
adj5 = 2/3*lat1*60       #menas govah hamedina
vis_points = if_else(lat_dir == 'N', to_sec(long4) + adj5, to_sec(long4) - adj5) %>% from_sec

vis2 = case_when(vis_points[1] < 9 ~ 'NO',
                 vis_points[1] < 14 ~ 'MAYBE',
                 vis_points[1] >= 14 ~ 'YES')

vis3 = case_when((vis_points[1] >=  9 & long1[1] >= 13) |
                 (vis_points[1] >= 10 & long1[1] >= 12) |
                 (vis_points[1] >= 11 & long1[1] >= 11) |
                 (vis_points[1] >= 12 & long1[1] >= 10) |
                 (vis_points[1] >= 13 & long1[1] >= 9) ~ 'YES',
                 TRUE ~ 'NO')

vis3

#return(c(mean_sun,true_sun, mean_moon, true_moon, long1, vis_points, vis3))
#}

legend(2,6,legend="sun", col="Blue",pch=16, lty = 2)

##########checkup#########
mean_sun
mean_apo
mean_moon
mean_epi
maslul
mana1
true_sun

adj1
mean_moon_adj
dist2
adj2
corr_epi
mana2
true_moon

mean_rosh
maslul_lat
lat_dir
lat1


#########
circos.trackPoints(factor = 1, x=curr_sun[1], y=5, col = "yellow", pch = 16, cex = 5)
circos.trackPoints(factor = 1, x=curr_sun[1], y=5, col = "gray", pch = 16, cex = 5)
circos.trackPoints(factor = 1, x=curr_sun[1]-20, y=5, col = "yellow", pch = 16, cex = 5)
circos.trackPoints(factor = 1, x=curr_sun[1]-20, y=5, col = "gray", pch = 16, cex = 5)

