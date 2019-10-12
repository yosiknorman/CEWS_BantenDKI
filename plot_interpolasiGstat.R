library(ncdf4)
library(magrittr)
library(rgdal)
# nc_dat = ncdf4::nc_open("dependency/GEBCO_2014_2D_90.0_-15.0_150.0_15.0.nc")
# 
# lon_tpc = ncdf4::ncvar_get(nc_dat, "lon")
# lat_tpc = ncdf4::ncvar_get(nc_dat, "lat")
# topoc = ncdf4::ncvar_get(nc_dat, "elevation")
# ilon = which(lon_tpc >= 105 & lon_tpc <= 107)
# ilat = which(lat_tpc >= -7.5 & lat_tpc <= -5.5)
# topoc = topoc[ilon, ilat]
# topoc[topoc> 0] = NA
# lon_tpc = lon_tpc[ilon]
# lat_tpc = lat_tpc[ilat]
# tpc = list(lon = lon_tpc, lat =lat_tpc, tpc = topoc)
# save(tpc,   file = "dependency/tpc.bin")

load("dependency/tpc.bin")
lon_tpc = tpc$lon
lat_tpc = tpc$lat
topoc = tpc$tpc


# plot(kec, col = warna[majority$Mayoritas])

# kabs = readOGR("dependency/IDN_adm/IDN_adm2.shp")
# plot(kabs)
# kabs = kabs[kabs$NAME_1 %in% c("Banten", "Jakarta Raya"),]
# save(kabs, file = "dependency/kabs.bin")
load("dependency/kabs.bin")

png(filename = paste0("design/subtitute.png"),width = 715, height = 754, units = "px")
par(mai=c(0,0,0,0))
plot.window(xlim=c(0,0), ylim=c(0, 0))
image(lon_tpc, lat_tpc,topoc, col = colorRampPalette((blues9)[2:5])(100), xaxt = 'n',yaxt = 'n' , bty ='n',
      xlab = "", ylab= '', frame = F,
      axes=FALSE)
# plot(kec, col = warna[majority$Mayoritas], border = warna[majority$Mayoritas], add = T)
plot(kec, col = warna[majority$Mayoritas], border = "grey", lty = 3,add = T)
plot(kabs, border = "black", add = T)
dev.off()

nowdate = Sys.Date()
# daytanggal = as.integer(substr(nowdate,9, 10))

sqnowdate = seq(as.Date(Sys.Date()-3), Sys.Date(), by = "days")

# sqnowdate = seq(as.Date("2019-10-13")-3, as.Date("2019-10-13"), by = "days")
daysqnowdate = substr(sqnowdate, 9, 10)
if(as.integer(daysqnowdate[length(sqnowdate)]) > 0 & as.integer(daysqnowdate[length(sqnowdate)]) <= 2 ){
      ccc = as.integer(daysqnowdate[length(sqnowdate)])
      nowdate = sqnowdate[length(sqnowdate)-ccc]
}else if(as.integer(daysqnowdate[length(sqnowdate)]) > 10 & as.integer(daysqnowdate[length(sqnowdate)]) <= 12){
      ccc = as.integer(daysqnowdate[length(sqnowdate)])-10
      nowdate = sqnowdate[length(sqnowdate)-ccc]
}else if(as.integer(daysqnowdate[length(sqnowdate)]) > 20 & as.integer(daysqnowdate[length(sqnowdate)]) <= 22){
      ccc = as.integer(daysqnowdate[length(sqnowdate)])-20
      nowdate = sqnowdate[length(sqnowdate)-ccc]
}else{
      nowdate = sqnowdate[length(sqnowdate)-1]
}

daytanggal = as.integer(substr(nowdate,9, 10))
# daytanggal = 1
# if(daytanggal >= 1 & daytanggal <= 10){
#       daytanggal = "10";
# }else if(daytanggal >= 11 & daytanggal <= 20){
#       daytanggal = "20";
# }(daytanggal >= 21 & daytanggal <= 31){
#       if(daytanggal %in% c(30, 31)) ){
#             daytanggal = as.character(daytanggal);
#       }else{
#             daytanggal = as.character(daytanggal);
#       } 
# }
mab = c("Jan", "Feb", "Mar", "Apr", "Mei", "Jun", 
        "Jul", "Agu", "Sep", "Okt", "Nov", "Des")
mab = toupper(mab)
png(filename = paste0("design/tanggal.png"),width = 195, height = 28, units = "px")
par(mai=c(0,0,0,0))
plot.window(xlim=c(0,0), ylim=c(0, 0))
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
text(x = 0.5, y = 0.5, paste0("UPDATE ",daytanggal,"-", 
                              mab[as.integer(substr(nowdate, 6, 7))], "-",
                             substr(nowdate,1, 4)), 
     cex = 1.4, col = "darkblue")
dev.off()


setwd("design/")
# "../output/TeksWarning/"
nama_png = paste0(substr(uniq_HTH_input, 1, nchar(uniq_HTH_input)-4), "png")
comm =paste0("inkscape -z -e ../output/TeksWarning/",nama_png," -w 2900 -h 2270 drawing.svg")
comm2 = "sed -i drawing"
system(command = comm)
setwd("..")

# png(filename = paste0("output/TeksWarning/Warning.png"))
# par(mai=c(0,0,0,0))
# plot.window(xlim=c(0,0), ylim=c(0, 0))
# plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
# text(x = 0.5, y = 0.5, paste0(head_text, mid_text), 
#      cex = 1.4, col = "black")
# dev.off()


