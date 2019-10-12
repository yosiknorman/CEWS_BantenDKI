#!/usr/bin/Rscript
library("ncdf4")
library("sp")
library("rgdal")
library("phylin")
library("rgdal")
library("leaflet")
library("raster")
library("gstat")
library("openxlsx")
library("readxl")

rm(list = ls())

is.integer0 <- function(x)
{
  is.integer(x) && length(x) == 0L
}

is.numeric0 <- function(x)
{
  is.numeric(x) && length(x) == 0L
}

is.character0 <- function(x)
{
  is.character(x) && length(x) == 0L
}

# setwd("/media/yosik/TOSHIBA/PD_Kekeringan/")
kec = readOGR("dependency/SHP_Kec_2015/banten_dki_2015.shp")
data_hth1 = read.csv("input/HTH_kec.csv", sep = ",", header = T)

uniq_HTH_input = tail(list.files("input/HTH/", pattern = "Monitoring_kekeringan"), 1L)
data_hth = readxl::read_excel(paste0("input/HTH/", uniq_HTH_input))
hhhHTH = as.matrix(data_hth)
colom_ihth = which(hhhHTH == "Indeks HTH", arr.ind = T )[2]
colom_hth = which(hhhHTH == "Hari Tanpa Hujan", arr.ind = T )[2]
hth_while = as.numeric(as.matrix(data_hth[7:151,colom_hth]))

prov_while = as.character(as.matrix(data_hth[7:151,2]))
kota_while = as.character(as.matrix(data_hth[7:151,3]))
kec_while = as.character(as.matrix(data_hth[7:151,4]))
sta_while = as.character(as.matrix(data_hth[7:151,5]))
lon_while = as.numeric(as.matrix(data_hth[7:151,6]))
lat_while = as.numeric(as.matrix(data_hth[7:151,7]))
ihth_while = as.numeric(as.matrix(data_hth[7:151,colom_ihth]))

hth_all_while = data.frame(prov_while, kota_while, kec_while, sta_while, lon_while, lat_while, hth_while, ihth_while)
hth_all_while = hth_all_while[!is.na(ihth_while),]
names(hth_all_while) = names(data_hth1)
# head(hth_all_while)
# head(data_hth1)
data_hth = hth_all_while
cchth = as.numeric(as.matrix(data_hth$hth))
ihth_whiles = cchth
ihth_whiles[cchth < 21] = 1
ihth_whiles[cchth >= 21 & cchth < 31] = 2
ihth_whiles[cchth >= 31 & cchth < 61] = 3
ihth_whiles[cchth > 61] = 4
data_hth$ihth = ihth_whiles

data_hth = data.frame(as.matrix(data_hth))
data_hth = data_hth[!is.na(data_hth$hth),]
lon = as.numeric(as.matrix(data_hth$lon))
lat = as.numeric(as.matrix(data_hth$lat))
xyll = data.frame(lon, lat)

coordinates(xyll) = ~lon + lat
xyll = SpatialPointsDataFrame(xyll, data_hth )
crs(xyll) = crs(kec)

lonlat <- expand.grid(x = seq(from = min(lon)-0.6,
                                         to = max(lon)+0.03,
                                         by = 0.004),
                                 y = seq(from = min(lat)-0.07,
                                         to = max(lat)+0.07,
                                         by = 0.004))
xaxts =  seq(from = min(lon),to = max(lon),by = 0.004)
yaxts =  seq(from = min(lat),to = max(lat),by = 0.004)
# length(xaxts); length(yaxts); 

coordinates(lonlat) <- ~x + y
crs(lonlat) = crs(kec)
gridded(lonlat) <- TRUE

neighbors = length(xyll)
beta = 5

idw_HTH = gstat(formula =  ihth ~ 1, # intercept only model
                 data = xyll, 
                 nmax = neighbors, 
                 set = list(idp = beta)) 

out <- predict(object = idw_HTH, newdata = lonlat, na.action = na.pass)
out1 = out
warna = colorRampPalette(c("green", "yellow", "orange", "red"))

class(out1)
rout1 = raster(out1)

fmask = list()
Mmask = list()

for(i in 1:length(kec)){
        fmask[[i]] = mask(rout1,kec[i,])
        Mmask[[i]] = as.matrix(fmask[[i]])
        Mmask[[i]] = Mmask[[i]][!is.na(Mmask[[i]])]
}

majority = data.frame(matrix(0,ncol = 7, nrow = length(kec)))
names(majority) = c("Kecamatan","Kabupaten","Provinsi", 
                    "Mayoritas", "Persen-Mayoritas", 
                    "Minoritas", "Persen-Minoritas")

for(i in 1:length(Mmask)){
        xindex = i
        if(any(is.numeric0(Mmask[[1]]))){
                majority$Kecamatan[xindex] = as.character(kec$Kecamatan[xindex])
                majority$Kabupaten[xindex] = as.character(kec$Kabupaten[xindex])
                majority$Provinsi[xindex] = as.character(kec$Provinsi[xindex])
                majority$Mayoritas[xindex] = 0
                majority$"Persen-Mayoritas"[xindex] = 0
                majority$Minoritas[xindex] = 0
                majority$"Persen-Minoritas"[xindex] = 0
        }else{
                
                p100 = length(Mmask[[xindex]])
                SK = Mmask[[xindex]][Mmask[[xindex]] >= 0 & Mmask[[xindex]] <= 1.5]
                K = Mmask[[xindex]][Mmask[[xindex]] > 1.5 & Mmask[[xindex]] <= 2.7]
                S = Mmask[[xindex]][Mmask[[xindex]] > 2.7 & Mmask[[xindex]] <= 3.5]
                C = Mmask[[xindex]][Mmask[[xindex]] > 3.5 & Mmask[[xindex]] <= 4]
                SC = Mmask[[xindex]][Mmask[[xindex]] > 4]
                
                pAll = c(round(length(SK)*100/p100,0), round(length(K)*100/p100,0),
                        round(length(S)*100/p100,0),round(length(C)*100/p100,0),
                        round(length(SC)*100/p100,0))
                Panggil = c(1,2,3,4,5)
                tertinggi = Panggil[which(pAll == max(pAll))]
                Ptertinggi = pAll[which(pAll == max(pAll))]
                pAll2 = pAll[which(pAll != max(pAll))]
                Kedua = Panggil[which(pAll != max(pAll))]
                tertinggi2 = Kedua[which(pAll2 == max(pAll2))]
                Ptertinggi2 = pAll2[which(pAll2 == max(pAll2))]
                if(any(is.numeric0(tertinggi))){
                        tertinggi = 0
                }
                if(any(is.numeric0(tertinggi2))){
                        tertinggi2 = 0
                }
                if(any(is.numeric0(Ptertinggi))){
                        Ptertinggi = 0
                }
                if(any(is.numeric0(Ptertinggi2))){
                        Ptertinggi2 = 0
                }
                majority$Kecamatan[xindex] = as.character(kec$Kecamatan[xindex])
                majority$Kabupaten[xindex] = as.character(kec$Kabupaten[xindex])
                majority$Provinsi[xindex] = as.character(kec$Provinsi[xindex])
                majority$Mayoritas[xindex] = tertinggi
                majority$"Persen-Mayoritas"[xindex] = Ptertinggi
                majority$Minoritas[xindex] = tertinggi2
                majority$"Persen-Minoritas"[xindex] = Ptertinggi2
        }
}

source("reduksi_pch_GSTAT.R")
MMajor = majority$Mayoritas
majority$Mayoritas[PCH2 != 2] = 1

file <- paste( "output.xlsx", sep="")
res <- write.xlsx(cbind(majority, MMajor), file) 
majority$Mayoritas[which(majority$Mayoritas %in% c(0, 1))] = 1
majority$Mayoritas[which(majority$Mayoritas > 4)] = 4
warna = c("green", "yellow", "orange", "red")

u_prov = unique(majority$Provinsi)
u_intensitas = c(4,3)
i_awas = which(majority$Mayoritas == u_intensitas[1])
data_awas = majority[i_awas,]
data_awas_prov = list()
for(i in 1:length(u_prov)){
        data_awas_prov[[i]] = data_awas[data_awas$Provinsi == u_prov[i],]
}
names(data_awas_prov) = u_prov
library(stringr)
make_content = function(iprov){
        u_kab_awas = unique(str_to_title(data_awas_prov[[iprov]]$Kabupaten))
        if(any(is.character0(u_kab_awas))){
          teks_warning = "Tidak Ada Peringatan"
        }else{
          u_kab_awas = unique((data_awas_prov[[iprov]]$Kabupaten))
          selected_kec = list()
          teks_warning = c()
          for(i in 1:length(u_kab_awas)){
            selected_kec[[i]] = data_awas_prov[[iprov]]$Kecamatan[which(data_awas_prov[[iprov]]$Kabupaten == u_kab_awas[i])]
            teks_warning[i] = paste0("_",str_to_title(u_kab_awas[i]),"_ (",
                                     paste(str_to_title(selected_kec[[i]]), collapse = ", ")
                                     ,")")
          }
        }
        return(teks_warning)
}

DKI_Awas = paste(make_content(2), collapse = ";\n")
BANTEN_Awas = paste(make_content(1), collapse = ";\n")

Tanggal = format(Sys.Date(),"%d %b %Y")
TanggalAkhir = format(Sys.Date()+(11),"%d %b %Y")
head_text = paste0("\nYth. Bapak/Ibu Disampaikan dengan hormat.\n\n*Peringatan Dini Kekeringan* \nUpdate ", 
Tanggal, " Berpotensi Terjadi kekeringan ekstrim di Provinsi Banten dan DKI Jakarta dengan pertimbangan sudah terdapat daerah tidak hujan berturut-turut selama >60 hari, serta dalam 2 dasarian kedepan (hingga ",TanggalAkhir,") diprakiraan masih terjadi curah hujan rendah (<20 mm/das) dengan peluang terjadi >70% sbb :\n")

mid_text = paste0("
*_Status AWAS :_*  \n
*Prov Banten* : ","\n",
BANTEN_Awas,"\n\n",
"*Prov DKI Jakarta* :\n",
DKI_Awas,"\n\n",
"Demikian disampaikan, Terima kasih
*_Stasiun Klimatologi Tangerang Selatan_*
")
write.table(file = "output/TeksWarning/teks.txt", paste0(head_text, mid_text), row.names = F, col.names= F)
source("plot_interpolasiGstat.R")