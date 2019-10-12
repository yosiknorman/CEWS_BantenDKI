#!/usr/bin/Rscript

library(gstat)
# dt_input =   c("PCH_SEPTEMBER_2019_DAS3.xls", "PCH_OKTOBER_2019_DAS1.xls")
dt_input = tail(list.files("input/PCH/", pattern = "PCH"), 2L)

filter_poly = function(pch){
  pch = data.frame(as.matrix(pch))
  lonpch = as.numeric(as.matrix(pch$lon))
  latpch = as.numeric(as.matrix(pch$lat))
  pchpch = as.numeric(as.matrix(pch$PCH..20mm))
  provpch = as.character(as.matrix(pch$PROV))
  pch_df = data.frame(Prov = provpch, lon = lonpch, lat= latpch, pch = pchpch)
  
  xyllpch = data.frame(lon = lonpch, lat = latpch)
  
  coordinates(xyllpch) = ~lon + lat
  xyllpch = SpatialPointsDataFrame(xyllpch, pch_df )
  crs(xyllpch) = crs(kec)
  
  lonlatpch <- expand.grid(x = seq(from = min(lonpch)-0.6,
                                   to = max(lonpch)+0.03,
                                   by = 0.009),
                           y = seq(from = min(latpch)-0.07,
                                   to = max(latpch)+0.07,
                                   by = 0.009))
  
  coordinates(lonlatpch) <- ~x + y
  crs(lonlatpch) = crs(kec)
  gridded(lonlatpch) <- TRUE
  
  neighborspch = length(xyllpch)
  beta = 5
  
  idw_PCH = gstat(formula =  pch ~ 1, # intercept only model
                  data = xyllpch, 
                  nmax = neighborspch, 
                  set = list(idp = beta)) 
  
  # na.exclude()
  outpch <- predict(object = idw_PCH, newdata = lonlatpch, na.action = na.pass)
  out1pch = outpch
  routpch = raster(out1pch)
  
  Mpch = list()
  hasil = c()
  for(i in 1:length(kec)){
    Mpch[[i]] = mask(routpch, kec[i,])
    ada = as.matrix(Mpch[[i]])
    ada[is.na(ada)] = 0
    if(any(ada >= 70)){
      hasil[i] = 1
    }else{
      hasil[i] = 0
    }
  }
  return(hasil)
}

pch = list()
hasil = list()
for(i in 1:length(dt_input)){
  pch[[i]] = readxl::read_excel(paste0("input/PCH/", dt_input[i]))
  hasil[[i]] = filter_poly(pch = pch[[i]])
}

PCH2 = data.frame(hasil[[1]], hasil[[2]])
PCH2 = apply(PCH2, 1, FUN = sum)
iPCH2 = which(PCH2 != 2)
# length(kec$Kecamatan)
