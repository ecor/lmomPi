####

library(rasterList)
library(lmomPi)
####
####
####

precff <- system.file("map/Mekrou_precipitation.grd", package="rasterList")
precs <- stack(precff)
prec_point <- precs[143][,]

## month index 
month <- as.character(as.Date(names(prec_point),format="X%Y.%m.%d"),format="M%m")
prec_point[(prec_point<1) & (month=="M01")] <- 0

distrib_prec2 <- c("gam")
para_vvv <- pel(x=prec_point,indices=month,distrib=distrib_prec2)
cdf_      <- cdf(x=prec_point,indices=month,para=para_vvv)      
spi_      <- spi.cdf(x=prec_point,indices=month,para=para_vvv)  

####  
## Not run: 
para_raster <- rasterList(precs,FUN=pel_x,indices=month,distrib=distrib_prec2)
spi1_raster <- stack(RasterListApply(x=rasterList(precs),para=para_raster,
                                     indices=list(month),FUN=spi.cdf))

para_raster_spi3 <- rasterList(precs,FUN=pel_x,indices=month,distrib=distrib_prec2,
                               spi.scale=3)
spi3_raster <- stack(RasterListApply(x=rasterList(precs),para=para_raster_spi3,
                                     indices=list(month),spi.scale=3,FUN=spi.cdf))

## End(Not run)


