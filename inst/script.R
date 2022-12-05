# TODO: Add comment
# 
# Author: cordaem
###############################################################################



########
#Author: Emanuele Cordano
#Date:   2017-11-28
#
# Comparison between 'lmomPi::spi.cdf' and 'SPEI::spi' 
#
########
#rm(list=ls())


library(lmomPi)
library(SPEI)
 
data(wichita)
 
distrib_wichita <- 'pe3'
spi.scale <- 1
 
month_wichita <- sprintf("M%02d",wichita$MONTH)
para_wichita  <- pel(x=wichita$PRCP,indices=month_wichita,distrib=distrib_wichita,spi.scale=spi.scale)
spi_wichita   <- spi.cdf(x=wichita$PRCP,indices=month_wichita,para=para_wichita,spi.scale=spi.scale)  
spi_wichita_speipkg   <- spi(data=wichita$PRCP,distrib='PearsonIII',scale=spi.scale)
 
difference <- spi_wichita-spi_wichita_speipkg$fitted

