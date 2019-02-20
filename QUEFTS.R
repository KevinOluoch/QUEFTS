#' Function that applied QUEFTS to a stack of rasters
#'
#' \code{QUEFTS} Takes a vector of soil parameters, fertilizer use, mass fraction and prices and returns total yields, total fertilizer costs and yields
#'
#' @param siteSoilNutrient Vector of Soil Chemical data	(1) soilC (g/kg), (2) soilPolsen (p-OLSEN, mg/kg), (3)soilK (exchangeable K, mmol/kg); acidity (4) soilPH; (5) maximum attainable yield kg/ha
#' @param nutrients_kg.ha Total inorganic input of N, P, and K (kg/ha) 
#' 
#' @references  Zijlstra, Mink QUEFTS version, Zattati, Janssen
#' @return yield
#'
#' @examples
#' ### EXAMPLE: APPLYING TO MANY POINTS
#' library(raster)
#' library(magrittr)
#' source('QUEFTS.R')
#' source('nutrientUPT.R')
#' source('combine_yields.R')
#' soilC <- raster('data/TZA_ORCDRC_T__M_sd1_1km.tif')
#' soilPolsen <- soilC
#' soilPolsen[] <- 15
#' soilK <- raster('data/TZA_EXKX_T__M_xd1_1km.tif')
#' soilpH <- raster('data/TZA_PHIHOX_T__M_sd1_1km.tif') /10
#' WY <- raster('data/africa_1km_crop_TZA.tif') %>% resample(soilC)
#' WY[WY < 0.8] <- NA   #remove low probability areas
#' WY[WY >= 0.8] <- 12000   #convert to WY. Should be taken from GYGA
#' rasters_input <- sapply(list(soilC, soilPolsen, soilK, soilpH, WY), getValues) %>% cbind(index=1:(nrow(.)))
#' rasters_input <- rasters_input[complete.cases(rasters_input),]  #compute only values with all data
#' colnames(rasters_input) <-  c('soilC', 'soilPolsen', 'soilK', 'soilpH', 'WY', 'index')
#' fert_massfrac1 <-  c(0.14, 0.061, 0.116)       #Mass fraction for NPK (0.14, 0.061, 0.116) and Urea (0.46, 0.0, 0.0)
#' fert_massfrac2 <-  c(0.46, 0.0, 0.0)       #Mass fraction for NPK (0.14, 0.061, 0.116) and Urea (0.46, 0.0, 0.0)
#' fert_amt1 <-  c(50)
#' fert_amt2 <-  c(10)
#' nutrients_kg.ha <- fert_amt1 * fert_massfrac1  + fert_amt2 * fert_massfrac2
#' yields <- apply(rasters_input,FUN = QUEFTS, MARGIN = 1, nutrients_kg.ha = nutrients_kg.ha)
#' results  <- soilC
#' results[] <- NA
#' results[rasters_input[,'index']] <- yields
QUEFTS <- function(siteSoilNutrient,
                   ad = matrix(c(26, 180, 24, 60, 540, 96)),  #Sattari 2014 parameters
                   nutrients_kg.ha) { 
  
  #### SOIL NUTRIENTS ####
  soilC <- siteSoilNutrient['soilC']	   #kg/ha??
  soilPolsen <- siteSoilNutrient['soilPolsen']   #mg/kg
  soilK <- siteSoilNutrient['soilK']   #mmol/kg
  soilpH <- siteSoilNutrient['soilpH']
  Yatt <- as.numeric(siteSoilNutrient['WY'])#kg/ha
  
  #### \\ Accumulation and dilution parameters####
  a <- c(ad[1], ad[2], ad[3])
  d <- c(ad[4], ad[5], ad[6])
  
  #### \\ MAIZE PARAMETERS ####
  RecFrac <- c(0.5, 0.1, 0.5)   #Recovery fraction (proportion, kg/kg)
  r <- c(5.0, 0.4, 2.0)  #Minimum nutrient i (i = N, P, K) uptake to produce any grain (Jannsen 1990)
  
  #### \\ AVAILABLE INPUT NUTRIENTS ####
  # 	(kg/ha) (In*Rn, Ip*Rp, Ik*Rk in Sattari)
  I <- nutrients_kg.ha*RecFrac  
  
  #### \\ pH CORRECTION FACTOR. Sattari 2014####
  fN <- 0.25*(soilpH-3)
  fP <- 1.0 - 0.5*(soilpH-6)^2
  fK <- 0.625*(3.4-0.4*soilpH) + I[3]
  
  #### \\ SUPPLY OF NUTRIENTS ####
  SN <- 6.8*fN*soilC + I[1]
  SP <- 0.35*fP*soilC + 0.5*soilPolsen  + I[2]
  SK <- 400*fK*soilK/(2+0.9*soilC) + I[3]
  
  ####+++++++ CALCULATION OF UPTAKE FOR NUTRIENTS GIVEN OTHER NUTRIENT +++++++####
  NPUPT <- nutrientUPT(SN, a[1], r[1], d[1], SP, a[2], r[2], d[2])
  NKUPT <- nutrientUPT(SN, a[1], r[1], d[1], SK, a[3], r[3], d[3])
  PNUPT <- nutrientUPT(SP, a[2], r[2], d[2], SN, a[1], r[1], d[1])
  PKUPT <- nutrientUPT(SP, a[2], r[2], d[2], SK, a[3], r[3], d[3])
  KNUPT <- nutrientUPT(SK, a[3], r[3], d[3], SN, a[1], r[1], d[1])
  KPUPT <- nutrientUPT(SK, a[3], r[3], d[3], SP, a[2], r[2], d[2])
  
  # Selected actual uptake. SHOULD LATER INCLUDE WLIMITED UPTAKKE
  #The lower uptake value of the two is used for the final estimation of nutrient uptake (Sattari 2014)
  UN <- min(NPUPT, NKUPT)
  UP <- min(PNUPT, PKUPT)
  UK <- min(KNUPT, KPUPT)
  
  #### CALCULATION OF YIELD RANGES ####
  # yields at max accumulation and max dilution
  YaN <- a[1] * max(UN-r[1],0)
  YdN <- d[1] * max(UN-r[1],0)
  YaP <- a[2] * max(UP-r[2],0)
  YdP <- d[2] * max(UP-r[2],0)
  YaK <- a[3] * max(UK-r[3],0)
  YdK <- d[3] * max(UK-r[3],0)
  
  # COMBINING YIELD RANGES OF TWO NUTRIENTS
  YNP <- combine_yields(UN, a[1], d[1], r[1], Ya1 = YaN, Yd1 = YdN, Ya2 = YaP, Yd2 = YdP, Yd3 = YdK, Yatt = Yatt)
  YNK <- combine_yields(UN, a[1], d[1], r[1], Ya1 = YaN, Yd1 = YdN, Ya2 = YaK, Yd2 = YdK, Yd3 = YdP, Yatt = Yatt)
  YPN <- combine_yields(UP, a[2], d[2], r[2], Ya1 = YaP, Yd1 = YdP, Ya2 = YaN, Yd2 = YdN, Yd3 = YdK, Yatt = Yatt)
  YPK <- combine_yields(UP, a[2], d[2], r[2], Ya1 = YaP, Yd1 = YdP, Ya2 = YaK, Yd2 = YdK, Yd3 = YdN, Yatt = Yatt)
  YKN <- combine_yields(UK, a[3], d[3], r[3], Ya1 = YaK, Yd1 = YdK, Ya2 = YaN, Yd2 = YdN, Yd3 = YdP, Yatt = Yatt)
  YKP <- combine_yields(UK, a[3], d[3], r[3], Ya1 = YaK, Yd1 = YdK, Ya2 = YaP, Yd2 = YdP, Yd3 = YdN, Yatt = Yatt)
  
  # Initial yield estimate (kg/ha)	
  YE <- (YNP+YNK+YPN+YPK+YKN+YKP)/6
  return(YE)
}