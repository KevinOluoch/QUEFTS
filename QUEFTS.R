#' Simulates QUEFTS for maize taking soil parameters and fertilizer 
#'
#' \code{QUEFTS} Takes a vector of soil parameters, fertilizer use, mass fraction and prices and returns total yields, total fertilizer costs and yields
#'
#' @param siteSoilNutrient Vector of Soil Chemical data	(1) soilC (g/kg), (2) soilPolsen (p-OLSEN, mg/kg), (3)soilK (exchangeable K, mmol/kg); acidity (4) soilPH 
#' @param nutrients_kg.ha Total inorganic input of N, P, and K (kg/ha) 
#' 
#' @references  Zijlstra, Mink QUEFTS version, Zattati, Janssen
#' @return yield
#'
#' @examples
#' source('startup.R')
#' 
#' ### EXAMPLE: APPLYING TO MANY POINTS
#' siteSoilNutrient <- matrix(c(20, 5, 3, 5.8), ncol = 4, byrow = TRUE)
#' colnames(siteSoilNutrient) <- c('soilC', 'soilPolsen', 'soilK', 'soilpH')
#' fert_massfrac <-  matrix(c(0.14, 0.061, 0.116, 0.46, 0.0, 0.0, .180, .209, 0), ncol = 3, byrow = TRUE)      #Mass fraction for NPK (0.14, 0.061, 0.116) and Urea (0.46, 0.0, 0.0)
#' fert_amt <-  matrix(c(130,10,10), ncol= 3)
#' nutrients_kg.ha <- fert_amt %*% t(fert_massfrac)
#' ad <- matrix(c(26, 180, 24, 60, 540, 96)) #from Sattari 2014
#' QUEFTS(siteSoilNutrient = siteSoilNutrient, nutrients_kg.ha = nutrients_kg.ha, ad = ad)
QUEFTS <- function(siteSoilNutrient,
                   ad,
                   nutrients_kg.ha) { 

  #### SOIL NUTRIENTS ####
  soilC <- siteSoilNutrient[,'soilC']	   #kg/ha??
  soilPolsen <- siteSoilNutrient[,'soilPolsen']   #mg/kg
  soilK <- siteSoilNutrient[,'soilK']   #mmol/kg
  soilpH <- siteSoilNutrient[,'soilpH']

  #### YIELD ASSUMPTIONS ####
  #y_max <- 12000   # maximum yield (kg/ha)	
  # y_cur <- 2000 # ?	# current yield (kg/ha)	#1200 seems to work fine
  ## Shamie:     Range 0-0.5;     0 no yield reduction;    0.5 Non resonse due to floding, physical degradation etc
  # y_red <- 0.2  # NOT SURE WHAT THIS IS. SHAMIE TO EXPLAIN... Original: 0.2
  #Yatt <- max((2+((y_max-y_cur)/2))*(1-y_red),0 )  # attainable yield (kg/ha)	
  Yatt <- 10000	
  

  #### \\ Mink Zijlstra QUEFTS parameters####
  #a <- c(29, 95, 38)  
  #d <- c(74, 476, 143)
  
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
  YNP <- combine_yields(UN, a[1], d[1], r[1], YaP, YdP, YdK, Yatt)
  YNK <- combine_yields(UN, a[1], d[1], r[1], YaK, YdK, YdP, Yatt)
  YPN <- combine_yields(UP, a[2], d[2], r[2], YaN, YdN, YdK, Yatt)
  YPK <- combine_yields(UP, a[2], d[2], r[2], YaK, YdK, YdN, Yatt)
  YKN <- combine_yields(UK, a[3], d[3], r[3], YaN, YdN, YdP, Yatt)
  YKP <- combine_yields(UK, a[3], d[3], r[3], YaP, YdP, YdN, Yatt)

  # Initial yield estimate (kg/ha)
  YE <- (YNP+YNK+YPN+YPK+YKN+YKP)/6
  return(YE)
  
  # CORRECTION FOR UPPER YIELD LIMITS  #Should it be y_max instead of Yatt
  # YNPc <- pmin(y_max, YdN, YdP, YdK, YNP)
  # YNKc <- pmin(y_max, YdN, YdP, YdK, YNK)
  # YPNc <- pmin(y_max, YdN, YdP, YdK, YPN)
  # YPKc <- pmin(y_max, YdN, YdP, YdK, YPK)
  # YKNc <- pmin(y_max, YdN, YdP, YdK, YKN)
  # YKPc <- pmin(y_max, YdN, YdP, YdK, YKP)
  # 
  # # Corrected yield estimate (kg/ha)
  # YEc <- (YNPc+YNKc+YPNc+YPKc+YKNc+YKPc)/6
  # return(YEc)
}
