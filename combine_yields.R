#' Combining yield ranges of two nutrients
#'
#' \code{combine_yields} Combining yield ranges of two nutrients
#'
#' @param UPT1 Uptake of nutrient
#' @param a1,d1,r1 accumulation, dilution and minimum nutrient updtaketo produce any grain for the nutrient
#' @param Ya2 
#' @param Yd2,Yd3 Yield of other nutrients
#' @param y_att Attainable yield
#'
#' @references Mink Zijlstra
#' @return netrev=netrev, yield=YEc, totfertcost=totfertcost
#'
#' @examples
#' UPT1 <- 3000
#' a1 <- 26
#' d1 <- 60
#' r1 <- 5
#' Ya1 <- 4893
#' Yd1 <- 11292
#' Ya2 <- 180
#' Yd2 <- 540
#' Yd3 <- 96
#' Yatt <- 10000
#' combine_yields2(UPT1,a1,d1,r1, Ya1, Yd1,Ya2,Yd2,Yd3,Yatt)
combine_yields <- function(UPT1, a1, d1, r1, Ya1, Yd1, Ya2, Yd2, Yd3, Yatt) {
  LYD <- min(Yd1, Yd2, Yd3, Yatt)
  if (Yd1 <= Ya2)  {
    EY = Yd1
    if (Yd1 > LYD) {
      EY = LYD
    }
    return(EY)
  } else if ((Ya1 >= LYD) | (Yd1 <= LYD))  {
    if ((Ya1 < LYD) & (Yd1 == LYD)) {
      LYD = min(Yd2, Yd3, Yatt)
    } else {
      EY = LYD
      return(EY)
    } 
  } 
  EY = Ya2 + 2*(LYD-Ya2) * (UPT1-r1 - Ya2/d1) / (LYD/a1 - Ya2 / d1) - (LYD - Ya2) * (UPT1-r1 - Ya2/d1)^2 / (LYD/a1 - Ya2/d1)^2
  if (Ya2 > LYD) {
    EY = LYD
  }
  return(EY);
}