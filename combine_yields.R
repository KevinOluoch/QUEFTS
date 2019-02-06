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
#' Ya2 <- 180
#' Yd2 <- 540
#' Yd3 <- 96
#' y_att <- 10000
#' combine_yields(UPT1,a1,d1,r1,Ya2,Yd2,Yd3,y_att)
combine_yields <- function(UPT1, a1, d1, r1, Ya2, Yd2, Yd3, y_att) {
  # Determine which nutrient limited yield is lowest.
  YdX <- min(Yd2, Yd3, y_att)
  a <- YdX - Ya2
  b <- UPT1 - r1 - Ya2/d1
  c <- YdX/a1 - Ya2/d1
  Y <- Ya2 + 2*a*b/c - a*b^2/c^2
  #Y <- 0
  #Y[!(UPT1 == 0 | YdX == 0)] <- (Ya2 + 2*a*b/c - a*b^2/c^2)[!(UPT1 == 0 | YdX == 0)]  #any other value
  return (Y)
}