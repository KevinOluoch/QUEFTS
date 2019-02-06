#' Calculation of uptake of nutrient given the other nutrient
#' 
#' \code{nutrientUPT} calculates the actual uptake of a nutrient as a function of the potential supply
#' of that nutrient in relation to the potential supplies of the other nutrients.
#'
#' @param S1,S2 Supply of nutrients 1 and 2
#' @param a1,r1,d1,a2,r2,d2  QUEFTS parameters of nutrients 1 and 2
#'
#' @references Jannsen
#' @return Nutrient uptake for nutrient 1 given nutrient 2
#'
#' @examples
#' nutrientUPT(100, 30, 5, 70, 120, 200, 0.4, 600)
nutrientUPT <- function(S1, a1, r1, d1, S2, a2, r2, d2) {
  if(S1 < r1 + (S2-r2)*(a2/d1)){
    return (S1)
  } else if (S1 > r1 + (S2-r2)*(2*d2/a1 - a2/d1)){
    return(r1 + (S2-r2)*(d2/a1))
  } else{
    return(S1 - 0.25*(S1 - r1-(S2 - r2)*(a2/d1))^2 / ((S2 - r2)*((d2/a1)-(a2/d1))))
  }
}