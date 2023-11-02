#' Equation for Kaya Identity
#' 
#' The kaya identity expresses yearly CO2 emissions as a product of four factors
#' 
#' @param pop population size (in millions)
#' @param gdp GDP per person (in 1000$/person)
#' @param enInt energy intensity (in Gigajoule/$1000GDP)
#' @param carbInt carbon intensity (in tonnes CO2/Gigajoule)
#' 
#' @return numerical value representing the yearly CO2 emissions in million tonnes
#' 
#' @importFrom checkmate assert_numeric
#' 
#' @examples
#' # the yearly CO2 emissions of Germany
#' kaya_eq(pop = 82.4, gdp = 44, enInt = 5, carbInt = 0.05)
#' 
#' @export

kaya_eq <- function(pop, gdp, enInt, carbInt) {
  assert_numeric(pop, lower = 0, any.missing = FALSE)
  assert_numeric(gdp, lower = 0, any.missing = FALSE)
  assert_numeric(enInt, lower = 0, any.missing = FALSE)
  assert_numeric(carbInt, lower = 0, any.missing = FALSE)
  
  co2 <- pop * gdp * enInt * carbInt
  co2
}
