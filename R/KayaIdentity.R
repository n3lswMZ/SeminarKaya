#' Equation for Kaya Identity
#' 
#' The kaya identity expresses yearly CO2 emissions as a product of four factors
#' 
#' @param pop population size (in millions), input as non-negative numeric
#' @param gdp GDP per person (in 1000$/person), input as non-negative numeric
#' @param enInt energy intensity (in Gigajoule/$1000GDP), input as non-negative numeric
#' @param carbInt carbon intensity (in tonnes CO2/Gigajoule), input as non-negative numeric
#' 
#' @return numerical value representing the yearly CO2 emissions in million tonnes
#' 
#' @importFrom checkmate assert_numeric
#' 
#' @examples
#' # the yearly CO2 emissions of Germany
#' kaya_eq(pop = 82.4, gdp = 44, enInt = 5, carbInt = 0.05)
#' # the yearly C emissions of Germany
#' kaya_eq(pop = 82.4, gdp = 44, enInt = 5, carbInt = 0.05, outputType = "C")
#' @export

kaya_eq <- function(pop, gdp, enInt, carbInt, outputType = "CO2") {
  assert_numeric(pop, lower = 0, any.missing = FALSE)
  assert_numeric(gdp, lower = 0, any.missing = FALSE)
  assert_numeric(enInt, lower = 0, any.missing = FALSE)
  assert_numeric(carbInt, lower = 0, any.missing = FALSE)
  assert_character(outputType, pattern = "^C(O2)?$")
  
  co2 <- pop * gdp * enInt * carbInt
  if (outputType == "C") {
    co2 <- co2 / 3.67
  }
  co2
}
