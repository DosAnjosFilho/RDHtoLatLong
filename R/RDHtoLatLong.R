#' Converte 'Rijksdriehoek' (RDH) Coordinates to Lat and Long (WGS84)
#'
#' @param x x vector of x-coordinates in rijksdriehoek system.
#' @param y y vector of y-coordinates in rijksdriehoek system.
#'
#' @return A vector of length 2 with latitude and longitude.
#'
#' @examples
#' # Converting the coordinates of the city of Amsterdam in RDH to Lat Long
#' x <- 121687
#' y <- 487484
#'
#' RDHtoLatLong(x, y)
#'
#' @export

RDHtoLatLong <- function(x, y){

# The city "Amsterfoort" is used as reference "Rijksdriehoek" coordinate.
referenceRdX = 155000
referenceRdY = 463000

dX = (x - referenceRdX) * (10^(-5))
dY = (y - referenceRdY) * (10^(-5))

sumN =
  (3235.65389 * dY) +
  (-32.58297 * (dX^2)) +
  (-0.2475 * (dY^2)) +
  (-0.84978 * (dX^2) * dY) +
  (-0.0655 * (dY^3)) +
  (-0.01709 * (dX^2) * (dY^2)) +
  (-0.00738 * dX) +
  (0.0053 * (dX^4)) +
  (-0.00039 * (dX^2) * (dY^3)) +
  (0.00033 * (dX^4) * dY) +
  (-0.00012 * dX * dY)

sumE =
  (5260.52916 * dX) +
  (105.94684 * dX * dY) +
  (2.45656 * dX * (dY^2)) +
  (-0.81885 * (dX^3)) +
  (0.05594 * dX * (dY^3)) +
  (-0.05607 * (dX^3) * dY) +
  (0.01199 * dY) +
  (-0.00256 * (dX^3) * (dY^2)) +
  (0.00128 * dX * (dY^4)) +
  (0.00022 * (dY^2)) +
  (-0.00022 * (dX^2)) +
  (0.00026 * (dX^5))

# The city "Amsterfoort" is used as reference "WGS84" coordinate.
referenceWgs84X = 52.15517
referenceWgs84Y = 5.387206
latitude = referenceWgs84X + (sumN / 3600)
longitude = referenceWgs84Y + (sumE / 3600)

lat <- round(latitude, digits = 6)
long <- round(longitude, digits = 6)

return(c(lat, long))
}
