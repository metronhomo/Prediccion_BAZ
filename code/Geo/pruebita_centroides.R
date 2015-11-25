centroide <- function(x){
  x0 = x[1:nrow(x)-1,1]
  x1 = x[2:nrow(x),1]
  y0 = x[1:nrow(x)-1,2]
  y1 = x[2:nrow(x),2]
  A = 0.5*sum(x0*y1 - x1*y0)
  cons = x0*y1 - x1*y0
  cx = sum((x0+x1)*cons)/(6*A)
  cy = sum((y0+y1)*cons)/(6*A)
  return(c(cx, cy))
}

coords <- matrix(c(1,2,3,4,5,6,7,8,9, 1,10, 3, 3, 6, 4, 8, 3, 6,4,10), ncol=2)
coords2 <- matrix(c(0, 0,10, 10,0, 0,10, 10,0,0), ncol=2)
coords3 <- matrix(c(15, 0,25, 15,35, 0,15, 0), ncol=2)

library(geosphere)
library(rgeos)
x = readWKT(paste("GEOMETRYCOLLECTION(POLYGON((0 0,10 0,10 10,0 10,0 0)),",
                  "POLYGON((15 0,25 15,35 0,15 0)))"))

centroid(coords3)
gCentroid(x)
