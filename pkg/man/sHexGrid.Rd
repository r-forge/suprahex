\name{sHexGrid}
\alias{sHexGrid}
\title{Function to define a supra-hexagonal grid}
\usage{
sHexGrid(r = NULL, nHex = NULL)
}
\arguments{
\item{r}{an integer specifying the radius in a
supra-hexagonal grid}

\item{nHex}{the number of input hexagons in the grid}
}
\value{
a list with following components: \itemize{ \item{\code{r}:
the grid radius} \item{\code{nHex}: the total number of
hexagons in the grid. It may differ from the input value;
actually it is always no less than the input one to ensure
a supra-hexagonal grid exactly formed}
\item{\code{centroid}: the 2D coordinates of the grid
centroid} \item{\code{stepCentroid}: a vector with the
length of nHex. It stores how many steps a hexagon is awawy
from the grid centroid ('1' for the centroid itself).
Starting with the centroid, it orders outward. Also, for
those hexagons of the same step, it orders from the
rightmost in an anti-clock wise} \item{\code{coord}: a
matrix of nHex x 2 with each row specifying the 2D
coordinates of a hexagon in the grid. The order of rows is
the same as 'centroid' above} \item{\code{call}: the call
that produced this result} }
}
\description{
\code{sHexGrid} is supposed to define a supra-hexagonal map
grid. A supra-hexagon is a giant hexagon, which seamlessly
consists of smaller hexagons. Due to the symmetric nature,
it can be uniquely determined by specifying the radius away
from the grid centroid. This function takes input the grid
radius (or the number of hexagons in the grid, but will be
adjusted to meet the definition of supra-hexagon), and
returns a list (see 'Value' below) containing: the grid
radius, the total number of hexagons in the grid, the 2D
coordinates of the grid centroid, the step for each hexogan
away from the grid centroid, and the 2D coordinates of all
hexagons in the grid.
}
\note{
The relationships among return values: \itemize{
\item{\eqn{nHex = 1+6*r*(r-1)/2}} \item{\eqn{centroid =
coord[1,]}} \item{\eqn{stepCentroid[1] = 1}}
\item{\eqn{stepCentroid[2:nHex] = unlist(sapply(2:r,
function(x) (c( (1+6*x*(x-1)/2-6*(x-1)+1) : (1+6*x*(x-1)/2)
)>=1)*x ))}} }
}
\examples{
# The supra-hexagonal grid is exactly determined by specifying the radius.
res <- sHexGrid(r=2)

# The grid is determined according to the number of input hexagons (after being adjusted).
# The return res$nHex is always no less than the input one.
# It ensures a supra-hexagonal grid is exactly formed.
res <- sHexGrid(nHex=12)

# Ignore input nHex if r is also given
res <- sHexGrid(r=3, nHex=100)

# By default, r=3 if no parameters are specified
res <- sHexGrid()
}
\seealso{
\code{\link{sTopology}}
}

