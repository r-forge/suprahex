\name{sTopology}
\alias{sTopology}
\title{Function to define the topology of a map grid}
\usage{
sTopology(data = NULL, xdim = NULL, ydim = NULL, nHex = NULL,
lattice = c("hexa", "rect"), shape = c("suprahex", "sheet"))
}
\arguments{
\item{data}{a data frame or matrix of input data}

\item{xdim}{an integer specifying x-dimension of the
grid}

\item{ydim}{an integer specifying y-dimension of the
grid}

\item{nHex}{the number of hexagons/rectangles in the
grid}

\item{lattice}{the grid lattice, either "hexa" for a
hexagon or "rect" for a rectangle}

\item{shape}{the grid shape, either "suprahex" for a
supra-hexagonal grid or "sheet" for a hexagonal/rectangle
sheet}
}
\value{
an object of class "sTopol", a list with following
components: \itemize{ \item{\code{nHex}: the total number
of hexagons/rectanges in the grid. It is not always the
same as the input nHex (if any); see "Note" below for the
explaination} \item{\code{xdim}: x-dimension of the grid}
\item{\code{ydim}: y-dimension of the grid}
\item{\code{lattice}: the grid lattice} \item{\code{shape}:
the grid shape} \item{\code{coord}: a matrix of nHex x 2,
with each row corresponding to the coordinates of a
hexagon/rectangle in the 2D map grid} \item{\code{call}:
the call that produced this result} }
}
\description{
\code{sTopology} is supposed to define the topology of a 2D
map grid. The topological shape can be either a
supra-hexagonal grid or a hexagonal/rectangle sheet. It
returns an object of "sTopol" class, containing: the total
number of hexagons/rectangles in the grid, the grid
xy-dimensions, the grid lattice, the grid shape, and the 2D
coordinates of all hexagons/rectangles in the grid. The 2D
coordinates can be directly used to measure distances
between any pair of lattice hexagons/rectangles.
}
\note{
The output of nHex depends on the input arguments and grid
shape: \itemize{ \item{How the input parameters are used to
determine nHex is taken priority in the following order:
"xdim & ydim" > "nHex" > "data"} \item{If both of xdim and
ydim are given, \eqn{nHex=xdim*ydim} for the "sheet" shape,
\eqn{r=(min(xdim,ydim)+1)/2} for the "suprahex" shape}
\item{If only data is input, \eqn{nHex=5*sqrt(dlen)}, where
dlen is the number of rows of the input data} \item{With
nHex in hand, it depends on the grid shape:} \itemize{
\item{For "sheet" shape, xy-dimensions of sheet grid is
determined according to the square root of the two biggest
eigenvalues of the input data} \item{For "suprahex" shape,
see \code{\link{sHexGrid}} for calculating the grid radius
r. The xdim (and ydim) is related to r via
\eqn{xdim=2*r-1}} } }
}
\examples{
# For "suprahex" shape
sTopol <- sTopology(xdim=3, ydim=3, lattice="hexa", shape="suprahex")

# Error: "The suprahex shape grid only allows for hexagonal lattice"
# sTopol <- sTopology(xdim=3, ydim=3, lattice="rect", shape="suprahex")

# For "sheet" shape with hexagonal lattice
sTopol <- sTopology(xdim=3, ydim=3, lattice="hexa", shape="sheet")

# For "sheet" shape with rectangle lattice
sTopol <- sTopology(xdim=3, ydim=3, lattice="rect", shape="sheet")

# By default, nHex=19 (i.e., r=3; xdim=ydim=5) for "suprahex" shape
sTopol <- sTopology(shape="suprahex")

# By default, xdim=ydim=5 (i.e., nHex=25) for "sheet" shape
sTopol <- sTopology(shape="sheet")

# Determine the topolopy of a supra-hexagonal grid based on input data
# 1) generate an iid normal random matrix of 100x10
data <- matrix(rnorm(100*10,mean=0,sd=1), nrow=100, ncol=10)
# 2) from this input matrix, determine nHex=5*sqrt(nrow(data))=50,
# but it returns nHex=61, via "sHexGrid(nHex=50)", to make sure a supra-hexagonal grid
sTopol <- sTopology(data=data, lattice="hexa", shape="suprahex")

# visualise a supre-hexagonal grid
visHexMapping(sTopol,mappingType="indexes")
}
\seealso{
\code{\link{sHexGrid}}, \code{\link{visHexMapping}}
}

