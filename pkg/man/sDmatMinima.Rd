\name{sDmatMinima}
\alias{sDmatMinima}
\title{Function to identify local minima (in 2D output space) of distance matrix (in high-dimensional input space)}
\usage{
sDmatMinima(sMap, which_neigh = 1, distMeasure = c("median", "mean",
"min",
"max"))
}
\arguments{
\item{sMap}{an object of class "sMap"}

\item{which_neigh}{which neighbors in 2D output space are
used for the calculation. By default, it sets to "1" for
direct neighbors, and "2" for neighbors within neighbors
no more than 2, and so on}

\item{distMeasure}{distance measure used to calculate
distances in high-dimensional input space. It can be one
of "median", "mean", "min" and "max" measures}
}
\value{
\itemize{ \item{\code{minima}: a vector to store a list of
local minima (represented by the indexes of
hexogans/rectangles} }
}
\description{
\code{sDmatMinima} is supposed to identify local minima of
distance matrix (resulting from \code{\link{sDmat}}). The
criterion of being local minima is that the distance
associated with a hexagon/rectangle is always smaller than
its direct neighbors (i.e., 1-neighborhood)
}
\note{
Do not get confused by "which_neigh" and the criteria of
being local minima. Both of them deal with 2D output space.
However, "which_neigh" is used to assist in the calculation
of distance matrix (so can be 1-neighborhood or more);
instead, the criterion of being local minima is only
1-neighborhood in the strictest sense
}
\examples{
# 1) generate an iid normal random matrix of 100x10
data <- matrix( rnorm(100*10,mean=0,sd=1), nrow=100, ncol=10)

# 2) get trained using by default setup
sMap <- sPipeline(data=data)

# 3) identify local minima of distance matrix based on "median" distances and direct neighbors
minima <- sDmatMinima(sMap=sMap, which_neigh=1, distMeasure="median")
}
\seealso{
\code{\link{sDmat}}, \code{\link{sNeighAny}}
}

