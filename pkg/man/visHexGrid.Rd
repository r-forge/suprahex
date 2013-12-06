\name{visHexGrid}
\alias{visHexGrid}
\title{Function to visualise a supra-hexagonal grid}
\usage{
  visHexGrid(hbin, area.size = 1, border.color = NULL,
    fill.color = NULL)
}
\arguments{
  \item{hbin}{an object of class "hexbin"}

  \item{area.size}{an inteter or a vector specifying the
  area size of each hexagon}

  \item{border.color}{the border color for each hexagon}

  \item{fill.color}{the filled color for each hexagon}
}
\value{
  invisible
}
\description{
  \code{visHexGrid} is supposed to visualise a
  supra-hexagonal grid
}
\note{
  none
}
\examples{
# 1) generate an iid normal random matrix of 100x10
data <- matrix( rnorm(100*10,mean=0,sd=1), nrow=100, ncol=10)
colnames(data) <- paste(rep('S',10), seq(1:10), sep="")

# 2) sMap resulted from using by default setup
sMap <- sPipeline(data=data)

# 3) create an object of "hexbin" class from sMap
dat <- data.frame(sMap$coord)
xdim <- sMap$xdim
ydim <- sMap$ydim
hbin <- hexbin::hexbin(dat$x, dat$y, xbins=xdim-1, shape=sqrt(0.75)*ydim/xdim)

# 4) visualise hbin object
vp <- hexbin::hexViewport(hbin)
visHexGrid(hbin)
}
\seealso{
  \code{\link{visHexComp}}
}

