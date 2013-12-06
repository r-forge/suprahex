\name{visDmatCluster}
\alias{visDmatCluster}
\title{Function to visualise clusters/bases partitioned from a supra-hexagonal grid}
\usage{
  visDmatCluster(sMap, sBase, height = 7,
    margin = rep(0.1, 4), area.size = 1,
    gp = grid::gpar(cex = 0.8, font = 2, col.label = "black"),
    border.color = "transparent",
    colormap = c("rainbow", "jet", "bwr", "gbr", "wyr", "br", "yr", "wb"),
    clip = c("on", "inherit", "off"), newpage = T)
}
\arguments{
  \item{sMap}{an object of class "sMap"}

  \item{sBase}{an object of class "sBase"}

  \item{height}{a numeric value specifying the height of
  device}

  \item{margin}{margins as units of length 4 or 1}

  \item{area.size}{an inteter or a vector specifying the
  area size of each hexagon}

  \item{gp}{an object of class "gpar". It is the output
  from a call to the function "gpar" (i.e., a list of
  graphical parameter settings)}

  \item{border.color}{the border color for each hexagon}

  \item{colormap}{short name for the colormap. It can be
  one of "jet" (jet colormap), "bwr" (blue-white-red
  colormap), "gbr" (green-black-red colormap), "wyr"
  (white-yellow-red colormap), "br" (black-red colormap),
  "yr" (yellow-red colormap), "wb" (white-black colormap),
  and "rainbow" (rainbow colormap, that is,
  red-yellow-green-cyan-blue-magenta). Alternatively, any
  hyphen-separated HTML color names, e.g.
  "blue-black-yellow", "royalblue-white-sandybrown",
  "darkgreen-white-darkviolet". A list of standard color
  names can be found in
  \url{http://html-color-codes.info/color-names}}

  \item{clip}{either "on" for clipping to the extent of
  this viewport, "inherit" for inheriting the clipping
  region from the parent viewport, or "off" to turn
  clipping off altogether}

  \item{newpage}{logical to indicate whether to open a new
  page. By default, it sets to true for opening a new page}
}
\value{
  invisible
}
\description{
  \code{visDmatCluster} is supposed to visualise
  clusters/bases partitioned from a supra-hexagonal grid
}
\note{
  none
}
\examples{
# 1) generate an iid normal random matrix of 100x10
data <- matrix( rnorm(100*10,mean=0,sd=1), nrow=100, ncol=10)

# 2) get trained using by default setup
sMap <- sPipeline(data=data)

# 3) partition the grid map into clusters using region-growing algorithm
sBase <- sDmatCluster(sMap=sMap, which_neigh=1,
distMeasure="median", clusterLinkage="average")

# 4) visualise clusters/bases partitioned from the sMap
visDmatCluster(sMap,sBase)
}
\seealso{
  \code{\link{sDmatCluster}}, \code{\link{visColormap}},
  \code{\link{visHexGrid}}
}

