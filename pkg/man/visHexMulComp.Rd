\name{visHexMulComp}
\alias{visHexMulComp}
\title{Function to visualise multiple component planes of a supra-hexagonal grid}
\usage{
visHexMulComp(sMap, margin = rep(0.1, 4), height = 7, title.rotate = 0,
title.xy = c(0.45, 1), colormap = c("bwr", "jet", "gbr", "wyr", "br",
"yr", "rainbow", "wb"), ncolors = 40, zlim = NULL,
border.color = "transparent", gp = grid::gpar())
}
\arguments{
\item{sMap}{an object of class "sMap"}

\item{margin}{margins as units of length 4 or 1}

\item{height}{a numeric value specifying the height of
device}

\item{title.rotate}{the rotation of the title}

\item{title.xy}{the coordinates of the title}

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

\item{ncolors}{the number of colors specified}

\item{zlim}{the minimum and maximum z values for which
colors should be plotted, defaulting to the range of the
finite values of z. Each of the given colors will be used
to color an equispaced interval of this range. The
midpoints of the intervals cover the range, so that
values just outside the range will be plotted}

\item{border.color}{the border color for each hexagon}

\item{gp}{an object of class gpar, typically the output
from a call to the function gpar (i.e., a list of
graphical parameter settings)}
}
\value{
invisible
}
\description{
\code{visHexMulComp} is supposed to visualise multiple
component planes of a supra-hexagonal grid
}
\note{
none
}
\examples{
# 1) generate data with an iid matrix of 1000 x 3
data <- cbind(matrix(rnorm(1000*3,mean=0,sd=1), nrow=1000, ncol=3),
matrix(rnorm(1000*3,mean=0.5,sd=1), nrow=1000, ncol=3),
matrix(rnorm(1000*3,mean=-0.5,sd=1), nrow=1000, ncol=3))
colnames(data) <- c("S1","S1","S1","S2","S2","S2","S3","S3","S3")

# 2) sMap resulted from using by default setup
sMap <- sPipeline(data=data)

# 3) visualise multiple component planes of a supra-hexagonal grid
visHexMulComp(sMap, colormap="jet", ncolors=20, zlim=c(-1,1),
gp=grid::gpar(cex=0.8))
}
\seealso{
\code{\link{visVp}}, \code{\link{visHexComp}},
\code{\link{visColorbar}}
}

