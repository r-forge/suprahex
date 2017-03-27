#' Function to visualise a supra-hexagonal grid
#'
#' \code{visHexGrid} is supposed to visualise a supra-hexagonal grid
#'
#' @param hbin an object of class "hexbin"
#' @param area.size an inteter or a vector specifying the area size of each hexagon
#' @param border.color the border color for each hexagon
#' @param fill.color the filled color for each hexagon
#' @param lty the line type for each hexagon. 0 for 'blank', 1 for 'solid', 2 for 'dashed', 3 for 'dotted', 4 for 'dotdash', 5 for 'longdash', 6 for 'twodash'
#' @param lwd the line width for each hexagon
#' @param lineend the line end style for each hexagon. It can be one of 'round', 'butt' and 'square'
#' @param linejoin the line join style for each hexagon. It can be one of 'round', 'mitre' and 'bevel'
#' @return 
#' invisible
#' @note none
#' @export
#' @seealso \code{\link{visHexComp}}
#' @include visHexGrid.r
#' @examples
#' # 1) generate an iid normal random matrix of 100x10 
#' data <- matrix( rnorm(100*10,mean=0,sd=1), nrow=100, ncol=10)
#' colnames(data) <- paste(rep('S',10), seq(1:10), sep="")
#' 
#' # 2) sMap resulted from using by default setup
#' sMap <- sPipeline(data=data)
#' 
#' # 3) create an object of "hexbin" class from sMap
#' dat <- data.frame(sMap$coord)
#' xdim <- sMap$xdim
#' ydim <- sMap$ydim
#' hbin <- hexbin::hexbin(dat$x, dat$y, xbins=xdim-1, shape=sqrt(0.75)*ydim/xdim)
#' 
#' # 4) visualise hbin object
#' vp <- hexbin::hexViewport(hbin)
#' visHexGrid(hbin)

visHexGrid <- function(hbin, area.size=1, border.color=NULL, fill.color=NULL, lty=1, lwd=1, lineend="round", linejoin="round") 
{
    if (class(hbin) != "hexbin"){
        stop("The funciton must apply to a 'hexbin' object.\n")
    }
    
    hexC <- list()
    hexC$x <- c(0.5,0.5,0,-0.5,-0.5,0)
    hexC$y <- c(-1*sqrt(3)/6, sqrt(3)/6, sqrt(3)/3, sqrt(3)/6, -1*sqrt(3)/6, -1*sqrt(3)/3)

    xnew <- hbin@xcm
    ynew <- hbin@ycm
    n <- length(xnew)
    
    minSize <- 0.25
    if(length(area.size) == n){
        scaled <- (area.size - min(area.size))/(max(area.size)-min(area.size))
        scaled[scaled <= minSize] <- minSize ## replace those <= minSize with minSize
        area.size <- scaled
    }else if(length(area.size) == 1){
        if(area.size > 1 | area.size <= minSize){
            area.size <- 1
        }
    }else{
        area.size <- 1
    }
    
    radius <- rep.int(1,n) * area.size
    n6 <- rep.int(6:6, n)
    
    pltx <- rep.int(hexC$x, n) * rep.int(radius, n6) + rep.int(xnew, n6)
    plty <- rep.int(hexC$y, n) * rep.int(radius, n6) + rep.int(ynew, n6)
    grid::grid.polygon(pltx, plty, default.units="native", id=NULL, id.lengths=n6, gp=grid::gpar(fill=fill.color, col=border.color, lty=lty, lwd=lwd, lineend=lineend, linejoin=linejoin))
    
    invisible()
}