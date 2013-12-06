#' Function to visualise clusters/bases partitioned from a supra-hexagonal grid
#'
#' \code{visDmatCluster} is supposed to visualise clusters/bases partitioned from a supra-hexagonal grid
#'
#' @param sMap an object of class "sMap"
#' @param sBase an object of class "sBase"
#' @param height a numeric value specifying the height of device
#' @param margin margins as units of length 4 or 1
#' @param area.size an inteter or a vector specifying the area size of each hexagon
#' @param gp an object of class "gpar". It is the output from a call to the function "gpar" (i.e., a list of graphical parameter settings)
#' @param border.color the border color for each hexagon
#' @param colormap short name for the colormap. It can be one of "jet" (jet colormap), "bwr" (blue-white-red colormap), "gbr" (green-black-red colormap), "wyr" (white-yellow-red colormap), "br" (black-red colormap), "yr" (yellow-red colormap), "wb" (white-black colormap), and "rainbow" (rainbow colormap, that is, red-yellow-green-cyan-blue-magenta). Alternatively, any hyphen-separated HTML color names, e.g. "blue-black-yellow", "royalblue-white-sandybrown", "darkgreen-white-darkviolet". A list of standard color names can be found in \url{http://html-color-codes.info/color-names}
#' @param clip either "on" for clipping to the extent of this viewport, "inherit" for inheriting the clipping region from the parent viewport, or "off" to turn clipping off altogether
#' @param newpage logical to indicate whether to open a new page. By default, it sets to true for opening a new page
#' @return 
#' invisible
#' @note none
#' @export
#' @seealso \code{\link{sDmatCluster}}, \code{\link{visColormap}}, \code{\link{visHexGrid}}
#' @include visDmatCluster.r
#' @examples
#' # 1) generate an iid normal random matrix of 100x10 
#' data <- matrix( rnorm(100*10,mean=0,sd=1), nrow=100, ncol=10) 
#'
#' # 2) get trained using by default setup
#' sMap <- sPipeline(data=data)
#'
#' # 3) partition the grid map into clusters using region-growing algorithm
#' sBase <- sDmatCluster(sMap=sMap, which_neigh=1, 
#' distMeasure="median", clusterLinkage="average")
#' 
#' # 4) visualise clusters/bases partitioned from the sMap
#' visDmatCluster(sMap,sBase)

visDmatCluster <- function (sMap, sBase, height=7, margin=rep(0.1,4), area.size=1, gp=grid::gpar(cex=0.8, font=2, col.label="black"),  border.color="transparent", colormap=c("rainbow","jet","bwr","gbr","wyr","br","yr","wb"), clip=c("on","inherit","off"), newpage=T)
{
    
    #colormap <- match.arg(colormap)
    palette.name <- visColormap(colormap=colormap)
    
    if (class(sMap) != "sMap"){
        stop("The funciton must apply to 'sMap' object.\n")
    }
    
    if (class(sBase) != "sBase"){
        stop("The funciton must apply to 'sBase' object.\n")
    }
    
    dat <- data.frame(sMap$coord)
    xdim <- sMap$xdim
    ydim <- sMap$ydim
    nHex <- sMap$nHex
    
    hbin <- hexbin::hexbin(dat$x, dat$y, xbins=xdim-1, shape=sqrt(0.75)*ydim/xdim)
        
    hbin@cell <- 1:nrow(dat)
    hbin@ncells <- nrow(dat)
    hbin@count <- rep(1,nrow(dat))
    hbin@xcm <- dat$x
    hbin@ycm <- dat$y
    
    if (newpage){
        #grid::grid.newpage()
        dev.new(width=height*xdim/ydim, height=height)
    }
    
    legend <- 0
    vp <- hexbin::hexViewport(hbin, offset=grid::unit(legend,"inches"), mar=grid::unit(margin,"lines"), xbnds=c(min(hbin@xcm)-0.5, max(hbin@xcm)+0.5), ybnds=c(min(hbin@ycm)-sqrt(0.75), max(hbin@ycm)+sqrt(0.75)))
    grid::pushViewport(vp@hexVp.off)
    
    xy <- list()
    xy$x <- dat$x
    xy$y <- dat$y
        

    labels <- rep("", length(sBase$bases))
    labels[sBase$seeds] <- as.character(seq(1,length(sBase$seeds)))
    myColor <- sample(palette.name(length(sBase$seeds)))
    fill.color <- myColor[sBase$bases]

    clip <- match.arg(clip)
    if (clip == "on") {
        grid::popViewport()
        grid::pushViewport(vp@hexVp.on)
    }
    
    visHexGrid(hbin, area.size=area.size, border.color=border.color, fill.color=fill.color)
    grid::grid.text(as.character(labels), xy$x, xy$y, gp=gp, default.units="native")
    
    invisible(vp)
}