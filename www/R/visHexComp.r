#' Function to visualise a component plane of a supra-hexagonal grid
#'
#' \code{visHexComp} is supposed to visualise a supra-hexagonal grid in the context of viewport
#'
#' @param sMap an object of class "sMap"
#' @param comp a component/column of codebook matrix from an object "sMap"
#' @param margin margins as units of length 4 or 1
#' @param area.size an inteter or a vector specifying the area size of each hexagon
#' @param colormap short name for the colormap. It can be one of "jet" (jet colormap), "bwr" (blue-white-red colormap), "gbr" (green-black-red colormap), "wyr" (white-yellow-red colormap), "br" (black-red colormap), "yr" (yellow-red colormap), "wb" (white-black colormap), and "rainbow" (rainbow colormap, that is, red-yellow-green-cyan-blue-magenta). Alternatively, any hyphen-separated HTML color names, e.g. "blue-black-yellow", "royalblue-white-sandybrown", "darkgreen-white-darkviolet". A list of standard color names can be found in \url{http://html-color-codes.info/color-names}
#' @param ncolors the number of colors specified
#' @param zlim the minimum and maximum z values for which colors should be plotted, defaulting to the range of the finite values of z. Each of the given colors will be used to color an equispaced interval of this range. The midpoints of the intervals cover the range, so that values just outside the range will be plotted
#' @param border.color the border color for each hexagon
#' @param newpage a logical to indicate whether or not to open a new page
#' @return 
#' invisible
#' @note none
#' @export
#' @import hexbin
#' @seealso \code{\link{visColormap}}, \code{\link{visHexGrid}}
#' @include visHexComp.r
#' @examples
#' # 1) generate an iid normal random matrix of 100x10 
#' data <- matrix( rnorm(100*10,mean=0,sd=1), nrow=100, ncol=10)
#' colnames(data) <- paste(rep('S',10), seq(1:10), sep="")
#'
#' # 2) sMap resulted from using by default setup
#' sMap <- sPipeline(data=data)
#'
#' # 3) visualise the first component plane with a supra-hexagonal grid
#' visHexComp(sMap, comp=sMap$codebook[,1], colormap="jet", ncolors=100, zlim=c(-1,1))

visHexComp <-function (sMap, comp, margin=rep(0.6, 4), area.size=1, colormap=c("bwr","jet","gbr","wyr","br","yr","rainbow","wb"), ncolors=40, zlim=c(0,1), border.color="transparent",newpage=T)
{

    #colormap <- match.arg(colormap)
    palette.name <- visColormap(colormap=colormap)
    
    if (class(sMap) != "sMap"){
        stop("The funciton must apply to 'sMap' object.\n")
    }
    M <- sMap$codebook
    if (is.vector(M)){
        M <- matrix(M, nrow=1, ncol=length(M))
    }
    
    #par(mar = margin)
    
    if (missing(ncolors)) {
        ncolors <- min(length(unique(comp[!is.na(comp)])), 10)
    }
    
    comp[comp < zlim[1]] <- zlim[1]
    comp[comp > zlim[2]] <- zlim[2]
    
    bgcol <- palette.name(ncolors)
    bgcolors <- rep("transparent", nrow(sMap$coord))
    showcolors <- as.integer(cut(comp, seq(zlim[1], zlim[2], length=ncolors+1), include.lowest=TRUE))
    bgcolors[!is.na(showcolors)] <- bgcol[showcolors[!is.na(showcolors)]]

    dat <- data.frame(sMap$coord)
    xdim <- sMap$xdim
    ydim <- sMap$ydim
    
    hbin <- hexbin::hexbin(dat$x, dat$y, xbins=xdim-1, shape=sqrt(0.75)*ydim/xdim)
    
    hbin@cell <- 1:nrow(dat)
    hbin@ncells <- nrow(dat)
    hbin@count <- rep(1,nrow(dat))
    hbin@xcm <- dat$x
    hbin@ycm <- dat$y
    
    if (newpage){
        grid::grid.newpage()
    }
    
    legend=0
    vp <- hexbin::hexViewport(hbin, offset=grid::unit(legend,"inches"), mar=grid::unit(margin,"lines"), xbnds=c(min(hbin@xcm)-0.5, max(hbin@xcm)+0.5), ybnds=c(min(hbin@ycm)-sqrt(0.75), max(hbin@ycm)+sqrt(0.75)))
    grid::pushViewport(vp@hexVp.off)

    visHexGrid(hbin, area.size=area.size, border.color=border.color, fill.color=bgcolors)
    
    invisible()
}