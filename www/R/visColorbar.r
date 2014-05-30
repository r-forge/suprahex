#' Function to define a colorbar
#'
#' \code{visColorbar} is supposed to define a colorbar
#'
#' @param colormap short name for the colormap. It can be one of "jet" (jet colormap), "bwr" (blue-white-red colormap), "gbr" (green-black-red colormap), "wyr" (white-yellow-red colormap), "br" (black-red colormap), "yr" (yellow-red colormap), "wb" (white-black colormap), and "rainbow" (rainbow colormap, that is, red-yellow-green-cyan-blue-magenta). Alternatively, any hyphen-separated HTML color names, e.g. "blue-black-yellow", "royalblue-white-sandybrown", "darkgreen-white-darkviolet". A list of standard color names can be found in \url{http://html-color-codes.info/color-names}
#' @param ncolors the number of colors specified
#' @param zlim the minimum and maximum z values for which colors should be plotted, defaulting to the range of the finite values of z. Each of the given colors will be used to color an equispaced interval of this range. The midpoints of the intervals cover the range, so that values just outside the range will be plotted
#' @param gp an object of class gpar, typically the output from a call to the function gpar (i.e., a list of graphical parameter settings)
#' @return 
#' invisibly
#' @note none
#' @export
#' @seealso \code{\link{visColormap}}, \code{\link{visHexMulComp}}, \code{\link{visCompReorder}}
#' @include visColorbar.r
#' @examples
#' # draw "blue-white-red" colorbar
#' visColorbar(colormap="bwr")

visColorbar <-function (colormap=c("bwr","jet","gbr","wyr","br","yr","rainbow","wb"), ncolors=40, zlim=c(0,1), gp=grid::gpar()) 
{

    #colormap <- match.arg(colormap)
    palette.name <- visColormap(colormap=colormap) 
    
    colors <- palette.name(ncolors)
    scale <- length(colors)
    lab.scale <- length(colors)/(zlim[2]-zlim[1])
    
    for (i in 1:length(colors)) {
        y <- (i-1)/scale
        
        xValue <- 0.2
        wValue <- 0.08
        xtValue <- xValue+wValue
        
        grid::grid.rect(x=grid::unit(xValue,"npc"), y=grid::unit(y,"npc"), width=grid::unit(wValue,"npc"), height=grid::unit(1/scale,"npc"), just=c("left","bottom"), gp=grid::gpar(col=colors[i], fill=colors[i]))
        
        if(i == 1 | i == 1+length(colors)/2){
            tx <- (i-1)/lab.scale + zlim[1]
            grid::grid.text(tx, x=grid::unit(xtValue*1.1,"npc"), y=grid::unit(y-0.1*1/scale,"npc"), just=c("left","center"), gp=gp)
        }
        
        if(i == length(colors)){
            y <- i/scale
            tx <- i/lab.scale + zlim[1]
            grid::grid.text(tx, x=grid::unit(xtValue*1.1,"npc"), y=grid::unit(y-0.1*1/scale,"npc"), just=c("left","center"), gp=gp)
        }
    }
    
    invisible()
}