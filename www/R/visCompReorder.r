#' Function to visualise multiple component planes reorded within a sheet-shape rectangle grid
#'
#' \code{visCompReorder} is supposed to visualise multiple component planes reorded within a sheet-shape rectangle grid
#'
#' @param sMap an object of class "sMap"
#' @param sReorder an object of class "sReorder"
#' @param margin margins as units of length 4 or 1
#' @param height a numeric value specifying the height of device
#' @param title.rotate the rotation of the title
#' @param title.xy the coordinates of the title
#' @param colormap short name for the colormap. It can be one of "jet" (jet colormap), "bwr" (blue-white-red colormap), "gbr" (green-black-red colormap), "wyr" (white-yellow-red colormap), "br" (black-red colormap), "yr" (yellow-red colormap), "wb" (white-black colormap), and "rainbow" (rainbow colormap, that is, red-yellow-green-cyan-blue-magenta). Alternatively, any hyphen-separated HTML color names, e.g. "blue-black-yellow", "royalblue-white-sandybrown", "darkgreen-white-darkviolet". A list of standard color names can be found in \url{http://html-color-codes.info/color-names}
#' @param ncolors the number of colors specified
#' @param zlim the minimum and maximum z values for which colors should be plotted, defaulting to the range of the finite values of z. Each of the given colors will be used to color an equispaced interval of this range. The midpoints of the intervals cover the range, so that values just outside the range will be plotted
#' @param border.color the border color for each hexagon
#' @param gp an object of class "gpar". It is the output from a call to the function "gpar" (i.e., a list of graphical parameter settings)
#' @return 
#' invisible
#' @note none
#' @export
#' @seealso \code{\link{visVp}}, \code{\link{visHexComp}}, \code{\link{visColorbar}}, \code{\link{sCompReorder}}
#' @include visCompReorder.r
#' @examples
#' # 1) generate data with an iid matrix of 1000 x 9
#' data <- cbind(matrix(rnorm(1000*3,mean=0,sd=1), nrow=1000, ncol=3), 
#' matrix(rnorm(1000*3,mean=0.5,sd=1), nrow=1000, ncol=3), 
#' matrix(rnorm(1000*3,mean=-0.5,sd=1), nrow=1000, ncol=3))
#' colnames(data) <- c("S1","S1","S1","S2","S2","S2","S3","S3","S3")
#'
#' # 2) sMap resulted from using by default setup
#' sMap <- sPipeline(data=data)
#'
#' # 3) reorder component planes
#' sReorder <- sCompReorder(sMap=sMap, amplifier=2, metric="none")
#'
#' # 4) visualise multiple component planes reorded within a sheet-shape rectangle grid
#' visCompReorder(sMap=sMap, sReorder=sReorder, margin=rep(0.1,4), height=7, 
#' title.rotate=0, title.xy=c(0.45, 1), colormap="gbr", ncolors=10, zlim=c(-1,1), 
#' border.color="transparent")

visCompReorder <-function (sMap, sReorder, margin=rep(0.1,4), height=7, title.rotate=0, title.xy=c(0.45, 1), colormap=c("bwr","jet","gbr","wyr","br","yr","rainbow","wb"), ncolors=40, zlim=NULL, border.color="transparent", gp=grid::gpar())
{
    
    #colormap <- match.arg(colormap)
    
    if (class(sMap) != "sMap"){
        stop("The funciton must apply to 'sMap' object.\n")
    }
    codebook <- sMap$codebook
    cnames <- colnames(codebook)
    if(is.null(cnames)){
        cnames <- seq(1,ncol(codebook))
    }

    vmin <- floor(quantile(codebook, 0.05))
    vmax <- ceiling(quantile(codebook, 0.95))
    if(vmin < 0 & vmax > 0){
        vsym <- abs(min(vmin, vmax))
        vmin <- -1*vsym
        vmax <- vsym
    }
    if(!is.null(zlim)){
        if(zlim[1] < floor(min(codebook)) | zlim[2] > ceiling(max(codebook))){
            #zlim <- c(vmin,vmax)
        }
    }else{
        zlim <- c(vmin,vmax)
    }

    xdim <- sMap$xdim
    ydim <- sMap$ydim
    aspect <- ceiling(ydim/xdim)
    
    if (class(sReorder) != "sReorder"){
        stop("The funciton must apply to 'sReorder' object.\n")
    }
    coord <- sReorder$coord
    rowNum <- sReorder$ydim +1 # top row for additional one
    colNum <- sReorder$xdim +1 # right-most column for additional one

    vpnames <- visVp(height=height, xdim=xdim, ydim=ydim, colNum=colNum, rowNum=rowNum, gp=grid::gpar(col="transparent", fill="transparent"))

    ## current.vpTree(all=T)
    for (k in 1:length(cnames)) {
        
        xy <- coord[k,]
        tmp_vpname <- paste(c("R",xy[2],"C",xy[1]), collapse="")
        grid::seekViewport(tmp_vpname)
        
        ## grid::grid.rect(gp=gpar(col="gray"))
        ## current.vpTree(FALSE)
        
        grid::grid.text(cnames[k], x=grid::unit(title.xy[1],"npc"), y=grid::unit(title.xy[2],"npc"), just=c("left","top"), rot=title.rotate, gp=gp)
        visHexComp(sMap=sMap, comp=codebook[,k], margin=margin, area.size=1, colormap=colormap, ncolors=max(ncolors*5, 100), zlim=zlim, border.color=border.color, newpage=F)
    }
    
    ## colorbar
    for (k in 1:length(vpnames)) {
      
        grid::seekViewport(vpnames[k])
        flag <- grep("colorbar", vpnames[k])
        if(!identical(flag,integer(0))){
            visColorbar(colormap=colormap, ncolors=ncolors, zlim=zlim, gp=gp)
        }
    }
    
    invisible()
}