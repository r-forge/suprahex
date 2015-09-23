#' Function to visualise multiple component planes of a supra-hexagonal grid
#'
#' \code{visHexMulComp} is supposed to visualise multiple component planes of a supra-hexagonal grid
#'
#' @param sMap an object of class "sMap"
#' @param which.components an integer vector specifying which compopnets will be visualised. By default, it is NULL meaning all components will be visualised
#' @param rect.grid a vector specifying the number of rows and columns for a rectangle grid wherein the component planes are placed. By defaul, it is NULL (decided on according to the number of component planes that will be visualised)
#' @param margin margins as units of length 4 or 1
#' @param height a numeric value specifying the height of device
#' @param title.rotate the rotation of the title
#' @param title.xy the coordinates of the title
#' @param colormap short name for the colormap. It can be one of "jet" (jet colormap), "bwr" (blue-white-red colormap), "gbr" (green-black-red colormap), "wyr" (white-yellow-red colormap), "br" (black-red colormap), "yr" (yellow-red colormap), "wb" (white-black colormap), and "rainbow" (rainbow colormap, that is, red-yellow-green-cyan-blue-magenta). Alternatively, any hyphen-separated HTML color names, e.g. "blue-black-yellow", "royalblue-white-sandybrown", "darkgreen-white-darkviolet". A list of standard color names can be found in \url{http://html-color-codes.info/color-names}
#' @param ncolors the number of colors specified
#' @param zlim the minimum and maximum z values for which colors should be plotted, defaulting to the range of the finite values of z. Each of the given colors will be used to color an equispaced interval of this range. The midpoints of the intervals cover the range, so that values just outside the range will be plotted
#' @param border.color the border color for each hexagon
#' @param gp an object of class gpar, typically the output from a call to the function gpar (i.e., a list of graphical parameter settings)
#' @return 
#' invisible
#' @note none
#' @export
#' @seealso \code{\link{visVp}}, \code{\link{visHexComp}}, \code{\link{visColorbar}}
#' @include visHexMulComp.r
#' @examples
#' # 1) generate data with an iid matrix of 1000 x 3
#' data <- cbind(matrix(rnorm(1000*3,mean=0,sd=1), nrow=1000, ncol=3), 
#' matrix(rnorm(1000*3,mean=0.5,sd=1), nrow=1000, ncol=3), 
#' matrix(rnorm(1000*3,mean=-0.5,sd=1), nrow=1000, ncol=3))
#' colnames(data) <- c("S1","S1","S1","S2","S2","S2","S3","S3","S3")
#'
#' # 2) sMap resulted from using by default setup
#' sMap <- sPipeline(data=data)
#'
#' # 3) visualise multiple component planes of a supra-hexagonal grid
#' visHexMulComp(sMap, colormap="jet", ncolors=20, zlim=c(-1,1), gp=grid::gpar(cex=0.8))
#' # 3a) visualise only the first 6 component planes
#' visHexMulComp(sMap, which.components=1:6, colormap="jet", ncolors=20, zlim=c(-1,1), gp=grid::gpar(cex=0.8))
#' # 3b) visualise only the first 6 component planes within the rectangle grid of 3 X 2
#' visHexMulComp(sMap, which.components=1:6, rect.grid=c(3,2), colormap="jet", ncolors=20, zlim=c(-1,1), gp=grid::gpar(cex=0.8))

visHexMulComp <-function(sMap, which.components=NULL, rect.grid=NULL, margin=rep(0.1,4), height=7, title.rotate=0, title.xy=c(0.45, 1), colormap=c("bwr","jet","gbr","wyr","br","yr","rainbow","wb"), ncolors=40, zlim=NULL, border.color="transparent", gp=grid::gpar())
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
   	
	if(all(!is.null(which.components))){
		which.components <- as.integer(which.components)
		if(all(which.components>=1 & which.components<=length(cnames))){
			codebook <- matrix(codebook[,which.components], ncol=length(which.components))
			cnames <- cnames[which.components]
			colnames(codebook) <- cnames
			sMap$codebook <- codebook
		}
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
    
    tmp_colNum <- ceiling(sqrt(length(cnames)/aspect))
    #tmp_rowNum <- ceiling((length(cnames)+1)/tmp_colNum)
    tmp_rowNum <- ceiling((length(cnames))/tmp_colNum)
    if(all(!is.null(rect.grid))){
		rect.grid[1] <- as.integer(rect.grid[1])
		rect.grid[2] <- as.integer(rect.grid[2])
		if(rect.grid[1]>0 & rect.grid[2]>0){
			tmp_colNum <- rect.grid[2]
			tmp_rowNum <- rect.grid[1]
		}
	}
	colNum <- tmp_colNum+1
    rowNum <- tmp_rowNum+1
    tolNum <- colNum*rowNum
    if(tolNum < length(cnames)+colNum+rowNum-1){
        rowNum <- rowNum+1
    }
    
    vpnames <- visVp(height=height, xdim=xdim, ydim=ydim, colNum=colNum, rowNum=rowNum, gp=grid::gpar(col="transparent", fill="transparent"))

    ## current.vpTree(all=T)
    t <- 0
    for (k in 1:length(vpnames)) {
    
        grid::seekViewport(vpnames[k])
        ## grid.rect(gp=grid::gpar(col="gray"))
        grid::current.vpTree(FALSE)
      
        flag1 <- grep("R0", vpnames[k])
        flag2 <- grep("C0", vpnames[k])
        flag3 <- grep("colorbar", vpnames[k])
        if(identical(flag1,integer(0)) & identical(flag2,integer(0)) & identical(flag3,integer(0))){
            t <- t+1
            if(t <= length(cnames)){
                grid::grid.text(cnames[t], x=grid::unit(title.xy[1],"npc"), y=grid::unit(title.xy[2],"npc"), just=c("left","top"), rot=title.rotate, gp=gp)
                visHexComp(sMap=sMap, comp=codebook[,t], margin=margin, area.size=1, colormap=colormap, ncolors=ncolors, zlim=zlim, border.color=border.color, newpage=F)
            }
        }else if(!identical(flag3,integer(0))){
            visColorbar(colormap=colormap, ncolors=ncolors, zlim=zlim, gp=gp)
        }
    }
    #grid::seekViewport("colorbar")
    #visColorbar(colormap=colormap, ncolors=ncolors, zlim=zlim, gp=gp)
    
    invisible()
}