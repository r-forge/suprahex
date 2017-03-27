#' Function to visualise codebook matrix using barplot for all hexagons or a specific one
#'
#' \code{visHexBarplot} is supposed to visualise codebook matrix using barplot for all hexagons or a specific one
#'
#' @param sObj an object of class "sMap" or "sTopol" or "sInit"
#' @param which.hexagon the integer specifying which hexagon to display. If NULL, all hexagons will be visualised
#' @param which.hexagon.highlight an integer vector specifying which hexagons are labelled. If NULL, all hexagons will be labelled
#' @param height a numeric value specifying the height of device
#' @param margin margins as units of length 4 or 1
#' @param colormap short name for the predifined colormap, and "customized" for custom input (see the next 'customized.color'). The predifined colormap can be one of "jet" (jet colormap), "bwr" (blue-white-red colormap), "gbr" (green-black-red colormap), "wyr" (white-yellow-red colormap), "br" (black-red colormap), "yr" (yellow-red colormap), "wb" (white-black colormap), and "rainbow" (rainbow colormap, that is, red-yellow-green-cyan-blue-magenta). Alternatively, any hyphen-separated HTML color names, e.g. "blue-black-yellow", "royalblue-white-sandybrown", "darkgreen-white-darkviolet". A list of standard color names can be found in \url{http://html-color-codes.info/color-names}
#' @param customized.color the customized color for pattern visualisation
#' @param zeropattern.color the color for zero horizental line
#' @param gp an object of class "gpar". It is the output from a call to the function "gpar" (i.e., a list of graphical parameter settings)
#' @param bar.text.cex a numerical value giving the amount by which bar text should be magnified relative to the default (i.e., 1)
#' @param bar.text.srt a numerical value giving the angle by which bar text should be orientated
#' @param newpage logical to indicate whether to open a new page. By default, it sets to true for opening a new page
#' @return 
#' invisible
#' @note none
#' @export
#' @seealso \code{\link{sPipeline}}, \code{\link{visColormap}}
#' @include visHexBarplot.r
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
#' # 3) plot codebook patterns using different types
#' # 3a) for all hexagons
#' visHexBarplot(sMap)
#' # 3b) only for the first hexagon
#' visHexBarplot(sMap, which.hexagon=1)

visHexBarplot <- function (sObj, which.hexagon=NULL, which.hexagon.highlight=NULL, height=7, margin=rep(0.1,4), colormap=c("customized","bwr","jet","gbr","wyr","br","yr","rainbow","wb"), customized.color="red", zeropattern.color="gray", gp=grid::gpar(cex=0.7,font=1,col="black"), bar.text.cex=0.8, bar.text.srt=90, newpage=TRUE)
{
    
    if(length(colormap)>1){
        colormap <- colormap[1]
    }
    
    if (class(sObj) != "sTopol" & class(sObj) != "sInit" & class(sObj) != "sMap"){
        stop("The funciton must apply to either 'sTopol' or 'sInit' or 'sMap' object.\n")
    }
    nHex <- sObj$nHex
    
    dat <- data.frame(sObj$coord)
    xdim <- sObj$xdim
    ydim <- sObj$ydim
    shape <- sObj$shape

    if(!is.null(which.hexagon)){
		dat <- data.frame(x=1,y=1)
		xdim <- 1
		ydim <- 1
		shape <- sObj$shape
		pattern <- sObj$codebook[which.hexagon, ]
    }

    if (newpage){
        dev.new(width=height*xdim/ydim, height=height)
    }
    
    if(is.null(which.hexagon)){
		visHexPattern(sObj, plotType="bars", colormap=colormap, customized.color=customized.color, alterntive.color=c("transparent","transparent"), legend=FALSE, newpage=FALSE)
		labels <- paste(sObj$hits, '@', 1:length(sObj$hits), sep='')
		if(!is.null(which.hexagon.highlight)){
			ind <- match(1:length(sObj$hits), which.hexagon.highlight)
			labels[is.na(ind)] <- ''
		}
		visHexMapping(sObj, mappingType="customized", labels=labels, border.color="transparent", gp=gp, newpage=FALSE)
    }else{

		par(mar = margin)
		xlim <- c(0, max(dat$x) + min(dat$x))
		ylim <- c(max(dat$y) + min(dat$y), 0)
		MASS::eqscplot(xlim, ylim, axes=FALSE, type="n")
		
        ncomp <- length(pattern)

        ## define the pattern colors
        if(colormap == "customized"){
            myPatternColor <- customized.color
        }else{
            palette.name <- visColormap(colormap=colormap)
            myPatternColor <- palette.name(ncomp)
        }
        
        ##################################################################################
        if(1){
            
            ## width and height of bars to be displayed
            bWidth <- 0.75 # must be within (0 1)
            bHeight <- 0.75 # must be within (0 1)
            
            ## center pattern to be displayed
            yrange <- range(pattern)
            pattern_centered <- pattern - mean(yrange)
            
            for(i in 1){
                ## for left and right x positions of rectangle
                if(ncomp%%2 == 0){ # for even
                    xLeft <- seq(dat$x[i]-bWidth/2, dat$x[i]+bWidth/2, length=ncomp+1)
                    xLeft <- xLeft[1:ncomp]
                    xRight <- xLeft + bWidth/ncomp
                }else{
                    xLeft <- seq(dat$x[i]-bWidth/2, dat$x[i]+bWidth/2, length=ncomp)    
                    xRight <- xLeft + bWidth/(ncomp-1)
                }
                
                ## for bottom and top y positions of rectangle
                if(yrange[1] < 0 & yrange[2] > 0) {
                    yzeroline <- dat$y[i]
                    lines(seq(dat$x[i]-bWidth/2, dat$x[i]+bWidth/2, length=2), rep(yzeroline,2), col=zeropattern.color, lty=1)
                    yBottom <- rep(yzeroline, ncomp)
                    yTop <- yBottom + bHeight*pattern_centered/diff(yrange)
                }else if(yrange[1] >= 0 & yrange[2] >= 0){
                    yzeroline <- dat$y[i] - 0.5*bHeight ## here 0.5 due to the range [-0.5 0.5] for pattern_centered
                    lines(seq(dat$x[i]-bWidth/2, dat$x[i]+bWidth/2, length=2), rep(yzeroline,2), col=zeropattern.color, lty=1)
                    yBottom <- rep(yzeroline, ncomp)
                    yTop <- yBottom + bHeight*pattern_centered/diff(yrange) + 0.5*bHeight
                }else if(yrange[1] <= 0 & yrange[2] <= 0){
                    yzeroline <- dat$y[i] + 0.5*bHeight ## here 0.5 due to the range [-0.5 0.5] for pattern_centered
                    lines(seq(dat$x[i]-bWidth/2, dat$x[i]+bWidth/2, length=2), rep(yzeroline,2), col=zeropattern.color, lty=1)
                    yBottom <- rep(yzeroline, ncomp)
                    yTop <- yBottom - bHeight*pattern_centered/diff(yrange) - 0.5*bHeight
                }
                
                ## draw bar
                rect(xLeft, yBottom, xRight, yTop, col=myPatternColor, border="transparent")
                text((xLeft+xRight)/2, yBottom*0.98, labels=names(pattern), srt=bar.text.srt, pos=2, cex=bar.text.cex, offset=0)
            }	
            
        }
    }
    
    invisible()
}
