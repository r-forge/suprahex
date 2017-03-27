#' Function to create viewports for multiple supra-hexagonal grids
#'
#' \code{visVp} is supposed to create viewports, which describe rectangular regions on a graphics device and define a number of coordinate systems for each of supra-hexagonal grids.
#'
#' @param height a numeric value specifying the height of device
#' @param xdim an integer specifying x-dimension of the grid
#' @param ydim an integer specifying y-dimension of the grid
#' @param colNum an integer specifying the number of columns
#' @param rowNum an integer specifying the number of rows
#' @param gp an object of class gpar, typically the output from a call to the function gpar (i.e., a list of graphical parameter settings)
#' @param newpage logical to indicate whether to open a new page. By default, it sets to true for opening a new page
#' @return 
#'  \item{vpnames}{an R object of "viewport" class}
#' @note none
#' @export
#' @seealso \code{\link{visHexMulComp}}, \code{\link{visCompReorder}}
#' @include visVp.r
#' @examples
#' # 1) create 5x5 viewports
#' vpnames <- visVp(colNum=5, rowNum=5)
#'
#' # 2) look at names of these viewports
#' vpnames

visVp <-function (height=7, xdim=1, ydim=1, colNum=1, rowNum=1, gp=grid::gpar(), newpage=TRUE) 
{
    
    unitWidth <- (1/colNum)
    unitHeight <- (1/rowNum)
    
    if (newpage){
    	dev.new(width=height*xdim/ydim*colNum/rowNum, height=height)
    }
    
    grid::grid.newpage()
    grid::grid.rect(gp=gp)

    vpnames <- vector()
    k=0
    for(j in rowNum:1){
        for(i in 1:colNum){
        
            if(j == rowNum){
                k <- k+1
                vpnames[k] <- paste("R", 0, "C", i, sep="")
                grid::pushViewport(grid::viewport(x=unitWidth*(i-1), y=unitHeight*(j-1), w=unitWidth, h=unitHeight, just=c("left","bottom"), name=vpnames[k]))
                grid::upViewport()
                
            }else{
                if(i == colNum){
                    
                    if(j == 1){
                        k <- k+1
                        vpnames[k] <- paste("colorbar", "R", j, sep="")
                    
                        ## y being moved unitHeight*0.1, and the unitHeight being scaled by 0.8
                        grid::pushViewport(grid::viewport(x=unitWidth*(i-1), y=unitHeight*(j-1)+unitHeight*0.1, w=unitWidth, h=unitHeight*0.8, just=c("left","bottom"), name=vpnames[k]))
                        grid::upViewport() 
                    }
                }else{
                    k <- k+1
                    vpnames[k] <- paste("R", j, "C", i, sep="")
                    grid::pushViewport(grid::viewport(x=unitWidth*(i-1), y=unitHeight*(j-1), w=unitWidth, h=unitHeight, just = c("left", "bottom"), name = vpnames[k]))
                    grid::upViewport() 
                }
                
            }
            
        }
    }
    
    invisible(vpnames)
}