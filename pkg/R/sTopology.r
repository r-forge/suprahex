#' Function to define the topology of a map grid
#'
#' \code{sTopology} is supposed to define the topology of a 2D map grid. The topological shape can be either a supra-hexagonal grid or a hexagonal/rectangle sheet. It returns an object of "sTopol" class, containing: the total number of hexagons/rectangles in the grid, the grid xy-dimensions, the grid lattice, the grid shape, and the 2D coordinates of all hexagons/rectangles in the grid. The 2D coordinates can be directly used to measure distances between any pair of lattice hexagons/rectangles.
#'
#' @param data a data frame or matrix of input data
#' @param xdim an integer specifying x-dimension of the grid
#' @param ydim an integer specifying y-dimension of the grid
#' @param nHex the number of hexagons/rectangles in the grid
#' @param lattice the grid lattice, either "hexa" for a hexagon or "rect" for a rectangle
#' @param shape the grid shape, either "suprahex" for a supra-hexagonal grid or "sheet" for a hexagonal/rectangle sheet. Also supported are suprahex's variants (including "triangle" for the triangle-shaped variant, "diamond" for the diamond-shaped variant, "hourglass" for the hourglass-shaped variant, "trefoil" for the trefoil-shaped variant, "ladder" for the ladder-shaped variant, and "butterfly" for the butterfly-shaped variant)
#' @return 
#' an object of class "sTopol", a list with following components:
#' \itemize{
#'  \item{\code{nHex}: the total number of hexagons/rectanges in the grid. It is not always the same as the input nHex (if any); see "Note" below for the explaination}
#'  \item{\code{xdim}: x-dimension of the grid}
#'  \item{\code{ydim}: y-dimension of the grid}
#'  \item{\code{lattice}: the grid lattice}
#'  \item{\code{shape}: the grid shape}
#'  \item{\code{coord}: a matrix of nHex x 2, with each row corresponding to the coordinates of a hexagon/rectangle in the 2D map grid}
#'  \item{\code{call}: the call that produced this result}
#' }
#' @note The output of nHex depends on the input arguments and grid shape: 
#' \itemize{
#' \item{How the input parameters are used to determine nHex is taken priority in the following order: "xdim & ydim" > "nHex" > "data"}
#' \item{If both of xdim and ydim are given, \eqn{nHex=xdim*ydim} for the "sheet" shape, \eqn{r=(min(xdim,ydim)+1)/2} for the "suprahex" shape}
#' \item{If only data is input, \eqn{nHex=5*sqrt(dlen)}, where dlen is the number of rows of the input data}
#' \item{With nHex in hand, it depends on the grid shape:}
#' \itemize{
#' \item{For "sheet" shape, xy-dimensions of sheet grid is determined according to the square root of the two biggest eigenvalues of the input data}
#' \item{For "suprahex" shape, see \code{\link{sHexGrid}} for calculating the grid radius r. The xdim (and ydim) is related to r via \eqn{xdim=2*r-1}}
#' }
#' }
#' @export
#' @seealso \code{\link{sHexGrid}}, \code{\link{visHexMapping}}
#' @include sTopology.r
#' @examples
#' # For "suprahex" shape
#' sTopol <- sTopology(xdim=3, ydim=3, lattice="hexa", shape="suprahex")
#'
#' # Error: "The suprahex shape grid only allows for hexagonal lattice" 
#' # sTopol <- sTopology(xdim=3, ydim=3, lattice="rect", shape="suprahex")
#'
#' # For "sheet" shape with hexagonal lattice
#' sTopol <- sTopology(xdim=3, ydim=3, lattice="hexa", shape="sheet")
#'
#' # For "sheet" shape with rectangle lattice
#' sTopol <- sTopology(xdim=3, ydim=3, lattice="rect", shape="sheet")
#'
#' # By default, nHex=19 (i.e., r=3; xdim=ydim=5) for "suprahex" shape
#' sTopol <- sTopology(shape="suprahex")
#'
#' # By default, xdim=ydim=5 (i.e., nHex=25) for "sheet" shape
#' sTopol <- sTopology(shape="sheet")
#'
#' # Determine the topolopy of a supra-hexagonal grid based on input data
#' # 1) generate an iid normal random matrix of 100x10 
#' data <- matrix(rnorm(100*10,mean=0,sd=1), nrow=100, ncol=10) 
#' # 2) from this input matrix, determine nHex=5*sqrt(nrow(data))=50, 
#' # but it returns nHex=61, via "sHexGrid(nHex=50)", to make sure a supra-hexagonal grid
#' sTopol <- sTopology(data=data, lattice="hexa", shape="suprahex")
#' # sTopol <- sTopology(data=data, lattice="hexa", shape="trefoil")
#'
#' # do visualisation
#' visHexMapping(sTopol,mappingType="indexes")
#'
#' \dontrun{
#' library(ggplot2)
#' # another way to do visualisation
#' df_polygon <- sHexPolygon(sTopol)
#' df_coord <- data.frame(sTopol$coord, index=1:nrow(sTopol$coord))
#' gp <- ggplot(data=df_polygon, aes(x,y,group=index)) + geom_polygon(aes(fill=factor(stepCentroid%%2))) + coord_fixed(ratio=1) + theme_void() + theme(legend.position="none") + geom_text(data=df_coord, aes(x,y,label=index), color="white")
#' }

sTopology <- function (data=NULL, xdim=NULL, ydim=NULL, nHex=NULL, lattice=c("hexa","rect"), shape=c("suprahex", "sheet", "triangle", "diamond", "hourglass", "trefoil", "ladder", "butterfly"))
{
    lattice <- match.arg(lattice)
    shape <- match.arg(shape)
    
    if (lattice == "rect" & shape != "sheet"){
        stop("The suprahex (or its variants) shape grid only allows for hexagonal lattice.\n")
    }
    
    if (is.vector(data)){
        data <- matrix(data, nrow=1, ncol=length(data))
    }else if(is.matrix(data) | is.data.frame(data)){
        data <- as.matrix(data)
    }
    
    ## if both of xdim and ydim are given, ignore given nHex 
    ## if only data is given, nHex is determined according to the number of rows in the given data
    if(is.null(xdim) | is.null(ydim)){
        if(!is.null(data) & is.null(nHex)){
            if(ncol(data) >= 2){
                ## A heuristic formula of "nHex = 5*sqrt(dlen)" is used to calculate the number of hexagons/rectangles, where dlen is the number of rows in the given data
                dlen <- nrow(data)
                nHex <- ceiling(5*sqrt(dlen))          
            }
        }
        
        if(!is.null(nHex)){

            if(shape != "sheet"){
                nHex <- nHex
            }else if(shape == "sheet"){
            
                ## xy-dimensions of sheet grid is determined according to the square root of the two biggest eigenvalues of the input data
                
                ##################################  
                ## initialize xdim/ydim ratio using principal components of the input space; the ratio is the square root of ratio of two largest eigenvalues
                
                ## calculate two largest eigenvalues and their corresponding eigenvectors
                data.center <- scale(data, center=T, scale=F)
                s <- svd(data.center)
                # d: a vector containing the singular values, i.e., the square roots of the non-zero eigenvalues of data %*% t(data)
                # u: a matrix whose columns contain the left singular vectors, i.e., eigenvectors of data %*% t(data)
                # v: a matrix whose columns contain the right singular vectors, i.e., eigenvectors of t(data) %*% data     
                ratio <- s$d[1]/s$d[2] # ratio between xdim and ydim
                ################################## 
                
                if(lattice == "hexa"){
                    ## in hexagonal lattice, the y-dimension is not directly proportional to the number of hexagons but being squeezed together by a factor of sqrt(0.75)
                    xdim <- min(nHex, round(sqrt(nHex/ratio * sqrt(0.75))))
                    ydim <- min(nHex, round(nHex/xdim))
                    nHex <- xdim*ydim
                }else if(lattice == "rect"){
                    xdim <- min(nHex, round(sqrt(nHex/ratio)))
                    ydim <- min(nHex, round(nHex/xdim))
                    nHex <- xdim*ydim
                }
            }
        }
    }
    
    if(shape == "sheet"){
        if(is.null(xdim)){
            xdim <- 5
        }
        if(xdim <= 1){
            xdim <- 2
        }
    
        if(is.null(ydim)){
            ydim <- 5
        }
        if(ydim <= 1){
            ydim <- 2
        }
    
        nHex <- xdim*ydim
        
        ## Calculates the coordinates
        if(lattice == "rect"){
            ## For rectangle lattice, the x-coordinates and the y-coordinates are 1:xdim and 1:ydim, respectively
            x <- 1L:xdim
            y <- 1L:ydim
            coord <- as.matrix(expand.grid(x = x, y = y))
        }else if(lattice == "hexa"){
            ## For hexagonal lattice
            ## initially, the x-coordinates and the y-coordinates are 1:xdim and 1:ydim, respectively
            x <- 1L:xdim
            y <- 1L:ydim
            coord <- as.matrix(expand.grid(x = x, y = y))
            ## to make sure the equal distance to all direct neighbors, 1) the x-coordinates of odd row are shifted by 0.5, 2) the y-coordinates are multiplied by sqrt(0.75)
            coord[, 1L] <- coord[, 1L] + 0.5 * (coord[, 2L] %% 2)
            coord[, 2L] <- sqrt(0.75) * coord[, 2L]        
        }
    
    }else{
        
        if(shape=="suprahex" | shape=="trefoil" | shape=="butterfly"){
			r <- NULL
			if(!is.null(xdim) & !is.null(ydim)){
				r <- ceiling((min(xdim, ydim)+1)/2)
			}else if(is.null(xdim) & !is.null(ydim)){
				r <- ceiling((ydim+1)/2)
			}else if(is.null(ydim) & !is.null(xdim)){
				r <- ceiling((xdim+1)/2)
			}
        
        }else if(shape=="diamond" | shape=="hourglass"){
			r <- NULL
			if(!is.null(xdim) & !is.null(ydim)){
				r <- ceiling((min(xdim*2-1, ydim)+1)/2)
			}else if(is.null(xdim) & !is.null(ydim)){
				r <- ceiling((ydim+1)/2)
			}else if(is.null(ydim) & !is.null(xdim)){
				r <- ceiling(xdim)
			}
			
        }else if(shape=="ladder"){
			r <- NULL
			if(!is.null(xdim) & !is.null(ydim)){
				r <- ceiling((min(xdim, ydim*2-1)+1)/2)
			}else if(is.null(xdim) & !is.null(ydim)){
				r <- ceiling(ydim)
			}else if(is.null(ydim) & !is.null(xdim)){
				r <- ceiling((xdim+1)/2)
			}
			
        }else if(shape=="triangle"){
			r <- NULL
			if(!is.null(xdim) & !is.null(ydim)){
				r <- ceiling(min(xdim, ydim))
			}else if(is.null(xdim) & !is.null(ydim)){
				r <- ceiling(ydim)
			}else if(is.null(ydim) & !is.null(xdim)){
				r <- ceiling(xdim)
			}
			
        }
        
        sHex <- sHexGridVariant(r=r, nHex=nHex, shape=shape)
        nHex <- sHex$nHex
        coord <- sHex$coord

        ## relations between xdim (or ydim) and r
        r <- sHex$r
        if(shape=="suprahex" | shape=="trefoil" | shape=="butterfly"){
			xdim <- ydim <- 2*r-1
        }else if(shape=="diamond" | shape=="hourglass"){
			xdim <- r
			ydim <- 2*r-1
        }else if(shape=="ladder"){
			xdim <- 2*r-1
			ydim <- r
        }else if(shape=="triangle"){
			xdim <- ydim <- r
        }
        
    }

    sTopol <- list(nHex = nHex, 
                   xdim = xdim, 
                   ydim = ydim,
                   lattice = lattice,
                   shape = shape,
                   coord = coord,
                   call = match.call(),
                   method = "suprahex")
    
    class(sTopol) <- "sTopol"
    
    invisible(sTopol)

}