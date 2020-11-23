#' Function to define a variant of a supra-hexagonal grid
#'
#' \code{sHexGridVariant} is supposed to define a variant of a supra-hexagonal map grid. In essence, it is the subset of the supra-hexagon.
#'
#' @param r an integer specifying the radius in a supra-hexagonal grid
#' @param nHex the number of input hexagons in the grid 
#' @param shape the grid shape, either "suprahex" for the suprahex itself, or its variants (including "triangle" for the triangle-shaped variant, "diamond" for the diamond-shaped variant, "hourglass" for the hourglass-shaped variant, "trefoil" for the trefoil-shaped variant, "ladder" for the ladder-shaped variant, "butterfly" for the butterfly-shaped variant, "ring" for the ring-shaped variant, and "bridge" for the bridge-shaped variant)
#' @return 
#' an object of class "sHex", a list with following components:
#' \itemize{
#'  \item{\code{r}: the grid radius}
#'  \item{\code{nHex}: the total number of hexagons in the grid. It may differ from the input value; actually it is always no less than the input one to ensure a supra-hexagonal grid exactly formed}
#'  \item{\code{centroid}: the 2D coordinates of the grid centroid}
#'  \item{\code{stepCentroid}: a vector with the length of nHex. It stores how many steps a hexagon is awawy from the grid centroid ('1' for the centroid itself). Starting with the centroid, it orders outward. Also, for those hexagons of the same step, it orders from the rightmost in an anti-clock wise}
#'  \item{\code{angleCentroid}: a vector with the length of nHex. It stores the angle a hexagon is in terms of  the grid centroid ('0' for the centroid itself). For those hexagons of the same step, it orders from the rightmost in an anti-clock wise}
#'  \item{\code{coord}: a matrix of nHex x 2 with each row specifying the 2D coordinates of a hexagon in the grid. The order of rows is the same as 'centroid' above}
#'  \item{\code{call}: the call that produced this result}
#' }
#' @note none
#' @export
#' @seealso \code{\link{sHexGrid}}
#' @include sHexGridVariant.r
#' @examples
#' # For "supraHex" shape itself
#' sHex <- sHexGridVariant(r=6, shape="suprahex")
#' 
#' \dontrun{
#' library(ggplot2)
#' 
#' #geom_polygon(color="black", fill=NA)
#'
#' # For "supraHex" shape itself
#' sHex <- sHexGridVariant(r=6, shape="suprahex")
#' df_polygon <- sHexPolygon(sHex)
#' df_coord <- data.frame(sHex$coord, index=1:nrow(sHex$coord))
#' gp_suprahex <- ggplot(data=df_polygon, aes(x,y,group=index)) + geom_polygon(aes(fill=factor(stepCentroid%%2))) + coord_fixed(ratio=1) + theme_void() + theme(legend.position="none") + geom_text(data=df_coord, aes(x,y,label=index), color="white", size=3) + labs(title="suprahex (r=6; xdim=ydim=11)") + theme(plot.title=element_text(hjust=0.5,size=8))
#' 
#' # For "triangle" shape
#' sHex <- sHexGridVariant(r=6, shape="triangle")
#' df_polygon <- sHexPolygon(sHex)
#' df_coord <- data.frame(sHex$coord, index=1:nrow(sHex$coord))
#' gp_triangle <- ggplot(data=df_polygon, aes(x,y,group=index)) + geom_polygon(aes(fill=factor(stepCentroid%%2))) + coord_fixed(ratio=1) + theme_void() + theme(legend.position="none") + geom_text(data=df_coord, aes(x,y,label=index), color="white", size=3) + labs(title="triangle (r=6; xdim=ydim=6)") + theme(plot.title=element_text(hjust=0.5,size=8))
#' 
#' # For "diamond" shape
#' sHex <- sHexGridVariant(r=6, shape="diamond")
#' df_polygon <- sHexPolygon(sHex)
#' df_coord <- data.frame(sHex$coord, index=1:nrow(sHex$coord))
#' gp_diamond <- ggplot(data=df_polygon, aes(x,y,group=index)) + geom_polygon(aes(fill=factor(stepCentroid%%2))) + coord_fixed(ratio=1) + theme_void() + theme(legend.position="none") + geom_text(data=df_coord, aes(x,y,label=index), color="white", size=3) + labs(title="diamond (r=6; xdim=6, ydim=11)") + theme(plot.title=element_text(hjust=0.5,size=8))
#' 
#' # For "hourglass" shape
#' sHex <- sHexGridVariant(r=6, shape="hourglass")
#' df_polygon <- sHexPolygon(sHex)
#' df_coord <- data.frame(sHex$coord, index=1:nrow(sHex$coord))
#' gp_hourglass <- ggplot(data=df_polygon, aes(x,y,group=index)) + geom_polygon(aes(fill=factor(stepCentroid%%2))) + coord_fixed(ratio=1) + theme_void() + theme(legend.position="none") + geom_text(data=df_coord, aes(x,y,label=index), color="white", size=3) + labs(title="hourglass (r=6; xdim=6, ydim=11)") + theme(plot.title=element_text(hjust=0.5,size=8))
#' 
#' # For "trefoil" shape
#' sHex <- sHexGridVariant(r=6, shape="trefoil")
#' df_polygon <- sHexPolygon(sHex)
#' df_coord <- data.frame(sHex$coord, index=1:nrow(sHex$coord))
#' gp_trefoil <- ggplot(data=df_polygon, aes(x,y,group=index)) + geom_polygon(aes(fill=factor(stepCentroid%%2))) + coord_fixed(ratio=1) + theme_void() + theme(legend.position="none") + geom_text(data=df_coord, aes(x,y,label=index), color="white", size=3) + labs(title="trefoil (r=6; xdim=ydim=11)") + theme(plot.title=element_text(hjust=0.5,size=8))
#' 
#' # For "ladder" shape
#' sHex <- sHexGridVariant(r=6, shape="ladder")
#' df_polygon <- sHexPolygon(sHex)
#' df_coord <- data.frame(sHex$coord, index=1:nrow(sHex$coord))
#' gp_ladder <- ggplot(data=df_polygon, aes(x,y,group=index)) + geom_polygon(aes(fill=factor(stepCentroid%%2))) + coord_fixed(ratio=1) + theme_void() + theme(legend.position="none") + geom_text(data=df_coord, aes(x,y,label=index), color="white", size=3) + labs(title="ladder (r=6; xdim=11, ydim=6)") + theme(plot.title=element_text(hjust=0.5,size=8))
#' 
#' # For "butterfly" shape
#' sHex <- sHexGridVariant(r=6, shape="butterfly")
#' df_polygon <- sHexPolygon(sHex)
#' df_coord <- data.frame(sHex$coord, index=1:nrow(sHex$coord))
#' gp_butterfly <- ggplot(data=df_polygon, aes(x,y,group=index)) + geom_polygon(aes(fill=factor(stepCentroid%%2))) + coord_fixed(ratio=1) + theme_void() + theme(legend.position="none") + geom_text(data=df_coord, aes(x,y,label=index), color="white", size=3) + labs(title="butterfly (r=6; xdim=ydim=11)") + theme(plot.title=element_text(hjust=0.5,size=8))
#' 
#' # For "ring" shape
#' sHex <- sHexGridVariant(r=6, shape="ring")
#' df_polygon <- sHexPolygon(sHex)
#' df_coord <- data.frame(sHex$coord, index=1:nrow(sHex$coord))
#' gp_ring <- ggplot(data=df_polygon, aes(x,y,group=index)) + geom_polygon(aes(fill=factor(stepCentroid%%2))) + coord_fixed(ratio=1) + theme_void() + theme(legend.position="none") + geom_text(data=df_coord, aes(x,y,label=index), color="white", size=3) + labs(title="ring (r=6; xdim=ydim=11)") + theme(plot.title=element_text(hjust=0.5,size=8))
#' 
#' # For "bridge" shape
#' sHex <- sHexGridVariant(r=6, shape="bridge")
#' df_polygon <- sHexPolygon(sHex)
#' df_coord <- data.frame(sHex$coord, index=1:nrow(sHex$coord))
#' gp_bridge <- ggplot(data=df_polygon, aes(x,y,group=index)) + geom_polygon(aes(fill=factor(stepCentroid%%2))) + coord_fixed(ratio=1) + theme_void() + theme(legend.position="none") + geom_text(data=df_coord, aes(x,y,label=index), color="white", size=3) + labs(title="bridge (r=6; xdim=11, ydim=6)") + theme(plot.title=element_text(hjust=0.5,size=8))
#' 
#' # combined visuals
#' library(gridExtra)
#' grid.arrange(grobs=list(gp_suprahex, gp_ring, gp_diamond, gp_trefoil, gp_butterfly, gp_hourglass, gp_ladder, gp_bridge, gp_triangle), layout_matrix=rbind(c(1,1,2,2,3),c(1,1,2,2,3),c(4,4,5,5,6),c(4,4,5,5,6),c(7,7,8,8,9)), nrow=5, ncol=5)
#' }

sHexGridVariant <- function (r=NULL, nHex=NULL, shape=c("suprahex", "triangle", "diamond", "hourglass", "trefoil", "ladder", "butterfly", "ring", "bridge"))
{
    shape <- match.arg(shape)
    
    if(shape == "suprahex"){
    	sHex <- sHexGrid(r=r, nHex=nHex)
    	
    }else{
	
		if(is.null(r) & is.null(nHex)){
			## r=3 by default
			r <- 3 
			warning("Ignore the input parameters but use the default radius.\n")
		}
	
		if(shape == "triangle"){
			## ignore nHex if r is given
			if(is.null(r) & !is.null(nHex)){
				sHex <- sHexGrid(nHex=nHex)
				angles <- round(360*sHex$angleCentroid/(2*pi))
				ind <- union(1, which((angles>=120 & angles<=180)))
				r_running <- sHex$r
				while(length(ind) < nHex){
					r_running <- r_running + 1
					sHex <- sHexGrid(r=r_running)
					angles <- round(360*sHex$angleCentroid/(2*pi))
					ind <- union(1, which((angles>=120 & angles<=180)))
				}
				r <- r_running
			}
		
			#########################################
			sHex <- sHexGrid(r=r)
			angles <- round(360*sHex$angleCentroid/(2*pi))
			ind <- union(1, which((angles>=120 & angles<=180)))
			## extract the subset
			nHex <- length(ind)
			coord <- sHex$coord[ind, ]
			centroid <- sHex$centroid
			stepCentroid <- sHex$stepCentroid[ind]
			angleCentroid <- sHex$angleCentroid[ind]
			## adjust y
			y_offset <- (r-1)*sqrt(0.75)
			coord[,2] <- coord[,2] - y_offset
			centroid[2] <- centroid[2] - y_offset
		
		}else if(shape == "diamond"){
			## ignore nHex if r is given
			if(is.null(r) & !is.null(nHex)){
				sHex <- sHexGrid(nHex=nHex)
				angles <- round(360*sHex$angleCentroid/(2*pi))
				ind <- union(1, which((angles>=120 & angles<=240)))
				r_running <- sHex$r
				while(length(ind) < nHex){
					r_running <- r_running + 1
					sHex <- sHexGrid(r=r_running)
					angles <- round(360*sHex$angleCentroid/(2*pi))
					ind <- union(1, which((angles>=120 & angles<=240)))
				}
				r <- r_running
			}
		
			#########################################
			sHex <- sHexGrid(r=r)
			angles <- round(360*sHex$angleCentroid/(2*pi))
			ind <- union(1, which((angles>=120 & angles<=240)))
			## extract the subset
			nHex <- length(ind)
			coord <- sHex$coord[ind, ]
			centroid <- sHex$centroid
			stepCentroid <- sHex$stepCentroid[ind]
			angleCentroid <- sHex$angleCentroid[ind]
		
		}else if(shape == "hourglass"){
			## ignore nHex if r is given
			if(is.null(r) & !is.null(nHex)){
				sHex <- sHexGrid(nHex=nHex)
				angles <- round(360*sHex$angleCentroid/(2*pi))
				ind <- union(1, which((angles>=60 & angles<=120) | (angles>=240 & angles<=300)))
				r_running <- sHex$r
				while(length(ind) < nHex){
					r_running <- r_running + 1
					sHex <- sHexGrid(r=r_running)
					angles <- round(360*sHex$angleCentroid/(2*pi))
					ind <- union(1, which((angles>=60 & angles<=120) | (angles>=240 & angles<=300)))
				}
				r <- r_running
			}
		
			#########################################
			sHex <- sHexGrid(r=r)
			angles <- round(360*sHex$angleCentroid/(2*pi))
			ind <- union(1, which((angles>=60 & angles<=120) | (angles>=240 & angles<=300)))
			## extract the subset
			nHex <- length(ind)
			coord <- sHex$coord[ind, ]
			centroid <- sHex$centroid
			stepCentroid <- sHex$stepCentroid[ind]
			angleCentroid <- sHex$angleCentroid[ind]
			## adjust x
			x_offset <- min(coord[,1]) - 1
			coord[,1] <- coord[,1] - x_offset
			centroid[1] <- centroid[1] - x_offset
			
		}else if(shape == "trefoil"){
			## ignore nHex if r is given
			if(is.null(r) & !is.null(nHex)){
				sHex <- sHexGrid(nHex=nHex)
				angles <- round(360*sHex$angleCentroid/(2*pi))
				ind <- union(1, which((angles>=0 & angles<=60) | (angles>=120 & angles<=180) | (angles>=240 & angles<=300)))
				r_running <- sHex$r
				while(length(ind) < nHex){
					r_running <- r_running + 1
					sHex <- sHexGrid(r=r_running)
					angles <- round(360*sHex$angleCentroid/(2*pi))
					ind <- union(1, which((angles>=0 & angles<=60) | (angles>=120 & angles<=180) | (angles>=240 & angles<=300)))
				}
				r <- r_running
			}
		
			#########################################
			sHex <- sHexGrid(r=r)
			angles <- round(360*sHex$angleCentroid/(2*pi))
			ind <- union(1, which((angles>=0 & angles<=60) | (angles>=120 & angles<=180) | (angles>=240 & angles<=300)))
			## extract the subset
			nHex <- length(ind)
			coord <- sHex$coord[ind, ]
			centroid <- sHex$centroid
			stepCentroid <- sHex$stepCentroid[ind]
			angleCentroid <- sHex$angleCentroid[ind]
		
		}else if(shape == "ladder"){
			## ignore nHex if r is given
			if(is.null(r) & !is.null(nHex)){
				sHex <- sHexGrid(nHex=nHex)
				angles <- round(360*sHex$angleCentroid/(2*pi))
				ind <- union(1, which((angles>=0 & angles<=180)))
				r_running <- sHex$r
				while(length(ind) < nHex){
					r_running <- r_running + 1
					sHex <- sHexGrid(r=r_running)
					angles <- round(360*sHex$angleCentroid/(2*pi))
					ind <- union(1, which((angles>=0 & angles<=180)))
				}
				r <- r_running
			}
		
			#########################################
			sHex <- sHexGrid(r=r)
			angles <- round(360*sHex$angleCentroid/(2*pi))
			ind <- union(1, which((angles>=0 & angles<=180)))
			## extract the subset
			nHex <- length(ind)
			coord <- sHex$coord[ind, ]
			centroid <- sHex$centroid
			stepCentroid <- sHex$stepCentroid[ind]
			angleCentroid <- sHex$angleCentroid[ind]
			## adjust y
			y_offset <- (r-1)*sqrt(0.75)
			coord[,2] <- coord[,2] - y_offset
			centroid[2] <- centroid[2] - y_offset
		
		}else if(shape == "butterfly"){
			## ignore nHex if r is given
			if(is.null(r) & !is.null(nHex)){
				sHex <- sHexGrid(nHex=nHex)
				angles <- round(360*sHex$angleCentroid/(2*pi))
				ind <- union(1, which((angles>=0 & angles<=60) | (angles>=120 & angles<=240) | (angles>=300 & angles<=360)))
				r_running <- sHex$r
				while(length(ind) < nHex){
					r_running <- r_running + 1
					sHex <- sHexGrid(r=r_running)
					angles <- round(360*sHex$angleCentroid/(2*pi))
					ind <- union(1, which((angles>=0 & angles<=60) | (angles>=120 & angles<=240) | (angles>=300 & angles<=360)))
				}
				r <- r_running
			}
		
			#########################################
			sHex <- sHexGrid(r=r)
			angles <- round(360*sHex$angleCentroid/(2*pi))
			ind <- union(1, which((angles>=0 & angles<=60) | (angles>=120 & angles<=240) | (angles>=300 & angles<=360)))
			## extract the subset
			nHex <- length(ind)
			coord <- sHex$coord[ind, ]
			centroid <- sHex$centroid
			stepCentroid <- sHex$stepCentroid[ind]
			angleCentroid <- sHex$angleCentroid[ind]
		
		}else if(shape == "ring"){
			## ignore nHex if r is given
			if(is.null(r) & !is.null(nHex)){
				sHex <- sHexGrid(nHex=nHex)
				r_running <- sHex$r
				steps <- sHex$stepCentroid
				ind <- which(steps+1 > ceiling(r_running/2))
				while(length(ind) < nHex){
					r_running <- r_running + 1
					sHex <- sHexGrid(r=r_running)
					steps <- sHex$stepCentroid
					ind <- which(steps+1 > ceiling(r_running/2))
				}
				r <- r_running
			}
		
			#########################################
			sHex <- sHexGrid(r=r)
			steps <- sHex$stepCentroid
			ind <- which(steps+1 > ceiling(r/2))
			## extract the subset
			nHex <- length(ind)
			coord <- sHex$coord[ind, ]
			centroid <- sHex$centroid
			stepCentroid <- sHex$stepCentroid[ind]
			angleCentroid <- sHex$angleCentroid[ind]
		
		}else if(shape == "bridge"){
			## ignore nHex if r is given
			if(is.null(r) & !is.null(nHex)){
				sHex <- sHexGrid(nHex=nHex)
				r_running <- sHex$r
				steps <- sHex$stepCentroid
				angles <- round(360*sHex$angleCentroid/(2*pi))
				ind <- intersect(which(steps+1 > ceiling(r_running/2)), which((angles>=0 & angles<=180)))
				while(length(ind) < nHex){
					r_running <- r_running + 1
					sHex <- sHexGrid(r=r_running)
					steps <- sHex$stepCentroid
					angles <- round(360*sHex$angleCentroid/(2*pi))
					ind <- intersect(which(steps+1 > ceiling(r_running/2)), which((angles>=0 & angles<=180)))
				}
				r <- r_running
			}
		
			#########################################
			sHex <- sHexGrid(r=r)
			steps <- sHex$stepCentroid
			angles <- round(360*sHex$angleCentroid/(2*pi))
			ind <- intersect(which(steps+1 > ceiling(r/2)), which((angles>=0 & angles<=180)))
			## extract the subset
			nHex <- length(ind)
			coord <- sHex$coord[ind, ]
			centroid <- sHex$centroid
			stepCentroid <- sHex$stepCentroid[ind]
			angleCentroid <- sHex$angleCentroid[ind]
		
		}

		sHex <- list(r = r,
					nHex = nHex,
					centroid = centroid, 
					coord = coord,
					stepCentroid = stepCentroid,
					angleCentroid = angleCentroid,
					call = match.call(),
					method = "suprahex")
				
		class(sHex) <- "sHex"
		
    }
    
    sHex

}