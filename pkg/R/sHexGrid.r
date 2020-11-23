#' Function to define a supra-hexagonal grid
#'
#' \code{sHexGrid} is supposed to define a supra-hexagonal map grid. A supra-hexagon is a giant hexagon, which seamlessly consists of smaller hexagons. Due to the symmetric nature, it can be uniquely determined by specifying the radius away from the grid centroid. This function takes input the grid radius (or the number of hexagons in the grid, but will be adjusted to meet the definition of supra-hexagon), and returns a list (see 'Value' below) containing: the grid radius, the total number of hexagons in the grid, the 2D coordinates of the grid centroid, the step for each hexogan away from the grid centroid, and the 2D coordinates of all hexagons in the grid.
#'
#' @param r an integer specifying the radius in a supra-hexagonal grid
#' @param nHex the number of input hexagons in the grid
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
#' @note The relationships among return values: 
#' \itemize{
#' \item{\eqn{nHex = 1+6*r*(r-1)/2}}
#' \item{\eqn{centroid = coord[1,]}}
#' \item{\eqn{stepCentroid[1] = 1}}
#' \item{\eqn{stepCentroid[2:nHex] = unlist(sapply(2:r, function(x) (c( (1+6*x*(x-1)/2-6*(x-1)+1) : (1+6*x*(x-1)/2) )>=1)*x ))}}
#' }
#' @export
#' @seealso \code{\link{sPipeline}}
#' @include sHexGrid.r
#' @examples
#' # The supra-hexagonal grid is exactly determined by specifying the radius.
#' sHex <- sHexGrid(r=2)
#'
#' # The grid is determined according to the number of input hexagons (after being adjusted).
#' # The return res$nHex is always no less than the input one.
#' # It ensures a supra-hexagonal grid is exactly formed.
#' sHex <- sHexGrid(nHex=12)
#'
#' # Ignore input nHex if r is also given
#' sHex <- sHexGrid(r=3, nHex=100)
#'
#' # By default, r=3 if no parameters are specified
#' sHex <- sHexGrid()

sHexGrid <- function(r=NULL, nHex=NULL)
{
    ## ignore nHex if r is given
    if(is.null(r) & !is.null(nHex)){
        ## determine r according to given nHex
        if(nHex == 1){
            r <- 1
        }else if(nHex > 1){
            flag <- 1
            r <- 1
            tol <- 1
            while(flag == 1){
                tol <- tol + 6*r
                if(tol >= nHex){
                    flag <- 0
                }
                r <- r + 1
            }
        }
    }
    
    if(is.null(r)){
        ## r=3 by default
        r <- 3 
        warning("Ignore the input parameters but use the default radius.\n")
    }
    
    if(r == 1){
        tol <- c(1)
    }else{
        tol <- c(1,seq(6*(2-1),6*(r-1),by=6))
    }
    
    coord <- matrix(0,nrow=sum(tol),ncol=2)
    colnames(coord) <- c("x","y")
    
    ## for centroid
    centroid <- c(r, r*sqrt(0.75))
    
    ## for the step away from centroid
    step_centroid <- matrix(0, nrow=nrow(coord), ncol=1)
    
    ## for middle and upper part
    k <- 0
    t <- 1
    lft <- 0
    rgt <- 0
    for(i in r:(2*r-1)){
    
        if(i != r){
            if(k %% 2){
                rgt <- rgt+1
            }else{
                lft <- lft+1
            }
        }
    
        tmp_x <- (lft+1):(2*r-1-rgt)
        if(k %% 2){
            tmp_x <- tmp_x+0.5
        }
    
        ## for the step away from centroid
        if((r-1) < (k+1)){
            step_centroid[t:(t+length(tmp_x)-1)] <- c(rep(k,(k+1)))
        }else{
            step_centroid[t:(t+length(tmp_x)-1)] <- c((r-1):(k+1), rep(k,(k+1)), (k+1):(r-1))
        }
        
        for(j in 1:length(tmp_x)){
            coord[t,1] <- tmp_x[j]
            coord[t,2] <- i*sqrt(0.75)
            
            t <- t+1
        }
        
        k <- k+1
    }
    
    ## for lower part
    k <- 1
    lft <- 0
    rgt <- 0
    for(i in (r-1):1){

        if(i != r){
            if(k %% 2){
                rgt <- rgt+1
            }else{
                lft <- lft+1
            }
        }
    
        tmp_x <- (lft+1):(2*r-1-rgt)
        if(k %% 2){
            tmp_x <- tmp_x+0.5
        }
        
    
        # for the step away from centroid
        if((r-1) < (k+1)){
            step_centroid[t:(t+length(tmp_x)-1)] <- c(rep(k,(k+1)))
        }else{
            step_centroid[t:(t+length(tmp_x)-1)] <- c((r-1):(k+1), rep(k,(k+1)), (k+1):(r-1))
        }
        
    
        for(j in 1:length(tmp_x)){
            coord[t,1] <- tmp_x[j]
            coord[t,2] <- i*sqrt(0.75)
            t <- t+1
        }
    
        k <- k+1
    }
    
    ## Determine the order according to the angle with the x-axis
    ## Always:
    ## 1) step from 1 to r
    ## 2) angle in an anti-clock wise
    angle_centroid <- matrix(0, nrow=length(step_centroid), ncol=1)
    for (i in 1:length(angle_centroid)){
        a <- c(1,0)
        b <- coord[i,]-coord[r,]
        angle_centroid[i] <- acos( sum(a*b) / ( sqrt(sum(a * a)) * sqrt(sum(b * b)) ) )
    
        ## convert into [0, 2*pi]
        if(b[2] <0){
            angle_centroid[i] <- 2*pi - angle_centroid[i]
        }
    
    }
    angle_centroid[is.na(angle_centroid)] <- 0
    
    step_angle <- cbind(step_centroid, angle_centroid)
    order_inds <- order(step_angle[,1],step_angle[,2])
    
    coord <- coord[order_inds,]
    stepCentroid  <- step_angle[order_inds, 1]
    angleCentroid  <- step_angle[order_inds, 2]
    
    sHex <- list(r = r,
                nHex = sum(tol),
                centroid = centroid, 
                coord = coord,
                stepCentroid = stepCentroid,
                angleCentroid = angleCentroid,
                call = match.call(),
                method = "suprahex")
                
    class(sHex) <- "sHex"
    
    sHex
    
}
