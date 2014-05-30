#' Function to initialise a sInit object given a topology and input data
#'
#' \code{sInitial} is supposed to initialise an object of class "sInit" given a topology and input data. As a matter of fact, it initialises the codebook matrix (in input high-dimensional space). The return object inherits the topology information (i.e., a "sTopol" object from \code{sTopology}), along with initialised codebook matrix and method used.
#'
#' @param data a data frame or matrix of input data
#' @param sTopol an object of class "sTopol" (see \code{sTopology})
#' @param init an initialisation method. It can be one of "uniform", "sample" and "linear" initialisation methods
#' @return 
#' an object of class "sInit", a list with following components:
#' \itemize{
#'  \item{\code{nHex}: the total number of hexagons/rectanges in the grid}
#'  \item{\code{xdim}: x-dimension of the grid}
#'  \item{\code{ydim}: y-dimension of the grid}
#'  \item{\code{lattice}: the grid lattice}
#'  \item{\code{shape}: the grid shape}
#'  \item{\code{coord}: a matrix of nHex x 2, with each row corresponding to the coordinates of a hexagon/rectangle in the 2D map grid}
#'  \item{\code{init}: an initialisation method}
#'  \item{\code{codebook}: a codebook matrix of nHex x ncol(data), with each row corresponding to a prototype vector in input high-dimensional space}
#'  \item{\code{call}: the call that produced this result}
#' }
#' @note The initialisation methods include: 
#' \itemize{
#' \item{"uniform": the codebook matrix is uniformly initialised via randomly taking any values within the interval [min, max] of each column of input data}
#' \item{"sample": the codebook matrix is initialised via randomly sampling/selecting input data}
#' \item{"linear": the codebook matrix is linearly initialised along the first two greatest eigenvectors of input data}
#' }
#' @export
#' @seealso \code{\link{sTopology}}
#' @include sInitial.r
#' @examples
#' # 1) generate an iid normal random matrix of 100x10 
#' data <- matrix( rnorm(100*10,mean=0,sd=1), nrow=100, ncol=10) 
#'
#' # 2) from this input matrix, determine nHex=5*sqrt(nrow(data))=50, 
#' # but it returns nHex=61, via "sHexGrid(nHex=50)", to make sure a supra-hexagonal grid
#' sTopol <- sTopology(data=data, lattice="hexa", shape="suprahex") 
#'
#' # 3) initialise the codebook matrix using different mehtods
#' # 3a) using "uniform" method
#' sI_uniform <- sInitial(data=data, sTopol=sTopol, init="uniform") 
#' # 3b) using "sample" method
#' # sI_sample <- sInitial(data=data, sTopol=sTopol, init="sample") 
#' # 3c) using "linear" method
#' # sI_linear <- sInitial(data=data, sTopol=sTopol, init="linear") 

sInitial <- function(data, sTopol, init=c("linear","uniform","sample")) 
{
    init <- match.arg(init)
    
    if (is.vector(data)){
        data <- matrix(data, nrow=1, ncol=length(data))
    }else if(is.matrix(data) | is.data.frame(data)){
        data <- as.matrix(data)
    }else if(is.null(data)){
        stop("The input data must be not NULL.\n")
    }
    
    if (class(sTopol) != "sTopol" ){
        stop("The funciton must apply to a 'sTopol' object.\n")
    }

    dlen <- nrow(data) ## dlen is the number of rows of input data
    nHex <- sTopol$nHex
    xdim <- sTopol$xdim
    ydim <- sTopol$ydim
        
    set.seed(1234)
    if(init == "uniform"){
        codebook <- matrix(NA, nrow=nHex, ncol=ncol(data))
        ## column-wise
        tmpMin <- apply(data, 2, min)
        tmpMax <- apply(data, 2, max)
        for (i in 1:nHex) {
          codebook[i,] <- runif(ncol(data), min=tmpMin, max=tmpMax)
        }
    }else if(init == "sample"){
        ind <- sample(1:dlen, size=nHex)
        codebook <- data[ind,]
    }else if(init == "linear"){
        ## The linear initialization is achieved via:
        ## 1) calculating the eigenvalues and eigenvectors of input data;
        ## 2) xdim and ydim is initialized along the 2 greatest eigenvectors of the input data

        ##################################  
        ## calculate 2 largest eigenvalues and their corresponding eigenvectors
        data.center <- scale(data, center=T, scale=F)
        s<-svd(data.center)
        # d: a vector containing the singular values, i.e., the square roots of the non-zero eigenvalues of data %*% t(data)
        # u: a matrix whose columns contain the left singular vectors, i.e., eigenvectors of data %*% t(data)
        # v: a matrix whose columns contain the right singular vectors, i.e., eigenvectors of t(data) %*% data     
        eigval <- (s$d[1:2])^2
        V <- s$v[,1:2]
        ##################################

        ## normalize eigenvectors to unit length and multiply them by corresponding square roots of eigenvalues
        for (j in 1:2){
            V[,j] = (V[,j] / norm(V[,j],"2")) * sqrt(eigval[j])
            #V[,j] = (V[,j] / sum(abs(V[,j]))) * sqrt(eigval[j])
        }
        
        ## initialize codebook vectors
        #sT <- sTopology(xdim=xdim, ydim=ydim, lattice="rect", shape="sheet")
        #Coords <- sT$coord[,c(2,1)]
        Coords <- sTopol$coord
        for (j in 1:2){
            tmpMax <- max(Coords[,j])
            tmpMin <- min(Coords[,j])
            if(tmpMax > tmpMin){
                Coords[,j] <- (Coords[,j] - tmpMin)/(tmpMax - tmpMin)
            }else{
                Coords[,j] <- 0.5
            }
        }
        Coords <- (Coords-0.5)*2
        
        tmpMean <- matrix(apply(data, 2, mean), nrow=1, ncol=ncol(data))        
        codebook <- matrix(rep(tmpMean, nHex), nrow=nHex, ncol=length(tmpMean), byrow = T)
        for(i in 1:nHex){
            codebook[i,] <- codebook[i,] + Coords[i,] %*% t(V)
        }

    }
    
    codebook <- matrix(codebook, nrow=nHex, ncol=ncol(data))
    colnames(codebook) <- colnames(data)
    
    sInit <- list(  nHex = nHex, 
                   xdim = xdim, 
                   ydim = ydim,
                   lattice = sTopol$lattice,
                   shape = sTopol$shape,
                   coord = sTopol$coord,
                   init = init,
                   codebook = codebook,
                   call = match.call(),
                   method = "suprahex")
    
    class(sInit) <- "sInit"
    
    invisible(sInit)
}