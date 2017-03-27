#' Function to compute the pairwise distance for a given data matrix
#'
#' \code{sDistance} is supposed to compute and return the distance matrix between the rows of a data matrix using a specified distance metric
#'
#' @param data a data frame or matrix of input data
#' @param metric distance metric used to calculate a symmetric distance matrix. See 'Note' below for options available
#' @return 
#' \itemize{
#'  \item{\code{dist}: a symmetric distance matrix of nRow x nRow, where nRow is the number of rows of input data matrix}
#' }
#' @note The distance metrics are supported:
#' \itemize{
#' \item{"pearson": Pearson correlation. Note that two curves that have identical shape, but different magnitude will still have a correlation of 1}
#' \item{"spearman": Spearman rank correlation. As a nonparametric version of the pearson correlation, it calculates the correlation between the ranks of the data values in the two vectors (more robust against outliers)}
#' \item{"kendall": Kendall tau rank correlation. Compared to spearman rank correlation, it goes a step further by using only the relative ordering to calculate the correlation. For all pairs of data points \eqn{(x_i, y_i)} and \eqn{(x_j, y_j)}, it calls a pair of points either as concordant (\eqn{Nc} in total) if \eqn{(x_i - x_j)*(y_i - y_j)>0}, or as discordant (\eqn{Nd} in total) if \eqn{(x_i - x_j)*(y_i - y_j)<0}. Finally, it calculates gamma coefficient \eqn{(Nc-Nd)/(Nc+Nd)} as a measure of association which is highly resistant to tied data}
#' \item{"euclidean": Euclidean distance. Unlike the correlation-based distance measures, it takes the magnitude into account (input data should be suitably normalized}
#' \item{"manhattan": Cityblock distance. The distance between two vectors is the sum of absolute value of their differences along any coordinate dimension}
#' \item{"cos": Cosine similarity. As an uncentered version of pearson correlation, it is a measure of similarity between two vectors of an inner product space, i.e., measuring the cosine of the angle between them (using a dot product and magnitude)}
#' \item{"mi": Mutual information (MI). \eqn{MI} provides a general measure of dependencies between variables, in particular, positive, negative and nonlinear correlations. The caclulation of \eqn{MI} is implemented via applying adaptive partitioning method for deriving equal-probability bins (i.e., each bin contains approximately the same number of data points). The number of bins is heuristically determined (the lower bound): \eqn{1+log2(n)}, where n is the length of the vector. Because \eqn{MI} increases with entropy, we normalize it to allow comparison of different pairwise clone similarities: \eqn{2*MI/[H(x)+H(y)]}, where \eqn{H(x)} and \eqn{H(y)} stand for the entropy for the vector \eqn{x} and \eqn{y}, respectively}
#' }
#' @export
#' @seealso \code{\link{sDmatCluster}}
#' @include sDistance.r
#' @examples
#' # 1) generate an iid normal random matrix of 100x10 
#' data <- matrix( rnorm(100*10,mean=0,sd=1), nrow=100, ncol=10) 
#'
#' # 2) calculate distance matrix using different metric
#' sMap <- sPipeline(data=data)
#' # 2a) using "pearson" metric
#' dist <- sDistance(data=data, metric="pearson")
#' # 2b) using "cos" metric
#' # dist <- sDistance(data=data, metric="cos")
#' # 2c) using "spearman" metric
#' # dist <- sDistance(data=data, metric="spearman")
#' # 2d) using "kendall" metric
#' # dist <- sDistance(data=data, metric="kendall")
#' # 2e) using "euclidean" metric
#' # dist <- sDistance(data=data, metric="euclidean")
#' # 2f) using "manhattan" metric
#' # dist <- sDistance(data=data, metric="manhattan")
#' # 2g) using "mi" metric
#' # dist <- sDistance(data=data, metric="mi")

sDistance <- function(data, metric=c("pearson","spearman","kendall","euclidean","manhattan","cos","mi"))
{   
    metric <- match.arg(metric)
    
    if (is.vector(data) | is.null(data)){
        stop("The input data must be matrix in the strictest sense.\n")
    }else if(is.data.frame(data)){
        data <- as.matrix(data)
    }
    
    if(metric == "euclidean" | metric == "manhattan"){
        res <- dist(x=data, method=metric, diag=FALSE, upper=TRUE)
    }else if(metric == "pearson" | metric == "kendall" | metric == "spearman"){
        res <- cor(x=t(data), method=metric) ## column-wise
        res <- 1-res
    }else if(metric == "cos"){
        n <- nrow(data)
        cItem <- sqrt(apply(data^2, 1, sum)) ## constant item
        res <- matrix(0, nrow=n, ncol=n)
        for(i in 1:n){
            res[i,] <- (data[i,] %*% t(data))/(cItem[i]*cItem)
        }
        res <- 1-res
        diag(res) <- 0
    }else if(metric == "mi"){
        
        ## Applying adaptive partitioning method for deriving equal-probability bins (i.e., each bin contains approximately the same number of data points)
        
        ## the number of bins is based on the heuristic: 1+log2(nc) for the lower bound on the number of bins
        nc <- ncol(data)
        bins <- ceiling(log2(nc) + 1)
        
        ## vector-specific stats
        nr <- nrow(data)
        mat_state <- matrix(0, nrow=nr, ncol=nc)
        mat_entropy <- matrix(0, nrow=nr, ncol=1)
        for(i in 1:nr){
            
            ## rank transformation
            x_rank <- rank(data[i,], ties.method="average")
            
            # rescale into [1 bins]
            rescale <- (x_rank -min(x_rank))/(max(x_rank)-min(x_rank))
            x_state <- ceiling(bins * rescale)
            x_state[x_state == 0] <- 1 # force state=0 into state=1
            mat_state[i,] <- x_state
            
            ## count for each state
            x_state_count <- sapply(1:bins, function(x) sum(x_state==x))
            
            ## entropy
            prob <- x_state_count/(sum(x_state_count))
            prob <- prob[prob != 0] ## make sure log(0)=0
            mat_entropy[i] <- sum(-1*prob*log(prob))
            
        }
        
        mat_MI <- matrix(0, nrow=nr, ncol=nr)
        joint_state <- expand.grid(1:bins, 1:bins)
        n_joint <- nrow(joint_state)
        for(i in 1:(nr-1)){
            x_state <- mat_state[i,]
            for(j in (i+1):nr){
                y_state <- mat_state[j,]
                
                ########################
                joint_state_count <- matrix(0, nrow=n_joint, ncol=1)
                for(k in 1:nrow(joint_state)){
                    joint_state_count[k,] <- sum(x_state==joint_state[k,1] & y_state==joint_state[k,2])
                }
                
                ## sum(Kij/N*log(Kij/N))+2log(bins)
                prob=joint_state_count/(sum(joint_state_count))
                prob <- prob[prob!=0] # make sure log(0)=0
                MI <- sum(prob*log(prob)) + log(n_joint)
                
                ## Because mutual information increases with entropy, we normalize it in a suitable way to allow comparison of different pairwise similarities: 2*MI/(entropy_x+entropy_y)
                mat_MI[i,j] <- 2*MI/(mat_entropy[i] + mat_entropy[j])
                ########################
            }
        }
        
        res <- mat_MI + t(mat_MI)
        res <- 1-res
        diag(res) <- 0
    }
    
    dist<- as.matrix(res)
    
    if(!is.null(rownames(data))){
        rownames(dist) <- rownames(data)
        colnames(dist) <- rownames(data)
    }
    
    invisible(dist)
}