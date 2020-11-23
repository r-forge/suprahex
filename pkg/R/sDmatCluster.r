#' Function to partition a grid map into clusters
#'
#' \code{sDmatCluster} is supposed to obtain clusters from a grid map. It returns an object of class "sBase".
#'
#' @param sMap an object of class "sMap"
#' @param which_neigh which neighbors in 2D output space are used for the calculation. By default, it sets to "1" for direct neighbors, and "2" for neighbors within neighbors no more than 2, and so on
#' @param distMeasure distance measure used to calculate distances in high-dimensional input space. It can be one of "median", "mean", "min" and "max" measures
#' @param constraint logic whether further constraint applied. If TRUE, only consider those hexagons 1) with 2 or more neighbors; and 2) neighbors are not within minima already found (due to the same distance)
#' @param clusterLinkage cluster linkage used to derive clusters. It can be "bmh", which accumulates a cluster just based on best-matching hexagons/rectanges but can not ensure each cluster is continuous. Instead, each cluster is continuous when using region-growing algorithm with one of "average", "complete" and "single" linkages
#' @param reindexSeed the way to index seed. It can be "hclust" for reindexing seeds according to hierarchical clustering of patterns seen in seeds, "svd" for reindexing seeds according to svd of patterns seen in seeds, or "none" for seeds being simply increased by the hexagon indexes (i.e. always in an increasing order as hexagons radiate outwards)
#' 
#' @return 
#' an object of class "sBase", a list with following components:
#' \itemize{
#'  \item{\code{seeds}: the vector to store cluster seeds, i.e., a list of local minima (in 2D output space) of distance matrix (in input space). They are represented by the indexes of hexagons/rectangles}
#'  \item{\code{bases}: the vector with the length of nHex to store the cluster memberships/bases, where nHex is the total number of hexagons/rectanges in the grid}
#'  \item{\code{ig}: an igraph object storing neighbor relations between bases, with node attributes 'name' (base), 'index', 'xcoord' and 'ycoord' (based on seeds)}
#'  \item{\code{hclust}: a hclust object storing tree-like relations between bases (based on seed model vectors)}
#'  \item{\code{call}: the call that produced this result}
#' }
#' @note The first item in the return "seeds" is the first cluster, whose memberships are those in the return "bases" that equals 1. The same relationship is held for the second item, and so on
#' @export
#' @seealso \code{\link{sPipeline}}, \code{\link{sDmatMinima}}, \code{\link{sBMH}}, \code{\link{sNeighDirect}}, \code{\link{sDistance}}, \code{\link{visDmatCluster}}
#' @include sDmatCluster.r
#' @examples
#' # 1) generate an iid normal random matrix of 100x10 
#' data <- matrix( rnorm(100*10,mean=0,sd=1), nrow=100, ncol=10) 
#'
#' # 2) get trained using by default setup
#' sMap <- sPipeline(data=data)
#'
#' # 3) partition the grid map into clusters based on different criteria
#' # 3a) based on "bmh" criterion
#' # sBase <- sDmatCluster(sMap=sMap, which_neigh=1, distMeasure="median", clusterLinkage="bmh")
#' # 3b) using region-growing algorithm with linkage "average"
#' sBase <- sDmatCluster(sMap=sMap, which_neigh=1, distMeasure="median", clusterLinkage="average")
#' 
#' # 4) visualise clusters/bases partitioned from the sMap
#' visDmatCluster(sMap,sBase)

sDmatCluster <- function(sMap, which_neigh=1, distMeasure=c("mean","median","min","max"), constraint=TRUE, clusterLinkage=c("average","complete","single","bmh"), reindexSeed=c("hclust","svd","none"))
{
    
    distMeasure <- match.arg(distMeasure)
    clusterLinkage <- match.arg(clusterLinkage)
    reindexSeed <- match.arg(reindexSeed)
    
    if (!is(sMap, "sMap")){
        stop("The funciton must apply to 'sMap' object.\n")
    }
    nHex <- sMap$nHex
    
    ## Find base seed based on local minima of distance matrix
    seed <- sDmatMinima(sMap=sMap, which_neigh=which_neigh, distMeasure=distMeasure, constraint=constraint)
    
    ## Partition the given data by flooding
    base <- matrix(0, nrow=nHex, ncol=1)
    base[seed] <- 1:length(seed)
    if(clusterLinkage == "bmh"){
        M <- sMap$codebook
        
        base_n <- which(base == 0)
        base_y <- which(base >= 1)

        res <- sBMH(sMap=M[base_y,], data=M[base_n,], which_bmh="all")
        # bmh: base_n x base_y (dlen x nHex) for bmh index
        # qerr: base_n x base_y (dlen x nHex) for the corresponding distance
        base[base_n] <- res$bmh[,1]
        
    }else{
        M <- sMap$codebook
        
        base_n <- which(base == 0)
        base_y <- which(base >= 1)
        
        ## 1-neighborhood for each hexagons/rectangles used for constrained clustering
        Ne1 <- sNeighDirect(sObj=sMap)
        
        ## nHex x nHex
        mm_dist <- sDistance(data=M, metric="euclidean")
        
        ## initial clusters
        base_inds <- as.list(seed)
        
        ## distance of the unclustered points to each cluster
        Cd <- mm_dist[base_n,base_y]
        npoints_index <- base_n # index from M
        
        #############################
        ## for contrained clustering
        ypoints_index <- unlist(base_inds) # index from M
        ypoints_Ne1_index <- which(apply(Ne1[ypoints_index,], 2, sum)>=1)
        npoints_index_interest <- intersect(npoints_index, ypoints_Ne1_index)
        CCd <- Cd
        CCd[!(npoints_index %in% npoints_index_interest),] <- Inf
        #############################
        
        while(1){
            ## find closest unclustered point k
            #matched_inds <- which(Cd == min(Cd), arr.ind=TRUE)
            matched_inds <- which(CCd == min(CCd), arr.ind=TRUE)
            
            k <- matched_inds[1]
            c <- matched_inds[2]
            
            ## make sure: npoints_index[k] and base_inds[[c]] are 1-neighborhood
            ## otherwise: continue to the next one till true
            while(1){
                if(sum(Ne1[npoints_index[k],base_inds[[c]]]) == 0){
                    CCd[k,c] <- Inf
                    c <- which(CCd[k,] == min(CCd[k,]))
                }else{
                    break
                }
            }
            
            ## add npoints_index[k] into cluster c
            base_inds[[c]] <- cbind(base_inds[[c]], npoints_index[k])
        
            ## remove point k from Cd and npoints_index
            if(k != 1 & k != nrow(Cd)){
                notk <- c(1:(k-1), (k+1):nrow(Cd))
            }else if(k == 1){
                if(k == nrow(Cd)){
                    break
                }else{
                    notk <- (k+1):nrow(Cd)
                }
            }else if(k == nrow(Cd)){
                if(k == 1){
                    break
                }else{
                    notk <- 1:(k-1)
                }
            }
            Cd <- Cd[notk,]
            npoints_index <- npoints_index[notk]
        
            ## update cluster distances to c
            dist_tmp <- mm_dist[npoints_index, base_inds[[c]]]
            if(length(npoints_index) == 1){
                dist_tmp <- matrix(dist_tmp, nrow=1, ncol=length(base_inds[[c]]))
            }
            if(clusterLinkage == "average"){
                update <- apply(dist_tmp, 1, mean)    
            }else if(clusterLinkage == "single"){
                update <- apply(dist_tmp, 1, min)    
            }else if(clusterLinkage == "complete"){
                update <- apply(dist_tmp, 1, max)    
            }
            
            if(length(npoints_index) == 1){
                Cd <- matrix(Cd, nrow=1, ncol=length(base_inds))
            }
            Cd[,c] <- update
            
            #############################
            # for contrained clustering
            ypoints_index <- unlist(base_inds) # index from M
            ypoints_Ne1_index <- which(apply(Ne1[ypoints_index,], 2, sum)>=1)
            npoints_index_interest <- intersect(npoints_index, ypoints_Ne1_index)
            CCd <- Cd
            CCd[!(npoints_index %in% npoints_index_interest),] <- Inf
            #############################
        }
        
        for (i in 1:length(base_inds)){
            base[as.vector(base_inds[[i]])] <- i
        }
        
    }
    
    ###########################################################
    ## whether reindexing seed
    if(reindexSeed=="hclust"){
        ## reordering via hierarchical clustering
        distance <- stats::as.dist(sDistance(M[seed,], metric="euclidean"))
        cluster <- stats::hclust(distance, method=c("ward.D","average")[2])
        ordering <- cluster$order
    	
    	## ?reorder.dendrogram
    	if(1){
    		# reorders the dendrogram by increasing weights, but maintaining the constraints on the dendrogram
    		# reverse twice to ensure the lower seeds tend to be lower
    		dd <- stats::as.dendrogram(cluster)
    		dd.reorder <- stats::reorder(dd, wts=rev(seq(length(seed))))
    		cluster.reorder <- stats::as.hclust(dd.reorder)
    		ordering <- cluster.reorder$order %>% rev()
    	}
    	
        ## reorder seed
        seed <- seed[ordering]
        ## reorder base
        old_index <- (1:length(seed))
        new_index <- old_index[ordering]
        base <- sapply(base, function(x) which(new_index==x))
        
        #tibble(old=base) %>% inner_join(tibble(new=1:length(seed),old=ordering), by='old') %>% pull(new) -> base
        
    }else if(reindexSeed=="svd"){
        ## reordering via SVD
        D <- M[seed,]
        sorted <- sort.int(D %*% svd(M)$v[,1], decreasing=TRUE, index.return=TRUE)
        ordering <- sorted$ix
        
        ## reorder seed
        seed <- seed[ordering]
        ## reorder base
        old_index <- (1:length(seed))
        new_index <- old_index[ordering]
        base <- sapply(base, function(x) which(new_index==x))
    
    }else{
		base <-  as.vector(base)    
    }
    
    ###########################################################
    # ig_base
    ig_base <- sMap$ig
    if(!is.null(ig_base)){
    
    	index <- from_base <- to_base <- x <- y <- name <- NULL
    	
    	## df_base
    	vec_seed <- rep(0, length(base))
		vec_seed[seed] <- seq(length(seed))
		df_base <- tibble::tibble(index=seq(sMap$nHex), base=base, seed=vec_seed) %>% dplyr::bind_cols(tibble::as_tibble(sMap$coord))
		
		## df_edge for sMap$ig
		df_edge <- sMap$ig %>% igraph::as_data_frame(what='edge') %>% tibble::as_tibble()
		
		## df_edge_base
		df_edge %>% dplyr::inner_join(df_base %>% dplyr::transmute(from=as.character(index), from_base=base), by='from') %>% dplyr::inner_join(df_base %>% dplyr::transmute(to=as.character(index), to_base=base), by='to') %>% dplyr::filter(from_base!=to_base) %>% dplyr::transmute(from_s=ifelse(from_base<to_base, from_base, to_base), to_l=ifelse(from_base<to_base, to_base, from_base)) %>% dplyr::distinct() -> df_edge_base
		
		## df_node_base
		df_base %>% dplyr::filter(seed!=0) %>% dplyr::transmute(name=base, index=index, xcoord=x, ycoord=y) %>% dplyr::arrange(name) -> df_node_base
		
		## ig_base
		ig_base <- igraph::graph_from_data_frame(d=df_edge_base, directed=FALSE, vertices=df_node_base)
		
    }
    
    ###########################################################
    # phylo
	distance <- stats::as.dist(sDistance(M[seed,], metric="euclidean"))
	cluster <- stats::hclust(distance, method=c("ward.D","average")[2])
	# reorders the dendrogram by increasing weights, but maintaining the constraints on the dendrogram
	# reverse twice to ensure the lower seeds tend to be lower
	dd <- stats::as.dendrogram(cluster)
	dd.reorder <- stats::reorder(dd, wts=rev(seq(length(seed))))
	cluster.reorder <- stats::as.hclust(dd.reorder)
	#ordering <- cluster.reorder$order %>% rev()
	#phylo <- ape::as.phylo(cluster.reorder)
	#xGT(phylo,layout=c("rectangular","fan")[1])
    
	###########################################################
    
    sBase <- list(seeds = seed, 
                  bases = base, 
                  ig = ig_base,
                  hclust = cluster.reorder,
                  call = match.call(),
                  method = "suprahex")
                
    class(sBase) <- "sBase"
    
    sBase
}