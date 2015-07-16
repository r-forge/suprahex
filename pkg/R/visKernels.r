#' Function to visualize neighborhood kernels
#'
#' \code{visKernels} is supposed to visualize a series of neighborhood kernels, each of which is a non-increasing functions of: i) the distance \eqn{d_{wi}} between the hexagon/rectangle \eqn{i} and the winner \eqn{w}, and ii) the radius \eqn{\delta_t} at time \eqn{t}.
#'
#' @param newpage logical to indicate whether to open a new page. By default, it sets to true for opening a new page
#' @return 
#' invisible
#' @note There are five kernels that are currently supported:
#' \itemize{
#' \item{For "gaussian" kernel, \eqn{h_{wi}(t)=e^{-d_{wi}^2/(2*\delta_t^2)}}}
#' \item{For "cutguassian" kernel, \eqn{h_{wi}(t)=e^{-d_{wi}^2/(2*\delta_t^2)}*(d_{wi} \le \delta_t)}}
#' \item{For "bubble" kernel, \eqn{h_{wi}(t)=(d_{wi} \le \delta_t)}}
#' \item{For "ep" kernel, \eqn{h_{wi}(t)=(1-d_{wi}^2/\delta_t^2)*(d_{wi} \le \delta_t)}}
#' \item{For "gamma" kernel, \eqn{h_{wi}(t)=1/\Gamma(d_{wi}^2/(4*\delta_t^2)+2)}}
#' }
#' These kernels above are displayed within a plot for each fixed radius. Three different radii (i.e., 1 and 2) are illustrated. 
#' @export
#' @seealso \code{\link{sTrainSeq}}, \code{\link{sTrainBatch}}
#' @include visKernels.r
#' @examples
#' # visualise currently supported five kernels
#' visKernels()

visKernels <-function (newpage=T) 
{

    if (newpage){
        dev.new(width=12, height=6)
    }
    
    par(mfrow=c(1,2), cex.axis=1.2, cex.lab=1.2, cex.main=1.5)
    par(mgp=c(2.5, 1, 0)) # In addition to changing the margin size of your charts, you may also want to change the way axes and labels are spatially arranged. One method of doing so is the mgp parameter option. The mgp setting is defined by a three item vector wherein the first value represents the distance of the axis labels or titles from the axes, the second value is the distance of the tick mark labels from the axes, and the third is the distance of the tick mark symbols from the axes
    
    cl <- rainbow(5)
    ph <- c(21,22,17,18,19)

    for (r in 1:2){
        x=seq(0,5,by=0.1)
    
        ## gaussian
        y1 <- exp(-1*x^2/(2*r^2))
    
        ## ep
        y2=1-(x/r)^2
        y2[y2<=0] <- 0
    
        ## bubble
        y3 <- 1*(r-x>0)
    
        ## cutgaussian
        y4 <- y1
        y4[r-x<=0] <- 0
        
        ## gamma
        y5 <- 1/factorial(x^2/(4*r^2)+1)

        if(r==1){
            tit <- expression(paste("Radius ", delta[t], "=", 1, sep=""))
        }else if(r==2){
            tit <- expression(paste("Radius ", delta[t], "=", 2, sep=""))
        }else if(r==3){
            tit <- expression(paste("Radius ", delta[t], "=", 3, sep=""))
        }
        
        plot(0,0, xlim = c(0,5),ylim = c(0,1), type = "n",
        xlab=expression(paste("Distance ", d[wi], " between the hexagon i and the winner w", sep="")), 
        ylab=expression(paste("Neighborhood kernel ", h[wi](t), sep="")),
        main=tit
        )
        lines(x, y1, type = "b", pch=ph[1], col = cl[1])
        lines(x, y2, type = "b", pch=ph[2], col = cl[2])
        lines(x, y3, type = "b", pch=ph[3], col = cl[3])
        lines(x, y4, type = "b", pch=ph[4], col = cl[4])
        lines(x, y5, type = "b", pch=ph[5], col = cl[5])
    
        if(r==1){
            leg.txt <- c(
            expression(paste("gaussian: ", h[wi](t), "=", exp*(frac(-d[wi]^2,2*delta[t]^2)), sep="")), 
            expression(paste("ep: ", h[wi](t), "=", (1-frac(d[wi]^2,delta[t]^2))*(d[wi]<=delta[t]), sep="")), 
            expression(paste("bubble: ", h[wi](t), "=", (d[wi]<=delta[t]), sep="")),
            expression(paste("cutgaussian: ", h[wi](t), "=", exp*(frac(-d[wi]^2,2*delta[t]^2))*(d[wi]<=delta[t]), sep="")),
            expression(paste("gamma: ", h[wi](t), "=", 1/Gamma(frac(d[wi]^2,4*delta[t]^2)+2), sep=""))
            )
            
            legend("topright", legend=leg.txt, pch=ph, col=cl, border = "transparent", box.col = "transparent", cex=0.8)
        }
    }
    
    invisible()
}