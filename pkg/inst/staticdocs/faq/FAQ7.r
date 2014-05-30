# <span style="font-weight:bold; color:#F87217; text-decoration:underline">The neighborhood kernel  dictates the final topology of the trained map, which is a non-increasing functions of: 1) the distance between the hexagon/rectangle and the winner, and 2) the radius.</span> There are five kernels supported so far:
visKernels()
# As you have seen, these kernels are displayed within a plot for each fixed radius; two different radii (i.e. 1 and 2) are illustrated only. From the mathematical definitions and curve forms above, it is clear that <span style="font-weight:bold; color:#F87217; text-decoration:underline">the "gamma" and "gaussian" kernels exert more global influence, the "ep" kernel puts more emphasis on local topological relationships, and the other two "cutgaussian" and "bubble" keep the relative balance.</span> 

# It becomes much clearer when using the function visHexMulComp to visualise trained maps using the same data input and the same trainology but choosing different kernels.

# Generate data
data <- cbind(
matrix(rnorm(1000*3,mean=0.5,sd=1), nrow=1000, ncol=3), 
matrix(rnorm(1000*3,mean=-0.5,sd=1), nrow=1000, ncol=3)
)
colnames(data) <- c("S1","S1","S1","S2","S2","S2")

# with "gaussian" kernel (by default)
sMap_ga <- sPipeline(data=data, neighKernel="gaussian", init="uniform")
visHexMulComp(sMap_ga)
# with "gamma" kernel
sMap_gm <- sPipeline(data=data, neighKernel="gamma", init="uniform")
visHexMulComp(sMap_gm)
# with "ep" kernel
sMap_ep <- sPipeline(data=data, neighKernel="ep", init="uniform")
visHexMulComp(sMap_ep)
# with "cutgaussian" kernel
sMap_cu <- sPipeline(data=data, neighKernel="cutgaussian", init="uniform")
visHexMulComp(sMap_cu)
# with "bubble" kernel
sMap_bu <- sPipeline(data=data, neighKernel="bubble", init="uniform")
visHexMulComp(sMap_bu)
