# visDmatCluster allows for visualisation of gene meta-clusters.

# The identification of gene meta-clusters is done by its sister fucntion sDmatCluster.

# Yes. <span style="font-weight:bold; color:#F87217; text-decoration:underline">These meta-clusters can also be useful to correlate with sample relationships displayed by visCompReorder.</span> See an example below:

data <- matrix( rnorm(100*10,mean=0,sd=1), nrow=100, ncol=10)
colnames(data) <- paste('S', seq(1:10), sep="")
sMap <- sPipeline(data=data)
sReorder <- sCompReorder(sMap=sMap)
visCompReorder(sMap=sMap, sReorder=sReorder)
sBase <- sDmatCluster(sMap=sMap)
visDmatCluster(sMap,sBase)

# As you have seen the previous two images, not only can you tell sample relationships but also the meta-clusters wherein. 
