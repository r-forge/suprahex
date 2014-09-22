# We do not offer this functionality partly because the meta-cluster id has been displayed within the map (at seeds). 

# <span style="font-weight:bold; color:#F87217; text-decoration:underline">Another key reason is based on the fact that: the users are always willing to check gene expression pattern within each meta-cluster. Because of this, we have provided the function visDmatHeatmap, from which the legend key is shown.</span>

data <- matrix( rnorm(100*10,mean=0,sd=1), nrow=100, ncol=10)
colnames(data) <- paste('S', seq(1:10), sep="")
sMap <- sPipeline(data=data)
sBase <- sDmatCluster(sMap=sMap)
visDmatCluster(sMap,sBase)
output <- visDmatHeatmap(sMap, data, sBase, base.legend.location="bottomleft", labRow=NA)

## As you have seen, heatmap is used to visualise patterns seen in genes within each meta-cluster/base. Row side bar indicates the meta-clusters/bases. 

## The returned variable "output" (NOT a text file) has 1st column for your input data ID (an integer; otherwise the row names of input data matrix), and 2nd column for the corresponding index of best-matching hexagons (i.e. gene clusters), and 3rd column for the cluster bases (i.e. gene meta-clusters). <span style="font-weight:bold; color:#F87217; text-decoration:underline">Note: it has rows in the same order as visualised in the heatmap.</span> You can save this output into the file 'output.txt':
write.table(output, file="output.txt", quote=F, row.names=F, sep="\t")
