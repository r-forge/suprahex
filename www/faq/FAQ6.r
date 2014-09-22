# <span style="font-weight:bold; color:#F87217; text-decoration:underline">The choice of 'sequential vs batch' training algorithm largely depends on the compromise: runtime, accuracy, and the nature of the input data.</span>

# Generally speaking, the 'batch' algorithm should be used when: 1) you care about the runtime, 2) input data is huge (both of rows and columns in number), and 3) input data do not contain too many zero entries. For these reasons, the function sPipeline uses 'batch' algorithm as a default choise.

# However, the 'sequential' algorithm should be favored when: 1) you really care about the accuracy and 2) are not sure the nature of the input data. For these reasons, the function sCompReorder uses 'sequential' algorithm as a default choise.

# <span style="font-weight:bold; color:#F87217; text-decoration:underline">Special note: if the input data do contain a great amount of zero entries (very sparse), the 'sequential' algorithm must be used.</span> Otherwise, using the 'batch' algorithm will lead to most of data points being clustered into one or a few of dominant hexagons/nodes, which is usually abnormal (see the example below).

# Generate data with an iid matrix of 100 x 10
data <- matrix(rnorm(100*10,mean=0,sd=1), nrow=100,ncol=10)
colnames(data) <- paste('S', seq(1:10), sep="")
# Force those negatives to be zeros, and thus being very sparse
data[data<0] <- 0
# Train using the 'batch' algorthm
sMap <- sPipeline(data, algorithm="batch")
## Look at the number of input data vectors hitting the hexagons
visHexMapping(sMap, mappingType="hits")
# Now, train using the 'sequential' algorthm
sMap <- sPipeline(data, algorithm="sequential")
## Look at the number of input data vectors hitting the hexagons
visHexMapping(sMap, mappingType="hits")
