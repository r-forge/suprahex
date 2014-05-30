# The layout shown in visCompReorder cannot be controlled by itself. The same thing is true for the side legend, which is automatically changed to fit the overall layout. 

# <span style="font-weight:bold; color:#F87217; text-decoration:underline">Instead, it is controlled by the sister function sCompReorder, from which xdim (and/or ydim) can be specified to layout the dimension of the rectangular lattice.</span>

# generate an iid normal random matrix of 100x10
data <- matrix( rnorm(100*10,mean=0,sd=1), nrow=100, ncol=10)
colnames(data) <- paste('S', seq(1:10), sep="")
# get trained using by default setup
sMap <- sPipeline(data=data)
# reorder component planes
sReorder <- sCompReorder(sMap=sMap)
# visualise multiple component planes reorded within a sheet-shape rectangle grid
visCompReorder(sMap=sMap, sReorder=sReorder)

# As you have seen, these 10 samples are organised onto a 6 X 5 rectangle grid. You can change it to 7 X 7, for example:
sReorder <- sCompReorder(sMap=sMap, xdim=7, ydim=7)
visCompReorder(sMap=sMap, sReorder=sReorder)

# <span style="font-weight:bold; color:#F87217; text-decoration:underline">In the function sComReorder, rather than specifying dimensions it is better to change another parameter 'amplifier', which means amplifying the number of samples as a total of nodes in the rectangle grid.</span> The rest work will be done by the function. Internally, once the total nodes are fixed, the xdim/ydim ratio is the square root of the two biggest eigenvalues of the input data.
