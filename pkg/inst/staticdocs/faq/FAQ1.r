# <span style="font-weight:bold; color:#F87217; text-decoration:underline">A supra-hexagonal map is a a giant hexagon formed seamlessly by smaller hexagons.</span> This architecture is prevalent in many natural and man-made objects, such as a honeycomb or at Giant's Causeway. It has symmetric beauty around the center, from which individual hexagons radiate outwards.

# generate data with an iid matrix of 1000 x 3
data <- matrix(rnorm(1000*3,mean=0,sd=1), nrow=1000, ncol=3)
# train a supra-hexagonal map by data
sMap <- sPipeline(data, xdim=15)
# visualise the supra-hexagonal map with node numbering
visHexMapping(sMap, mappingType="indexes")

# As you have seen, this architecture has a total of 169 smaller hexagons (ie map nodes) that are indexed as follows: start from the center, and then expand circularly outwards, and for each circle increase in an anti-clockwise order. <span style="font-weight:bold; color:#F87217; text-decoration:underline">It is uniquely determined by the x- or y-dimension of the map grid (or its radius r).</span>

# There is an inherent relationship between x-dimension (or y-dimension), the radius r and the total number of hexagons nHex:
xdim <- sMap$xdim
r <- (xdim + 1)/2
nHex <- 1 + 6 * r * (r - 1)/2
