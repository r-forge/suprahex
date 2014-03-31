\name{sDistance}
\alias{sDistance}
\title{Function to compute the pairwise distance for a given data matrix}
\usage{
sDistance(data, metric = c("pearson", "spearman", "kendall",
"euclidean",
"manhattan", "cos", "mi"))
}
\arguments{
\item{data}{a data frame or matrix of input data}

\item{metric}{distance metric used to calculate a
symmetric distance matrix. See 'Note' below for options
available}
}
\value{
\itemize{ \item{\code{dist}: a symmetric distance matrix of
nRow x nRow, where nRow is the number of rows of input data
matrix} }
}
\description{
\code{sDistance} is supposed to compute and return the
distance matrix between the rows of a data matrix using a
specified distance metric
}
\note{
The distance metrics are supported: \itemize{
\item{"pearson": Pearson correlation. Note that two curves
that have identical shape, but different magnitude will
still have a correlation of 1} \item{"spearman": Spearman
rank correlation. As a nonparametric version of the pearson
correlation, it calculates the correlation between the
ranks of the data values in the two vectors (more robust
against outliers)} \item{"kendall": Kendall tau rank
correlation. Compared to spearman rank correlation, it goes
a step further by using only the relative ordering to
calculate the correlation. For all pairs of data points
\eqn{(x_i, y_i)} and \eqn{(x_j, y_j)}, it calls a pair of
points either as concordant (\eqn{Nc} in total) if
\eqn{(x_i - x_j)*(y_i - y_j)>0}, or as discordant (\eqn{Nd}
in total) if \eqn{(x_i - x_j)*(y_i - y_j)<0}. Finally, it
calculates gamma coefficient \eqn{(Nc-Nd)/(Nc+Nd)} as a
measure of association which is highly resistant to tied
data} \item{"euclidean": Euclidean distance. Unlike the
correlation-based distance measures, it takes the magnitude
into account (input data should be suitably normalized}
\item{"manhattan": Cityblock distance. The distance between
two vectors is the sum of absolute value of their
differences along any coordinate dimension} \item{"cos":
Cosine similarity. As an uncentered version of pearson
correlation, it is a measure of similarity between two
vectors of an inner product space, i.e., measuring the
cosine of the angle between them (using a dot product and
magnitude)} \item{"mi": Mutual information (MI). \eqn{MI}
provides a general measure of dependencies between
variables, in particular, positive, negative and nonlinear
correlations. The caclulation of \eqn{MI} is implemented
via applying adaptive partitioning method for deriving
equal-probability bins (i.e., each bin contains
approximately the same number of data points). The number
of bins is heuristically determined (the lower bound):
\eqn{1+log2(n)}, where n is the length of the vector.
Because \eqn{MI} increases with entropy, we normalize it to
allow comparison of different pairwise clone similarities:
\eqn{2*MI/[H(x)+H(y)]}, where \eqn{H(x)} and \eqn{H(y)}
stand for the entropy for the vector \eqn{x} and \eqn{y},
respectively} }
}
\examples{
# 1) generate an iid normal random matrix of 100x10
data <- matrix( rnorm(100*10,mean=0,sd=1), nrow=100, ncol=10)

# 2) calculate distance matrix using different metric
sMap <- sPipeline(data=data)
# 2a) using "pearson" metric
dist <- sDistance(data=data, metric="pearson")
# 2b) using "cos" metric
# dist <- sDistance(data=data, metric="cos")
# 2c) using "spearman" metric
# dist <- sDistance(data=data, metric="spearman")
# 2d) using "kendall" metric
# dist <- sDistance(data=data, metric="kendall")
# 2e) using "euclidean" metric
# dist <- sDistance(data=data, metric="euclidean")
# 2f) using "manhattan" metric
# dist <- sDistance(data=data, metric="manhattan")
# 2g) using "mi" metric
# dist <- sDistance(data=data, metric="mi")
}
\seealso{
\code{\link{sDmatCluster}}
}

