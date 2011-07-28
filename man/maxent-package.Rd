\name{maxent-package}
\alias{maxent-package}
\docType{package}
\title{
package for maximum entropy text classification.
}
\description{
\pkg{maxent} is an R package with tools for text classification using multinomial logistic regression, also known as maximum entropy. The focus of this maximum entropy classifier is to minimize memory consumption on very large datasets, particularly sparse document-term matrices represented by the \pkg{tm} package. The library is built on top of an efficient C++ implementation written by Yoshimasa Tsuruoka.
}
\details{
\tabular{ll}{
Package: \tab maxent\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2011-07-28\cr
License: \tab GPL-3\cr
LazyLoad: \tab yes\cr
}
}
\author{
Tim Jurka

Maintainer: Tim Jurka <tpjurka@ucdavis.edu>
}
\references{
Y. Tsuruoka. "A simple C++ library for maximum entropy classification." University of Tokyo Department of Computer Science (Tsujii Laboratory), 2006. URL \url{http://www-tsujii.is.s.u-tokyo.ac.jp/~tsuruoka/maxent/}.
}
\keyword{ package }

\examples{
# LOAD LIBRARY
library(maxent)

# READ THE DATA, PREPARE THE CORPUS, and CREATE THE MATRIX
data <- read.csv(system.file("data/NYTimes.csv",package="maxent"))
corpus <- Corpus(VectorSource(data$Title))
matrix <- DocumentTermMatrix(corpus)

sparse <- dtm_to_sparsem(matrix[1:1000,])
normal <- as.matrix(matrix[1:3000,])

# TRAIN/PREDICT USING SPARSEM REPRESENTATION
sparse <- dtm_to_sparsem(matrix)
model <- maxent(sparse[1:1000,],data$Topic.Code[1:1000])
results <- predict(model,sparse[1001:1500,])

# TRAIN/PREDICT USING REGULAR MATRIX REPRESENTATION
model <- maxent(as.matrix(matrix)[1:1000,],as.factor(data$Topic.Code)[1:1000])
results <- predict(model,as.matrix(matrix)[1001:1500,])
}
