\name{print_weights}
\alias{print_weights}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
prints the weights used for the maximum entropy model.
}
\description{
Prints the weights after training the maximum entropy model. These weights are also stored in the \code{\link{maxent-class}} object returned by function \code{\link{maxent}}.
}
\usage{
print_weights()
}

\value{
Weights are printed as three columns: \code{Weight}, \code{Label}, and \code{Feature}.
}

\author{
Tim Jurka <tpjurka@ucdavis.edu>
}

\examples{
# LOAD LIBRARY
library(maxent)

# READ THE DATA, PREPARE THE CORPUS, and CREATE THE MATRIX
data <- read.csv(system.file("data/NYTimes.csv",package="maxent"))
corpus <- Corpus(VectorSource(data$Title))
matrix <- DocumentTermMatrix(corpus)

# TRAIN/PREDICT USING SPARSEM REPRESENTATION
sparse <- dtm_to_sparsem(matrix)
model <- maxent(sparse[1:1000,],as.factor(data$Topic.Code)[1:1000])

# PRINT WEIGHTS
print_weights()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
