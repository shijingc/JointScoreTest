\name{Longi_Binom}
\alias{Longi_Binom}
\docType{data}
\title{
Example dataset, a list of outcome variable, design matrices of fixed and random effects 
}
\description{
an example dataset from a longitudinal study design, a list of objects which contains `y' for the vecotr of binomial outcomes (where the number of repeated trials of the binomial experiment is 6); 
`X' for the design matrix of the fixed effects to be tested for; `S' for the design matrix of the fixed effects to be adjusted for;
`U' for the design matrix of the random effects to be tested for; `Z' for the design matrix of the random effects to be adjusted for;
`SIGMA' for the scaled covariance matrix of the testing random effecs; and `group' for the vector describing the clustering of the response variable.
}
\usage{
data(Longi_Binom)
}

\examples{

data(Longi_Binom)

head(Longi_Binom$X)

head(Longi_Binom$S)

head(Longi_Binom$U)

head(Longi_Binom$Z)

head(Longi_Binom$SIGMA)

head(Longi_Binom$group)

}
\keyword{datasets}
