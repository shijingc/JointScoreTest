\name{Longi_Gaussian}
\alias{Longi_Gaussian}
\docType{data}
\title{
Example dataset, a list of outcome variable, design matrices of fixed and random effects 
}
\description{
an example dataset from a longitudinal study design, a list of objects which contains `y' for the vecotr of continuous outcomes; 
`X' for the design matrix of the fixed effects to be tested for; `S' for the design matrix of the fixed effects to be adjusted for;
`U' for the design matrix of the random effects to be tested for; `Z' for the design matrix of the random effects to be adjusted for;
`SIGMA' for the scaled covariance matrix of the testing random effecs; and `group' for the vector describing the clustering of the response variable.
}
\usage{
data(Longi_Gaussian)
}

\examples{

data(Longi_Gaussian)

head(Longi_Gaussian$X)

head(Longi_Gaussian$S)

head(Longi_Gaussian$U)

head(Longi_Gaussian$Z)

head(Longi_Gaussian$SIGMA)

head(Longi_Gaussian$group)

}
\keyword{datasets}
