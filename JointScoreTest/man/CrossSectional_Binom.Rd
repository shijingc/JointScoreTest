\name{CrossSectional_Binom}
\alias{CrossSectional_Binom}
\docType{data}
\title{
Example dataset, a list of outcome variable, design matrices of fixed and random effects 
}
\description{
an example dataset from a cross sectional study design, a list of objects which contains `y' for the vecotr of binomial outcomes (where the number of repeated trials of the binomial experiment is 6); 
`X' for the design matrix of the fixed effects to be tested for; `S' for the design matrix of the fixed effects to be adjusted for;
`U' for the design matrix of the random effects to be tested for; and `SIGMA' for the scaled covariance matrix of the testing random effecs.
}
\usage{
data(CrossSectional_Binom)
}

\examples{

data(CrossSectional_Binom)

head(CrossSectional_Binom$X)

head(CrossSectional_Binom$S)

head(CrossSectional_Binom$U)

head(CrossSectional_Binom$SIGMA)

}
\keyword{datasets}
