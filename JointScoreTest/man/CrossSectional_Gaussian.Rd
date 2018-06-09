\name{CrossSectional_Gaussian}
\alias{CrossSectional_Gaussian}
\docType{data}
\title{
Example dataset, a list of outcome variable, design matrices of fixed and random effects 
}
\description{
an example dataset from a cross sectional study design, a list of objects which contains `y' for the vecotr of continuous outcomes; 
`X' for the design matrix of the fixed effects to be tested for; `S' for the design matrix of the fixed effects to be adjusted for;
`U' for the design matrix of the random effects to be tested for; and `SIGMA' for the scaled covariance matrix of the testing random effecs.
}
\usage{
data(CrossSectional_Gaussian)
}
\examples{
data(CrossSectional_Gaussian)

head(CrossSectional_Gaussian$X)

head(CrossSectional_Gaussian$S)

head(CrossSectional_Gaussian$U)

head(CrossSectional_Gaussian$SIGMA)


}
\keyword{datasets}
