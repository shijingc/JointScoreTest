\name{JointScoreTest_withNuisanceVC}
\alias{JointScoreTest_withNuisanceVC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Joint testing of the fixed and random effects in a GLMM with nuisance random effects
}
\description{
Implementing the score test for jointly testing the fixed and random effects in a GLMM with nuisance random effects in the null model.
}
\usage{
JointScoreTest_withNuisanceVC(y, X, S, U, Z, group, SIGMA, 
	rho = c(0, 0.25, 0.5, 0.75, 1), out_type = "C", binom_size = 1, B0 = 500)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
A vector for the response variable.
}
  \item{X}{
The design matrix for the testing fixed effects.
}
  \item{S}{
The design matrix for the adjusting fixed effects.
}
  \item{U}{
The design matrix for the testing random effects.
}
  \item{Z}{
The design matrix for the adjusting random effects (i.e. the nuisance random effects).
}
  \item{group}{
A vector describing the clustering of the response variable.
}
  \item{SIGMA}{
The scaled covariance matrix for the testing random effects. 
}
  \item{rho}{
A vector of possible values of \eqn{\rho} to be used, default = c(0, 0.25, 0.5, 0.75, 1).
}
  \item{out_type}{
an indicator for the outcome type. `C' for continuous outcomes and `D' for
binomial outcomes.
}
  \item{binom_size}{
the number of repeated trials of the binomial experiment, for example, binom_size = 1 if the outcome if a binary outcome. 
}
  \item{B0}{
The number of iterations for the re-sampling based copula method, default is 500.
}
}

\value{
\item{pvalue }{P-value of the score test.}
\item{ opt.rho }{The value in the rho sequence which yields the largest score statistic.}
\item{ score.each }{The score statistic for each of the rho value in the rho sequence.}
}

\examples{
############################################################# 

# load the example data
data(Longi_Gaussian)
data(Longi_Binom)

############################################################# 
# Joint testing of the fixed and random effects in a GLMM with nuisance 
# random effects in the null model, the outcome varianble is of the
# continuous response type
  
JointScoreTest_withNuisanceVC(Longi_Gaussian$y[,1], Longi_Gaussian$X, 
	Longi_Gaussian$S, Longi_Gaussian$U, Longi_Gaussian$Z, 
	Longi_Gaussian$group, Longi_Gaussian$SIGMA, out_type = "C")

############################################################# 
# Joint testing of the fixed and random effects in a GLMM with nuisance 
# random effects in the null model, the outcome varianble is of the
# binomial response type
  
JointScoreTest_withNuisanceVC(Longi_Binom$y[,1], Longi_Binom$X, 
	Longi_Binom$S, Longi_Binom$U, Longi_Binom$Z, Longi_Binom$group,
	Longi_Binom$SIGMA, out_type = "D", binom_size = 6)


}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

