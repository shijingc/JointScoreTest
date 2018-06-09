pkgname <- "JointScoreTest"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('JointScoreTest')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("CrossSectional_Binom")
### * CrossSectional_Binom

flush(stderr()); flush(stdout())

### Name: CrossSectional_Binom
### Title: Example dataset, a list of outcome variable, design matrices of
###   fixed and random effects
### Aliases: CrossSectional_Binom
### Keywords: datasets

### ** Examples


data(CrossSectional_Binom)

head(CrossSectional_Binom$X)

head(CrossSectional_Binom$S)

head(CrossSectional_Binom$U)

head(CrossSectional_Binom$SIGMA)




cleanEx()
nameEx("CrossSectional_Gaussian")
### * CrossSectional_Gaussian

flush(stderr()); flush(stdout())

### Name: CrossSectional_Gaussian
### Title: Example dataset, a list of outcome variable, design matrices of
###   fixed and random effects
### Aliases: CrossSectional_Gaussian
### Keywords: datasets

### ** Examples

data(CrossSectional_Gaussian)

head(CrossSectional_Gaussian$X)

head(CrossSectional_Gaussian$S)

head(CrossSectional_Gaussian$U)

head(CrossSectional_Gaussian$SIGMA)





cleanEx()
nameEx("JointScoreTest_withNuisanceVC")
### * JointScoreTest_withNuisanceVC

flush(stderr()); flush(stdout())

### Name: JointScoreTest_withNuisanceVC
### Title: Joint testing of the fixed and random effects in a GLMM with
###   nuisance random effects
### Aliases: JointScoreTest_withNuisanceVC
### Keywords: ~kwd1 ~kwd2

### ** Examples

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





cleanEx()
nameEx("JointScoreTest_withoutNuisanceVC")
### * JointScoreTest_withoutNuisanceVC

flush(stderr()); flush(stdout())

### Name: JointScoreTest_withoutNuisanceVC
### Title: Joint testing of the fixed and random effects in a GLMM with no
###   nuisance random effects.
### Aliases: JointScoreTest_withoutNuisanceVC
### Keywords: ~kwd1 ~kwd2

### ** Examples

############################################################# 

# load the example data
data(CrossSectional_Gaussian)
data(CrossSectional_Binom)

############################################################# 
# Joint testing of the fixed and random effects in a GLMM without nuisance 
# random effects in the null model, the outcome varianble is of the
# continuous response type
  
JointScoreTest_withoutNuisanceVC(CrossSectional_Gaussian$y[,1], 
	CrossSectional_Gaussian$X, CrossSectional_Gaussian$S, 
	CrossSectional_Gaussian$U, CrossSectional_Gaussian$SIGMA, 
	out_type = "C")

############################################################# 
# Joint testing of the fixed and random effects in a GLMM without nuisance 
# random effects in the null model, the outcome varianble is of the
# binomial response type
  
JointScoreTest_withoutNuisanceVC(CrossSectional_Binom$y[,1], 
	CrossSectional_Binom$X, CrossSectional_Binom$S, 
	CrossSectional_Binom$U, CrossSectional_Binom$SIGMA, 
	out_type = "D", binom_size = 6)





cleanEx()
nameEx("Longi_Binom")
### * Longi_Binom

flush(stderr()); flush(stdout())

### Name: Longi_Binom
### Title: Example dataset, a list of outcome variable, design matrices of
###   fixed and random effects
### Aliases: Longi_Binom
### Keywords: datasets

### ** Examples


data(Longi_Binom)

head(Longi_Binom$X)

head(Longi_Binom$S)

head(Longi_Binom$U)

head(Longi_Binom$Z)

head(Longi_Binom$SIGMA)

head(Longi_Binom$group)




cleanEx()
nameEx("Longi_Gaussian")
### * Longi_Gaussian

flush(stderr()); flush(stdout())

### Name: Longi_Gaussian
### Title: Example dataset, a list of outcome variable, design matrices of
###   fixed and random effects
### Aliases: Longi_Gaussian
### Keywords: datasets

### ** Examples


data(Longi_Gaussian)

head(Longi_Gaussian$X)

head(Longi_Gaussian$S)

head(Longi_Gaussian$U)

head(Longi_Gaussian$Z)

head(Longi_Gaussian$SIGMA)

head(Longi_Gaussian$group)




### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
