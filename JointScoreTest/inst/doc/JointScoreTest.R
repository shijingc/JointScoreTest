### R code from vignette source 'JointScoreTest.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: data1
###################################################
###########################################################################
####### To run this code, first download and unzip example files
###########################################################################


############################################################# 
## Joint testing of the fixed and random effects in a GLMM without nuisance 
## random effects in the null model, the outcome varianble is of the 
## continuous response type
###################################################################

library(JointScoreTest)
y = as.vector(read.table('CrossSectional_Gaussian_y.txt'))
X = as.matrix(read.table('CrossSectional_Gaussian_X.txt'))
S = as.matrix(read.table('CrossSectional_Gaussian_S.txt'))
U = as.matrix(read.table('CrossSectional_Gaussian_U.txt'))
SIGMA = as.matrix(read.table('CrossSectional_Gaussian_SIGMA.txt'))

##########

CrossSectional_Gaussian = list()
CrossSectional_Gaussian$y = y[,1]
CrossSectional_Gaussian$X = X
CrossSectional_Gaussian$S =S
CrossSectional_Gaussian$U = U
CrossSectional_Gaussian$SIGMA = SIGMA




###################################################
### code chunk number 2: Test1
###################################################

JointScoreTest_withoutNuisanceVC(CrossSectional_Gaussian$y, CrossSectional_Gaussian$X, 
	CrossSectional_Gaussian$S, CrossSectional_Gaussian$U, 
	CrossSectional_Gaussian$SIGMA, out_type = "C")




###################################################
### code chunk number 3: data2
###################################################
############################################################# 
## Joint testing of the fixed and random effects in a GLMM without nuisance 
## random effects in the null model, the outcome varianble is of 
## the binomial response type
###################################################################

y = as.vector(read.table('CrossSectional_Binom_y.txt'))
X = as.matrix(read.table('CrossSectional_Binom_X.txt'))
S = as.matrix(read.table('CrossSectional_Binom_S.txt'))
U = as.matrix(read.table('CrossSectional_Binom_U.txt'))
SIGMA = as.matrix(read.table('CrossSectional_Binom_SIGMA.txt'))

############

CrossSectional_Binom = list()
CrossSectional_Binom$y = y[,1]
CrossSectional_Binom$X = X
CrossSectional_Binom$S =S
CrossSectional_Binom$U = U
CrossSectional_Binom$SIGMA = SIGMA



###################################################
### code chunk number 4: Test2
###################################################

JointScoreTest_withoutNuisanceVC(CrossSectional_Binom$y, CrossSectional_Binom$X, 
	CrossSectional_Binom$S, CrossSectional_Binom$U, 
	CrossSectional_Binom$SIGMA, out_type = "D", binom_size = 6)




###################################################
### code chunk number 5: data3
###################################################

############################################################# 
## Joint testing of the fixed and random effects in a GLMM with nuisance 
## random effects in the null model, the outcome varianble is of the 
## continuous response type
###################################################################

y = as.vector(read.table('Longi_Gaussian_y.txt'))
X = as.matrix(read.table('Longi_Gaussian_X.txt'))
S = as.matrix(read.table('Longi_Gaussian_S.txt'))
U = as.matrix(read.table('Longi_Gaussian_U.txt'))
Z = as.matrix(read.table('Longi_Gaussian_Z.txt'))
SIGMA = as.matrix(read.table('Longi_Gaussian_SIGMA.txt'))
group = as.matrix(read.table('Longi_Gaussian_group.txt'))

############

Longi_Gaussian = list()
Longi_Gaussian$y = y[,1]
Longi_Gaussian$X = X
Longi_Gaussian$S =S
Longi_Gaussian$U = U
Longi_Gaussian$Z = Z
Longi_Gaussian$SIGMA = SIGMA
Longi_Gaussian$group = group



###################################################
### code chunk number 6: Test3
###################################################
JointScoreTest_withNuisanceVC(Longi_Gaussian$y, Longi_Gaussian$X, 
	Longi_Gaussian$S, Longi_Gaussian$U, 
	Longi_Gaussian$Z, Longi_Gaussian$group, 
	Longi_Gaussian$SIGMA, out_type = "C")


###################################################
### code chunk number 7: data4
###################################################

############################################################# 
## Joint testing of the fixed and random effects in a GLMM with nuisance 
## random effects in the null model, the outcome varianble is of the 
## binomial response type
###################################################################

y = as.vector(read.table('Longi_Binom_y.txt'))
X = as.matrix(read.table('Longi_Binom_X.txt'))
S = as.matrix(read.table('Longi_Binom_S.txt'))
U = as.matrix(read.table('Longi_Binom_U.txt'))
Z = as.matrix(read.table('Longi_Binom_Z.txt'))
SIGMA = as.matrix(read.table('Longi_Binom_SIGMA.txt'))
group = as.matrix(read.table('Longi_Binom_group.txt'))

############

Longi_Binom = list()
Longi_Binom$y = y[,1]
Longi_Binom$X = X
Longi_Binom$S =S
Longi_Binom$U = U
Longi_Binom$Z = Z
Longi_Binom$SIGMA = SIGMA
Longi_Binom$group = group




###################################################
### code chunk number 8: Test4
###################################################
JointScoreTest_withNuisanceVC(Longi_Binom$y, Longi_Binom$X, 
	Longi_Binom$S, Longi_Binom$U, Longi_Binom$Z, 
	Longi_Binom$group, Longi_Binom$SIGMA, 
	out_type = "D", binom_size = 6)


