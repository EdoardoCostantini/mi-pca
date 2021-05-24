# Packages ----------------------------------------------------------------

library(MASS)
library(nnet) # for multinomial regression
library(kamila)
library(BinNor)
library(OrdNor)
library(PoisNor)
library(PoisBinOrdNor)

# Transform continuous variables to categorical predictors ----------------

set.seed(1234)
Sigma <- matrix(c(1, .7, .7,
                  .7, 1, .7,
                  .7, .7, 1), 
                ncol = 3)
mu <- rep(2, ncol(Sigma))
n <- 1e3
X <- MASS::mvrnorm(n, mu, Sigma)

lm.out1 <- lm(X[, 1] ~ X[, 2])
lm.out2 <- lm(X[, 1] ~ X[, 2] + X[, 3])
summary(lm.out1)
summary(lm.out2)

# Multinomial Logistic
# Coefficients (K-1 values (where K = number of categories of DV)
set.seed(1234)
a <- c(1, 1, 1, 1, 1)
b <- c(1, 2, 3, 4, 3)
K <- length(a)
x <- X[, 3]

# Calculation of denominator for probability calculation
denom <- 1 + rowSums(sapply(1:K, 
                        function(i) {
                          exp(a[i] + x * b[i])
                        }
)
)

# Calculating the matrix of probabilities for each category of the DV
vProb <- cbind(1/denom, 
               sapply(1:K, 
                      function(i) {
                        exp(a[i] + x * b[i])/denom
                      }
               )
)

# Assigning the value one to maximum probability and zero for rest to 
# get the appropriate choices for value of x
DV_location = t(apply(vProb, 1, rmultinom, n = 1, size = 1)) 
# 1 for the category in which is most 
# likely that an observation is

# Assing value of Categorical Variable
DV <- apply(DV_location, 1, function(x) which(x==1))
table(DV)

X3_factor <- factor(DV)
fit2 <- multinom(X3_factor ~ X[, 3])
coef(fit2)
fit_sum <- summary(fit2)
fit_sum$

# Fit models
summary(lm(X[, 1] ~ X[, 2]))$r.squared
summary(lm(X[, 1] ~ X[, 2] + X[, 3]))$r.squared
summary(lm(X[, 1] ~ X[, 2] + X3_factor))$r.squared
summary(lm(X[, 1] ~ X3_factor))$r.squared
summary(lm(X[, 1] ~ X[, 3]))$r.squared


# Kamila ------------------------------------------------------------------

dat <- genMixedData(sampSize = 100, 
                    nConVar = 2, 
                    nCatVar = 2,
                    nCatLevels = 4,
                    nConWithErr = 1,
                    nCatWithErr = 1,
                    popProportions = c(0.3,0.7),
                    conErrLev = 0.3,
                    catErrLev = 0.2)
dat$catVars
dat$catVars

head(dat)
with(dat, plot(conVars, col = trueID))
with(dat,table(data.frame(catVars[,1:2],trueID, stringsAsFactors = TRUE)))


# Work in progress --------------------------------------------------------

set.seed(1234)
n <- 1e3
p <- 10
pr_junk <- .2 # proportion of junk variables
data_type <- c("num", "likert", "cat")

# Sample Continuous Variables ---------------------------------------------

# Define useful and junk variables numbers

p_u <- p * (1 - pr_junk)
p_j <- p * (pr_junk)

# Sample high correlation values for the useful variables

p_u_rho <- runif(n = p_u * (p_u - 1) / 2, 
                 min = .9, max = .95)

# Populate the Correlation matrix with these values
Sigma <- diag(p)
Sigma[1:p_u, 1:p_u][lower.tri(Sigma[1:p_u, 1:p_u])] <- p_u_rho
Sigma[upper.tri(Sigma)] = t(Sigma)[upper.tri(Sigma)]

# Find the nearest Positive Define Correlation Matrix
Sigma <- Matrix::nearPD(Sigma, corr = TRUE)$mat

# Populate mean vector
mu <- rep(0, p)

# Get Samples
X <- MASS::mvrnorm(n = n, mu = mu, Sigma = Sigma)

# Generate Categorical Predictors -----------------------------------------
# All or nothing

# if(data_type == "cat"){
# Multinomial Logistic
# Coefficients (K-1 values (where K = number of categories of DV)
a <- c(1, 1, 1)
b <- c(ln(2), ln(2), ln(2))
b <- c(2, 4, 6)
K <- length(a)

X.cat <- sapply(1:p, function(j){
  # Calculation of denominator for probability calculation
  denom <- 1 + rowSums(sapply(1:K, 
                              function(i) {
                                exp(a[i] + X[, j] * b[i])
                              }
  )
  )
  
  # Calculating the matrix of probabilities for each category of the DV
  vProb <- cbind(1/denom, 
                 sapply(1:K, 
                        function(i) {
                          exp(a[i] + X[, j] * b[i])/denom
                        }
                 )
  )
  
  # Assigning the value one to maximum probability and zero for rest to 
  # get the appropriate choices for value of x
  DV_location = t(apply(vProb, 1, rmultinom, n = 1, size = 1)) 
  
  # Assing value of Categorical Variable
  DV <- apply(DV_location, 1, function(x) which(x==1))
  table(DV)
  # DV <- apply(vProb, 1, which.max)
  return(DV)
  # OR
  sort(runif(K, min(X[, j]), max(X[, j])))
  
})
# }

out <- GoodmanKruskal::GKtauDataframe(X.cat)
plot(out)

# PoisBinOrdNor -----------------------------------------------------------

n <- 5e2
P <- 50+1+1
K1 <- 1 # poisson
K2 <- 1 # binary
K3 <- P*.5 # ordinal
  K3_ncat <- 3
K4 <- P*.5 # numerical
K <- K1 + K2 + K3 + K4
lamvec <- sample(1:5, K1, replace = TRUE)
pbin <- round(runif(K2, .3, .7), 1)

pord <- list(c(0.3, .3, 0.4))
pord <- do.call(rep, args =list(x = pord, times = K3))

norm.mean <- rep(0, K4)
norm.var <- rep(1, K4)

# Define Correlation matrix and validate it
rho <- .5
Sigma <- matrix(rho, nrow = K, ncol = K)
diag(Sigma) <- 1

validation.specs(no.pois = K1, 
                 no.bin = K2,
                 no.ord = K3,
                 no.norm = K4,
                 corr.mat = Sigma,
                 lamvec = lamvec,
                 prop.vec.bin = pbin,
                 prop.vec.ord = pord,
                 nor.mean = norm.mean,
                 nor.var = norm.var)

# Define Intermediate mat
intmat <- intermat(no_pois = K1,
                   no_bin = K2,
                   no_ord = K3,
                   no_norm = K4,
                   corr_mat = Sigma,
                   lam_vec = lamvec,
                   prop_vec_bin = pbin,
                   prop_vec_ord = pord,
                   nor_mean = norm.mean,
                   nor_var = norm.var)

# Get sample
dat <- genPBONdata(n = n,
                   no_pois = K1,
                   no_bin = K2,
                   no_ord = K3,
                   no_norm = K4,
                   inter.mat = intmat,
                   lamvec = lamvec,
                   prop_vec_bin = pbin,
                   prop_vec_ord = pord,
                   nor.mean = norm.mean,
                   nor.var = norm.var)
round(cor(dat$data),2)

# Densities
apply(dat$data, 2, function(x){plot(density(x))})

# Using ordinal variables for everything ----------------------------------

set.seed(12345)
n <- 5e2
P <- 50
Pp <- list(num = .5, ord = .3, dum = .2)
K <- lapply(Pp, prod, P)
rho = .3 # desired correlation
do.call(sum, K) == P
Sigma <- matrix(rho, nrow = P, ncol = P)
diag(Sigma) <- 1

marginal_dum <- lapply(1:K$dum, function(x){
  p1 <- round(runif(1, .3, .7), 1)
})

marginal_ord <- lapply(1:K$ord, function(x){
  p1 <- round(runif(1, .1, .2), 2)
  p2 <- round(runif(1, .3, .4), 2)
  p3 <- round(runif(1, .5, .6), 2)
  p4 <- round(runif(1, .7, .8), 2)
  return(sort(c(p1, p2, p3, p4)))
})

marginal <- c(marginal_dum, marginal_ord)

# Validation steps
# Validate the marginal probabilities and correlations 
# validate.plist(marginal, no.ord = K$ord + K$dum) 
# valid.limits(marginal, 
#              no.ord = K$ord + K$dum , 
#              no.norm = K$num)
# validate.target.cormat(marginal, 
#                        Sigma,
#                        no.ord = K$ord + K$dum, 
#                        no.norm = K$num)

# Using package for correction of matrix
cmat = OrdNor::cmat.star(marginal, 
                         Sigma, 
                         no.ord = K$dum+K$ord, 
                         no.norm = K$num) 
plist = marginal
CorrMat = Sigma
no.ord = K$dum+K$ord
no.norm = K$num
# This is the part that goes wrong
OO = IntermediateOO(plist, CorrMat[1:no.ord, 1:no.ord])
ordcont(marginal = plist,
        Sigma = CorrMat[1:no.ord, 1:no.ord],
        epsilon = .1
        )
ON = IntermediateON(plist, CorrMat[(no.ord + 1):nrow(CorrMat), 
                                   1:no.ord])
NN = CorrMat[(no.ord + 1):ncol(CorrMat), (no.ord + 
                                            1):ncol(CorrMat)]
Sigma = cbind(rbind(OO, ON), rbind(t(ON), NN))
!is.positive.definite(Sigma)
if (!is.positive.definite(Sigma)) {
  warning("Intermediate correlation matrix is not positive definite. A nearPD function is applied.")
  Sigma = as.matrix(nearPD(Sigma, corr = TRUE, 
                           keepDiag = TRUE)$mat)
}
Sigma = (Sigma + t(Sigma))/2

# Obtain Sample
Y = genOrdNor(n = n,
              plist = marginal, 
              cmat.star = cmat, 
              mean.vec = rep(0, K$num), 
              sd.vec = rep(1, K$num), 
              no.ord = K$dum+K$ord, 
              no.norm = K$num)
head(Y)
(corY_p <- round(cor(Y), 2))

corY_p - corY_m

