require(urca)
require(mcompanion)
################### IBM Listing Exchanges VS Other Exchanges ######################
df.exIBM<-read.csv('df.exIBM.csv', sep = ',', header = TRUE)
# I lag variables below for vecm estimation
data<-df.exIBM[,2:5] 
data.t<-data[11:57137,]
data.L10<-data[10:57136,]
data.L9<-data[9:57135,]
data.L8<-data[8:57134,]
data.L7<-data[7:57133,]
data.L6<-data[6:57132,]
data.L5<-data[5:57131,]
data.L4<-data[4:57130,]
data.L3<-data[3:57129,]
data.L2<-data[2:57128,]
data.L1<-data[1:57127,]
colnames(data.L1)<-c("NBBotherL1","NBOotherL1","NBBlistL1","NBOlistL1")
colnames(data.L2)<-c("NBBotherL2","NBOotherL2","NBBlistL2","NBOlistL2")
colnames(data.L3)<-c("NBBotherL3","NBOotherL3","NBBlistL3","NBOlistL3")
colnames(data.L4)<-c("NBBotherL4","NBOotherL4","NBBlistL4","NBOlistL4")
colnames(data.L5)<-c("NBBotherL5","NBOotherL5","NBBlistL5","NBOlistL5")
colnames(data.L6)<-c("NBBotherL6","NBOotherL6","NBBlistL6","NBOlistL6")
colnames(data.L7)<-c("NBBotherL7","NBOotherL7","NBBlistL7","NBOlistL7")
colnames(data.L8)<-c("NBBotherL8","NBOotherL8","NBBlistL8","NBOlistL8")
colnames(data.L9)<-c("NBBotherL9","NBOotherL9","NBBlistL9","NBOlistL9")
colnames(data.L10)<-c("NBBotherL10","NBOotherL10","NBBlistL10","NBOlistL10")
dataset.prices<-cbind(data.t,data.L1,data.L2,data.L3,data.L4,data.L5,data.L6,data.L7,data.L8,data.L9,data.L10)
diff.data<-diff(as.matrix(dataset.prices), lag = 1, differences = 1)
##here I create the cointegrating relationships given the a priori decided cointegrating
##vectors
b1<-dataset.prices[,8]-dataset.prices[,7]
b2<-dataset.prices[,8]-dataset.prices[,6]
b3<-dataset.prices[,8]-dataset.prices[,5]
Bp<-cbind(b1,b2,b3)
Bp<-Bp[-57127,]
####################################
df.vecm<-cbind(diff.data,Bp)
df.vecm<-as.data.frame(df.vecm) #dataset used for vecm estimation
#VECM ESTIMATION EQUATION BY EQUATION LEAST SQUARES
fit1<-lm(NBBother~ . + 0, data = df.vecm[,-c(2,3,4)])
fit2<-lm(NBOother~ . + 0, data = df.vecm[,-c(1,3,4)])
fit3<-lm(NBBlist~ . + 0, data = df.vecm[,-c(1,2,4)])
fit4<-lm(NBOlist~ . + 0, data = df.vecm[,-c(1,2,3)])

res1<-resid(fit1)
res2<-resid(fit2)
res3<-resid(fit3)
res4<-resid(fit4)
resmat<-cbind(res1,res2,res3,res4)

Sigma.res<-cov(resmat)
coeff.matrix<-rbind(fit1$coefficients, fit2$coefficients, fit3$coefficients, fit4$coefficients)
Sigma.chol<-t(chol(Sigma.res))
alpha<-coeff.matrix[,41:43]
Beta<-matrix(c(1,-1,0,0,1,0,-1,0,1,0,0,-1),nrow = 4,ncol = 3)
orth.alpha<-null_complement(alpha)
orth.Beta<-null_complement(Beta)
psi1<-coeff.matrix[,1:4]
psi2<-coeff.matrix[,5:8]
psi3<-coeff.matrix[,9:12]
psi4<-coeff.matrix[,13:16]
psi5<-coeff.matrix[,17:20]
psi6<-coeff.matrix[,21:24]
psi7<-coeff.matrix[,25:28]
psi8<-coeff.matrix[,29:32]
psi9<-coeff.matrix[,33:36]
psi10<-coeff.matrix[,37:40]
PSI<-psi1 + psi2 + psi3 + psi4 + psi5 + psi6 + psi7 + psi8 + psi9 + psi10
I<-diag(4)
H<-solve(t(orth.alpha)%*%(I-PSI)%*%orth.Beta)
C1<-orth.Beta%*%H%*%t(orth.alpha)
rw.var<-t(C1[1,])%*%Sigma.res%*%C1[1,]
shares<-t(C1[1,])%*%Sigma.chol
shares2<-shares^2
IS1<-c(shares2[1]/rw.var,shares2[2]/rw.var,shares2[3]/rw.var,shares2[4]/rw.var)
### permuting rows and columns
perm1.resmat<-resmat[,c(4,3,2,1)]
sigma.perm1<-cov(perm1.resmat)
chol.perm1<-t(chol(sigma.perm1))
C1.perm1<-C1[,c(4,3,2,1)]
shares.perm1<-t(C1.perm1[1,])%*%chol.perm1
shares2.perm1<-shares.perm1^2
IS2<-c(shares2.perm1[1]/rw.var,shares2.perm1[2]/rw.var,shares2.perm1[3]/rw.var,shares2.perm1[4]/rw.var)

# ICA-Lingam
require(fastICA)
X<-as.matrix(resmat)
dims <- ncol(X)
icaout<-fastICA(X, ncol(X), tol=1e-14)
W <- t((icaout$K) %*% (icaout$W)) # un-mixing matrix
print(W)
#A <- solve(W) # mixing matrix
ICs <- icaout$S
print('Test for Gaussianity of the independent components')
print(Gauss_Tests(ICs))
# small dimensionality, hence use brutal heuristic method
temp <- nzdiagbruteforce( W )
Wp <- temp$Wopt
rowp <- temp$rowp
# Divide each row of Wp by the diagonal element
estdisturbancestd <- 1/diag(abs(Wp))
Wp <- Wp/diag(Wp)
# Compute corresponding B
Best <- diag(4)-Wp
temp <- sltbruteforce( Best )
Bestcausal <- temp$Bopt
causalperm <- temp$optperm
# Here, we report how lower triangular the result was
percentinupper <- sltscore(Bestcausal)/sum(Bestcausal^2)
print(Bestcausal)
# Set the upper triangular to zero
Bestcausal[upper.tri(Bestcausal,diag=FALSE)] <- 0
# Finally, permute 'Bestcausal' back to the original variable
# ordering and rename all the variables to the way we defined them
# in the function definition
icausal <- iperm(causalperm);
res <- list()
res$B <- Bestcausal[icausal, icausal];
res$stde <- estdisturbancestd
res$k <- causalperm
res$W <- W
res$ICs <- ICs
# Return the result
res$B  

# Unique Choleski IS computation using the causal order identified with ICA-Lingam
Gamma0<-diag(4)-Bestcausal
causal.order<-resmat[,c(3,4,2,1)] ## I see the causal order from res$B and I set the column order accordingly
sigma.causal<-cov(causal.order)
causal.chol<-t(chol(sigma.causal))
C1.order<-C1[,c(3,4,2,1)]
shares.order<-t(C1.order[1,])%*%causal.chol
shares2.order<-shares.order^2
prova<-c(shares2.order[1]/rw.var,shares2.order[2]/rw.var,shares2.order[3]/rw.var,shares2.order[4]/rw.var)

# -------------- BOOTSTRAP FOR TESTING THE IMPACT MATRIX COEFFICIENTS
multiB0<-list()
for (i in 1:100) {
  Sample<-resmat[sample(nrow(resmat),30000,replace = TRUE),]
  X<-as.matrix(Sample)
  dims <- ncol(X)
  icaout<-fastICA(X, ncol(X), tol=1e-14)
  W <- t((icaout$K) %*% (icaout$W)) # un-mixing matrix
  print(W)
  #A <- solve(W) # mixing matrix
  ICs <- icaout$S
  # small dimensionality, hence use brutal heuristic method
  temp <- nzdiagbruteforce( W )
  Wp <- temp$Wopt
  rowp <- temp$rowp
  # Divide each row of Wp by the diagonal element
  estdisturbancestd <- 1/diag(abs(Wp))
  Wp <- Wp/diag(Wp)
  # Compute corresponding B
  Best <- diag(4)-Wp
  temp <- sltbruteforce( Best )
  Bestcausal <- temp$Bopt
  causalperm <- temp$optperm
  # Here, we report how lower triangular the result was
  percentinupper <- sltscore(Bestcausal)/sum(Bestcausal^2)
  # Set the upper triangular to zero
  Bestcausal[upper.tri(Bestcausal,diag=FALSE)] <- 0
  # Finally, permute 'Bestcausal' back to the original variable
  # ordering and rename all the variables to the way we defined them
  # in the function definition
  icausal <- iperm(causalperm);
  res <- list()
  res$B <- Bestcausal[icausal, icausal];
  multiB0[[i]]<-res$B
}

B0tot<-matrix(c(replicate(16,0)),ncol = 4,nrow = 4)
for (i in 1:100) {
  B0tot<-B0tot + multiB0[[i]]
}
B0mean<-B0tot/100

multiB0.vector<-unlist(multiB0)
a<-matrix(c(1:1600),nrow = 100,ncol = 16, byrow = TRUE)
Sds<-c(replicate(16,0))
for (i in 1:16) {
  Sds[i]<-sd(multiB0.vector[a[,i]])
}
B0sd<-matrix(Sds,nrow=4,ncol=4)

B0test<-B0mean/B0sd
B0mean
B0sd
B0test


# LANNE approach ----- relaxing the triangular assumption
library(vars)
library(svars)

df.exIBM<-read.csv('df.exIBM.csv', sep = ',', header = TRUE)
data<-df.exIBM[19800:45000,2:5] 
data.t<-data[11:25201,]
data.L1<-data[10:25200,]
colnames(data.L1)<-c("NBBotherL1","NBOotherL1","NBBlistL1","NBOlistL1")
dataset.prices<-cbind(data.t,data.L1)
# I decide the cointegrating relationships 
b1<-dataset.prices[,8]-dataset.prices[,7]
b2<-dataset.prices[,8]-dataset.prices[,6]
b3<-dataset.prices[,8]-dataset.prices[,5]
Bp<-as.data.frame(cbind(b1,b2,b3))
Bp<-Bp[-25191,]
diff.data<-diff(as.matrix(data.t), lag = 1, differences = 1)
data.vecm<-as.data.frame(cbind(diff.data,Bp))

fit<-VAR(data.vecm[,1:4], p=10, type = "none", exogen = data.vecm[,5:7])
Bcoef(fit)
id<-id.ngml(fit)
B0ex1s<-id$B_stand
B0ex1s.sd<-id$B_stand_SE
B0ex1s/B0ex1s.sd

shares<-t(C1[1,])%*%B0ex1s
shares2<-shares^2
rw.var<-t(C1[1,])%*%(B0ex1s%*%t(B0ex1s))%*%C1[1,]
IS.Lanne.ex1s<-c(shares2[1]/rw.var,shares2[2]/rw.var,shares2[3]/rw.var,shares2[4]/rw.var)
IS.Lanne.ex1s

getAnywhere(id.ngml())


#Graphs

residui1<-resmat[sample(nrow(resmat),500),1]
residui2<-resmat[sample(nrow(resmat),500),2]
residui3<-resmat[sample(nrow(resmat),500),3]
residui4<-resmat[sample(nrow(resmat),500),4]

par(mfrow=c(2,2))
par(mar = c(2, 2, 1, 2.5))
par(mgp = c(1.5, 0.5, 0))
par(oma = c(2, 2, 2, 0))
qqnorm(residui1, xlab = "",ylab="", main = "Bid(listing)", cex.main=1)
qqline(residui1,col="darkred",lwd=2)

qqnorm(residui2, xlab = "",ylab="", main = "Ask(listing)",cex.main=1)
qqline(residui2,col="darkred",lwd=2)

qqnorm(residui3, xlab = "",ylab="", main = "Bid(others)",cex.main=1)
qqline(residui3,col="darkred",lwd=3)

qqnorm(residui4, xlab = "",ylab="", main = "Ask(others)",cex.main=1)
qqline(residui4,col="darkred",lwd=2)
mtext("(b)",outer = TRUE,cex = 1)
