all.perm <- function(n) {
  p <- matrix(1, ncol = 1)
  for (i in 2:n) {
    p <- pp <- cbind(p, i)
    v <- c(1:i, 1:(i - 1))
    for (j in 2:i) {
      v <- v[-1]
      p <- rbind(p, pp[, v[1:i]])
    }
  }
  p
}

Gauss_Tests <- function(X) {
  
  # INPUT
  # X: data vector or data matrix (assume more observations than variables)
  #
  # OUTPUT
  # p-values of normality tests: Shapiro-Wilk, Shapiro-Francia and Jarque-Bera
  
  require(nortest)
  require(tseries)
  
  X <- as.matrix(X)
  dims <- dim(X)
  
  if (dims[1]<dims[2]) {
    X <- t(X)
    dims <- dim(X)
  }
  
  m <- dims[1]
  if (m > 5000) {
    cat("get subsample of residuals to perform normality-test\n")
    subsamp <- sample(m,5000)
    X1 <- X[subsamp,]
  }
  else X1 <- X
  
  shapiro <- list(); shapfranc <- list(); jarque <- list();
  pval1 <- NULL; pval2 <- NULL; pval3 <- NULL
  
  for (i in 1:dims[2]) {
    # Shapiro-Wilk test
    shapiro[[i]] <- shapiro.test(X1[,i])
    pval1 <- c(pval1, unclass(shapiro[[i]])$p.value)
    # Shapiro-Francia test
    shapfranc[[i]] <- sf.test(X1[,i])
    pval2 <- c(pval2, unclass(shapfranc[[i]])$p.value)
    # Jarque-Bera test
    jarque[[i]] <- jarque.bera.test(X1[,i])
    pval3 <- c(pval3, unclass(jarque[[i]])$p.value)
  }
  
  ls <- list()
  ls$ShapiroWilk <- pval1
  ls$ShapiroFrancia <- pval2
  ls$JarqueBera <- pval3
  
  ls
  
}

iperm <- function( p ) {
  
  q <- array(0,c(1,length(p)))
  
  for (i in 1:length(p)) {
    ind <- which(p==i)
    q[i] <- ind[1]
  }
  
  q
  
}

nzdiagbruteforce <- function( W ) {
  
  #--------------------------------------------------------------------------
  # Try all row permutations, find best solution
  #--------------------------------------------------------------------------
  
  n <- nrow(W)
  
  bestval <- Inf;
  besti <- 0;
  allperms <- all.perm(n) 
  nperms <- nrow(allperms)
  
  for (i in 1:nperms) {
    Pr <- diag(n)
    Pr <- Pr[,allperms[i,]]
    Wtilde <- Pr %*% W
    c <- nzdiagscore(Wtilde)
    if (c<bestval) {
      bestWtilde <- Wtilde
      bestval <- c
      besti <- i
    }
  }
  
  res <- list()
  res$Wopt <- bestWtilde
  res$rowp <- allperms[besti,]
  res$rowp <- iperm(res$rowp)
  
  res
  
}

nzdiagscore <- function( W ) {
  
  res <- sum(1/diag(abs(W)))
  res
  
}

sltbruteforce <- function( B ) {
  
  #--------------------------------------------------------------------------
  # Try all row permutations, find best solution
  #--------------------------------------------------------------------------
  
  n <- nrow(B)
  
  bestval <- Inf;
  besti <- 0;
  allperms <- all.perm(n) 
  nperms <- nrow(allperms)
  
  for (i in 1:nperms) {
    Btilde <- B[allperms[i,],allperms[i,]]
    c <- sltscore(Btilde)
    if (c<bestval) {
      bestBtilde <- Btilde
      bestval <- c
      besti <- i
    }
  }
  
  res <- list()
  res$Bopt <- bestBtilde
  res$optperm <- allperms[besti,]
  
  res
  
}

sltscore <- function( B ) {
  
  s <- sum((B[upper.tri(B,diag=TRUE)])^2)
  s
  
}




