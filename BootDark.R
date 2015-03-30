# Copyright 2015 Dr Jeremiah MF Kelly

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at

    # http://www.apache.org/licenses/LICENSE-2.0

# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


BootDark <- function(obj, R, graph) {
  source('~/Dropbox/JMFK-PostDoc/Papers_In_Progress/Z_Analogue_DA/R_Code/LPvJK_Code/Models2.R')
  mFn<-c(1,1,P3,1,P5c,1,P7c)
  
  #library(MASS) # Bootstrap
  
  set.seed(1234)
  if (missing(R)) 
    R = 400
  if (missing(graph)) 
    graph = TRUE

  
  Parms = NULL
  qJK <- function(a) quantile(a, c(2.5, 50, 97.5)/100)
  
  if (is.list(obj)) {
  	out<-obj
    out$call<- NULL
    x <- unlist(obj$time)
    y <- unlist(obj$thrs)
    p = obj$opt
    resid <- obj$resid
    fit <- obj$fit
    val <- obj$val
    AIC <- obj$AIC
    if(is.null(obj$Pn)) Pn=7 else  Pn <- obj$Pn 
    Mod<- obj$Mod
  } else {
    x <- x
    Y <- y
  }
  
  if (is.null(p)) {
    p = obj$init
  }
  Fn = mFn[[Pn]]
  
  if (is.null(resid)) {
    O <- optim(p,Fn)
    while (O$con) {
      O <- optim(O$par, Fn)
    }
    p <- O$par
    fit <- Fn(p, x)
    resid <- (y - fit)
    val <- O$val
  }
  if (graph) {
    XL <- expression(bold(Time ~ (min)))
    YL <- expression(bold(Threshold ~ (LU)))
    plot(x, y, ylim = c(-4, 0), xlim = c(0, 20), xlab = XL, ylab = YL)
    lines(x, Fn(p, x), col = 2)
    
  }
  
  
  
  BS <- matrix(0, R, 7) #NULL
  for (ii in 1:R) {
    y <<- fit + sample(resid, replace = T)
    BS[ii, 1:Pn] <- optim(p, Fn)$par
  }
  
  BSq <- round(apply(BS, 2, qJK), 3)
  if (graph) {
    lines(x, Fn(BSq[1, ], x), col = 3)
    lines(x, Fn(BSq[3, ], x), col = 3)
  }
  
  
  Boot <- t(BSq)
  row.names(Boot) <- c("CT", "CC", "Tau", "S2", "Alpha", "S3", "Beta")
  
  valid<-as.integer((Boot[,1]*Boot[,3])>0)
  weight<-1/abs(Boot[,1]-Boot[,3])
  
  out$call=match.call()
  out$time=obj$time
  out$thrs=obj$thrs
  out$resid=resid
  out$fit=fit
  out$Bootstrap = Boot
  out$opt=obj$opt
  out$weight=weight
  out$valid = valid
  out$data=obj$data
  out$Mod=obj$Mod
  out$Pn=obj$Pn
  out$AIC=obj$AIC
  out$R2 <- 1-(var(resid)/var(obj$thrs))          
  class(out)='dark'
  return(out)
}