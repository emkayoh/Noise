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


BestFit<- function(obj, MSC, draw){
  source('~/Dropbox/JMFK-PostDoc/Papers_In_Progress/Z_Analogue_DA/R_Code/LPvJK_Code/Models2.R')
  if(missing(draw)) draw=F
  mFn<-c(1,1,P3,1,P5c,1,P7c)
  idx<-c(3,5,7)
  obj$call=NULL
  
  Mod<- which(MSC$AIC==min(MSC$AIC))
  idx2 <- idx==Mod
  Fn=mFn[[Mod]]
  
  oP<-MSC$param[idx2,]
  
  Opt<-optim(oP, Fn)
  while (Opt$con) Opt<-optim(Opt$par, Fn)
  opt<-Opt$par
  
  if(draw){X<- seq(0, max(obj$time), length.out = 1000)
           lines(X, Fn(opt,X), col=2)
  }

  Res<-c(call=match.call(),obj, list(opt=opt, Mod=Fn(Mod)$Mod, Pn=Fn(Mod)$Pn, AIC=MSC$AIC, val=Opt$val))
  Y<- Fn(opt,obj$time) 

  Res$fit <- Y 
  Res$resid <- obj$thrs-Y
  Res$R2 <- 1-(var(obj$thrs-Y)/var(obj$thrs))
  
  class(Res)='dark'
    Res
}
