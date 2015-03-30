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


ModelSelect<- function(obj, P){
source('~/Dropbox/JMFK-PostDoc/Papers_In_Progress/Z_Analogue_DA/R_Code/LPvJK_Code/Models2.R')
source('~/Dropbox/JMFK-PostDoc/Papers_In_Progress/Z_Analogue_DA/R_Code/LPvJK_Code/AICc.R')

param<- matrix(0, 3,7)
idx<-c(3,5,7)
AIC<-numeric(7)
mFn<-c(1,1,P3,1,P5c,1,P7c)
jj=1
for(ii in idx){
  Fn=mFn[[ii]]

  Out<-apply(P,1,FUN = Fn)
  oP<-P[which(Out==min(Out)),]
  oPval<-Out[which(Out==min(Out))]
  Opt<-optim(oP[1:ii], Fn)
  while (Opt$con) Opt<-optim(Opt$par, Fn)
  
  init<-oP[1:Fn(1)$Pn]
  opt<-Opt$par[1:Fn(1)$Pn]
  param[jj, 1:Fn(1)$Pn]<-opt
  val<-Opt$val
  Pn<-Fn(1)$Pn
  Mod<-Fn(1)$Mod 
  # builds the dark object to pass to AICc
  zz<-obj
  zz$init=init
  zz$opt=opt
  zz$val=val
  zz$Pn=Pn
  zz$Mod=Mod
  AIC[ii]<- AICc(zz)
  
  jj=jj+1
}

list(AIC=AIC, param=param)
}

# X<- seq(0, max(x), length.out = 1000)
# lines(X, Fn(init,X))
# lines(X, Fn(opt,X), col=2)