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

 rm(list=ls())
 
# Numerical experiment to evaluate the effect of signal noise 
# on model selection and parameter estimates
source('~/GitHub/Noise/AICc.R')
source('~/GitHub/Noise/BestFit.R')
source('~/GitHub/Noise/BootDark.R')
source('~/GitHub/Noise/Declutter.R')
source('~/GitHub/Noise/Models2.R')
source('~/GitHub/Noise/ModelSelect.R')
source('~/GitHub/Noise/MultiStart.R')
source('~/GitHub/Noise/Start.R')
source('~/GitHub/Noise/TestData.R')

Draw=F
List <- list()
Pts<-1:40
 
Repeats <- 200
x<- seq(0,20, by=0.5)

sse=0.01

for(ii in Pts){
theta<-c(-1.5, 1, 1, -0.24, 6, 0.06, 15)
tmp<- TestData(x, sse=sse, theta=theta)
tmp1<- Declutter(tmp)


x<-tmp1$time
y<-tmp1$thrs
if(Draw){
par(mfrow=c(2,2), las=1, bty='n')
plot(tmp$time, tmp$thrs)
points(tmp1$time, tmp1$thrs, pch=16, col=2)
}

P<-Start(tmp1,Repeats)
MSC<-ModelSelect(tmp1, P)
tmp2<-BestFit(tmp1, MSC,draw = Draw)



tmp3<-MultiStart_2(tmp2,repeats = as.integer(Repeats/5),draw=Draw)
Out<-BootDark(tmp3,R=2*Repeats, graph = Draw)
 List[[length(List)+1]] <- list(Out)
# save(List, file=File)
 }