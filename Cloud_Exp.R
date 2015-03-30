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

# This is only needed for local machine 

setwd('~/GitHub/Noise')

# script for cloud instance to evaluate the effect of noise on parameters 
rm(list = ls())

# initialise other scripts
source("TestData.R")
source("Declutter.R")
source("Models2.R")
source("ModelSelect.R")
source("Start.R")
source("BestFit.R")
source("MultiStart.R")
source("BootDark.R")

# Initialise values

Repeats <- 10 
# this is the number of times 'multistart' runs and is a scalar for 'bootdark'
x <- seq(0, 20, by = 0.5)
# The times at which thresholds are generated
Draw=F
# Dont create a plot for each case. 
thet = c(-1, 1, 1, -0.24, 6, 0.082, 15)
Out<-NULL

sse = 0.2
for(kk in 1:10){
tmp <- TestData(x, sse, thet=thet)
tmp1 <- Declutter(tmp)

# tmp1 is the data in dark object form

x <- tmp1$time
y <- tmp1$thrs

if (Draw){
 par(mfrow = c(2, 2), las = 1, bty = "n")
 plot(tmp$time, tmp$thrs)
 points(tmp1$time, tmp1$thrs, pch = 16, col = 2)
 lines(tmp1$time, tmp1$fit, col = 2)
}

P <- Start(tmp1, Repeats)
MSC <- ModelSelect(tmp1, P)
tmp2 <- BestFit(tmp1, MSC, draw = Draw)
tmp3 <- MultiStart_2(tmp2, repeats = as.integer(Repeats/2.5), draw = Draw)
tmp4 <- BootDark(tmp3, R = Repeats, graph = Draw)
Out<-c(Out, tmp4)
}
save(Out, file=paste('data/Out_',sse,'.Rdata', sep=''))

