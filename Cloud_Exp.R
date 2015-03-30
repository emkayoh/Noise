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


# script for cloud instance to evaluate the effect of noise on parameters 
rm(list = ls())
source("~/Dropbox/JMFK-PostDoc/Papers_In_Progress/Z_Analogue_DA/R_Code/LPvJK_Code/TestData.R")
# source('~/Dropbox/Dark/Dark1/TestData.R')
source("~/Dropbox/JMFK-PostDoc/Papers_In_Progress/Z_Analogue_DA/R_Code/LPvJK_Code/Declutter.R")
source("~/Dropbox/JMFK-PostDoc/Papers_In_Progress/Z_Analogue_DA/R_Code/LPvJK_Code/Models2.R")
source("~/Dropbox/JMFK-PostDoc/Papers_In_Progress/Z_Analogue_DA/R_Code/LPvJK_Code/ModelSelect.R")
source("~/Dropbox/JMFK-PostDoc/Papers_In_Progress/Z_Analogue_DA/R_Code/LPvJK_Code/Start.R")
source("~/Dropbox/JMFK-PostDoc/Papers_In_Progress/Z_Analogue_DA/R_Code/LPvJK_Code/BestFit.R")
source("~/Dropbox/JMFK-PostDoc/Papers_In_Progress/Z_Analogue_DA/R_Code/LPvJK_Code/MultiStart.R")
source("~/Dropbox/JMFK-PostDoc/Papers_In_Progress/Z_Analogue_DA/R_Code/LPvJK_Code/BootDark.R")
Repeats <- 1000 # this is the number of times 'multistart' runs and is a scalar for 'bootdark'
x <- seq(0, 20, by = 0.5)
sse = 0.2
Draw=F
tmp <- TestData(x, sse, thet = c(-1, 1, 1, -0.24, 6, 0.082, 18))
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
Out <- BootDark(tmp3, R = Repeats, graph = Draw)
Out