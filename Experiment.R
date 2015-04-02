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

rm(list = ls())

# Numerical experiment to evaluate the effect of signal noise 
# on model selection and parameter estimates
source("~/GitHub/Noise/AICc.R")
source("~/GitHub/Noise/BestFit.R")
source("~/GitHub/Noise/BootDark.R")
source("~/GitHub/Noise/Declutter.R")
source("~/GitHub/Noise/GetTestData.R")
source("~/GitHub/Noise/Models2.R")
source("~/GitHub/Noise/ModelSelect.R")
source("~/GitHub/Noise/MultiStart.R")
source("~/GitHub/Noise/Start.R")
source("~/GitHub/Noise/TestData.R")

Draw = T
load(GetTestData(-0.24, 2)) # loads the 1000 test data sets in a list called List
Pts <- 1
Repeats <- 1000

for (ii in Pts) {
	tmp <- List[[ii]]

	x <- tmp$time
	y <- tmp$thrs


	if (Draw) {
		par(mfrow = c(2, 2), las = 1, bty = "n")
		plot(tmp$time, tmp$thrs)
	}

	P <- Start(tmp, Repeats)
	MSC <- ModelSelect(tmp, P)
	tmp1 <- BestFit(tmp, MSC, draw = Draw)

	tmp1 <- MultiStart_2(tmp1, repeats = as.integer(Repeats/5), draw = Draw)
	Out <- BootDark(tmp1, R = 2 * Repeats, graph = Draw)
}

# List[[length(List)+1]] <- list(Out)
# rm(tmp1, tmp)
# save(List, file=File)