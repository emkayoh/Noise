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


# test data 
TestData <- function(x, ...) UseMethod("TestData")

TestData.default <- function(x, theta = NULL, sse = 0.03) {

	if (missing(theta)) 
		theta <- c(-1, 1, 1, -0.24, 6, 0.2, 13)
	if (length(theta) == 3) 
		theta <- c(theta, 0, 0, 0, 0)
	if (length(theta) == 5) 
		theta <- c(theta, 0, 0)

	Len <- length(x)
	Noise <- rnorm(Len, 0, sqrt(sse))
	Y <- theta[1] + theta[2] * exp(-x * theta[3]) + theta[4] * pmax(0, x - theta[5]) + theta[6] * 
		pmax(0, x - theta[7])

	tmp <- list(call = match.call(), time = x, thrs = (Y + Noise), resid = Noise, fit = Y, 
		thet = theta, sse = sse, val = var(Noise), data=paste('Test_data_', sse, sep=''))
	class(tmp) = "dark"
	tmp
}