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


MultiStart_2 <- function(P, repeats, draw, spread, debug) {

	source("~/Dropbox/JMFK-PostDoc/Papers_In_Progress/Z_Analogue_DA/R_Code/LPvJK_Code/Models2.R")
	mFn <- c(1, 1, P3, 1, P5c, 1, P7c)


	if (missing(repeats)) 
		repeats = 400
	if (missing(draw)) 
		draw = FALSE
	if (missing(spread)) 
		spread = 0.15
	if (missing(debug)) 
		debug = F

		
	if (debug) 
		print("+++ missing values assigned OK")

	resid = NULL
	fit = NULL
	val = NULL
	Pn = NULL
	AIC = NULL
	if (is.list(P)) {
		Res <- P
		Res$call = NULL
		x <- P$time
		y <- P$thrs
		p <- P$opt
		val <- P$val
		if (is.null(P$opt)) 
			p = P$init
		Pn <- P$Pn
		if (is.null(P$Pn)) 
			Pn = 7
		AIC <- P$AIC[1:7]
	} else {
		x <<- x
		y <<- y
		p <- P
	}

	if (debug) 
		print("+++ object processed OK")
	Fn <- mFn[[Pn]]

	if (debug) 
		print(paste("+++ Using the ", Pn, " parameter model ", sep = ""))

	OptJK <- function(a) {

		tmp <- numeric(9)
		# two iterations of the optim fn could allow 1000 cycles, will compare later
		X = optim(a, Fn)
		X = optim(X$par, Fn)
		X = optim(X$par, Fn)
		# X = optim(X$par, Fn)
		tmp[1:Pn] = X$par
		tmp[8] = X$val
		tmp[9] = X$con
		tmp
	}

	Par <- matrix(p * rnorm(7 * repeats, 1, spread), 7, repeats)

	O <- t(apply(Par, 2, OptJK))


	input <- numeric(9)
	input[1:7] = p
	input[8] = val
	input[9] = 0
	O <- rbind(input, O)
	if (debug) 
		print(head(O))

	Test <- sum(O[, 9] == 0)
	if (debug) 
		print(paste("+++ the test has boolean value ", Test, sep = ""))
	if (Test) {
		idx <- O[, 9] == 0
		O <- O[idx, ]
	}
	if (debug) 
		print(head(idx))

	if (length(O) != 9) {
		idx <- order(O[, 8])
		O <- O[idx, ]
		val <- O[1, 8]
		p <- O[1, 1:Pn]
	}else{
			val <- O[8]
			p <- O[1:Pn]
			}
	if (length(O) == 9) 
		Res$warning <- "+++ None of the jittered values converged"

	if (debug) 
		print("+++ Ordered index made")


	val = val[[1]]

	fit <- Fn(p, x)
	resid <- (y - fit)

	if (draw) {
		plot(x, y)
		lines(x, fit)
	}

	#return(list(Out=O[1:5,], P=P))
	Res$call <- match.call()
	Res$opt = p
	Res$time = x
	Res$thrs = y
	Res$resid = resid
	Res$fit = fit
	Res$val = val
	Res$data = P$data
	Res$Mod = P$Mod
	Res$Pn = P$Pn
	Res$AIC = AIC
	Res$R2 <- 1 - (var(resid)/var(y))

	if (debug) 
		Res$O <- O
	class(Res) = "dark"
	return(Res)
}
