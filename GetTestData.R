GetTestData <- function(S2, Noise) {
	if (missing(S2)) 
		S2 = -0.24
		if(S2>=0){
			print('S2 must be negative and greater than -0.28')
			return(List=NULL)
			}
		S2 = as.integer((-100*S2)%/%4)*-0.04
	if (missing(Noise)) 
		Noise = 2
	if (Noise > 20) {
		print("Noise level outside range. The index must be between 1 and 20.")
	} else {
		S2 = as.integer((-100*S2)%/%4)*-0.04
		idx0 <- paste(S2, ".R", sep = "")
		FoI <- c("~/GitHub/Noise/data")
		tmp <- dir(FoI, pattern = "Test*")
		idx <- grep(idx0, tmp)
		FoI <- paste(FoI, "/", tmp[idx[Noise]], sep = "")
		FoI
	}
}
