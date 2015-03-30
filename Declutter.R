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


Declutter <- function(tmp, delta) {
	# function that takes data from early experiments where double points are recorded
	if (missing(delta)) 
		delta <- 2/60
	# button presses within 2s of each other
	
	if (class(tmp) == "dark") {
		if (is.null(tmp$data)) 
			tmp$data = "unknown"

		# idx <- c(diff(tmp$time),1) > delta    
		# this removes the first button press
		
		idx <- c(1, diff(tmp$time)) > delta
		# this removes the second press
		
		x <- tmp$time[idx]
		y <- tmp$thrs[idx]
		tmp$time=x
		tmp$thrs=y
		obj <- tmp
		class(obj) = "dark"
		return(obj)
	} else {
		idx <- c(0, diff(tmp[, 1])) > delta
		x <- tmp[idx, 1]
		y <- tmp[idx, 2]
		tmp$time=x
		tmp$thrs=y
		obj <- tmp
		class(obj) = "dark"
		return(obj)
	}
}
