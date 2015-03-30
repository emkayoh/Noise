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


Start<-function(obj, Reps){
  if(missing(Reps)) Reps=1500
  meshVec<- function(lB, uB, Reps) sample(seq(lB, uB, length.out=Reps), replace=F)
  x<- obj$time
  y<-obj$thrs
  xSp<- mean(range(x))
  ySp<- mean(range(y))
  
  # set.seed(1234)
  
  CT   <-  meshVec(ySp, max(y), Reps)
  CC   <-  meshVec(0,-ySp,Reps)
  Tau  <-  sample(CC, replace=F)
  S2   <-  meshVec(-0.6, 0,Reps)
  Alph <-  meshVec(0,xSp,Reps)
  S3   <- -sample(S2, replace=F)
  Beta <-  meshVec(xSp, max(x),Reps)

  
  cbind(CT, CC,Tau, S2, Alph, S3,Beta)
}