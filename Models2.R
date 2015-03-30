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


# Models These are the models available in the dark package, some are trivial 
# (P2) while others are more complex (P9c). These models have more than one 
# method. They can return the SSE for a particular parameter 'a', they can return
# the model name and parameter count, and they can return the fitted values for
# a parameter vector 'a'|X.

############### Linear
P2<-function(a,X){
  
  #   flag (X) is an array at which to calculate f(X), if missing then the 
  #   function calls the outer environment and calculates the SSE for 'a'. If 'a'
  #   is boolean then a description of the model is returned. 
  
  if (missing(X)){
    if(length(a)==1){
      list(Pn=2L, Mod='P2')
    }else{
      sum((y-(a[1]+a[2]*x))^2)
    }
  }else{
    a[1]+a[2]*X 
  }
}
############### Exponential decay
P3<-function(a,X){
  
  #   flag (X) is an array at which to calculate f(X), if missing then the 
  #   function calls the outer environment and calculates the SSE for 'a'. If 'a'
  #   is boolean then a description of the model is returned. 
  
  if (missing(X)){
    if(length(a)==1){
      list(Pn=3, Mod='P3')
    }else{
      sum((y-(a[1]+a[2]*exp(-x*a[3])))^2)
    }
  }else{
    a[1]+a[2]*exp(-X*a[3])
  }
  
}
############### Exponential growth
P3g<-function(a,X){  
  #   flag (X) is an array at which to calculate f(X), if missing then the 
  #   function calls the outer environment and calculates the SSE for 'a'. If 'a'
  #   is boolean then a description of the model is returned. 
  
  if (missing(X)){  
  if(length(a)==1){
    list(Pn=3L, Mod='P3g')
  }else{
    sum((y-(a[1]+a[2]*exp(x*a[3])))^2)
  }  
  }else{
    a[1]+a[2]*exp(X*a[3])
  }
}	
############### Hill slope
P4<- function(a,X){	
  
  #   flag (X) is an array at which to calculate f(X), if missing then the 
  #   function calls the outer environment and calculates the SSE for 'a'. If 'a'
  #   is boolean then a description of the model is returned. 
  
  if (missing(X)){ 
    if(length(a)==1){
      list(Pn=4L, Mod='P4')
    }else{
      Yest<-a[1]+(a[2]-a[1])/(1+((10^a[3])/10^x)^a[4])
      sum((y-Yest)^2)
    } 
  }else{
    a[1]+(a[2]-a[1])/(1+((10^a[3])/10^X)^a[4])
  }
  
  # a[1]=bottom
  # a[2]=top
  # a[3]=ec50
  # a[4]=slope
}
############### Five Param Exp + Linear		
P5<-function (a,X){
  
  #   flag (X) is an array at which to calculate f(X), if missing then the 
  #   function calls the outer environment and calculates the SSE for 'a'. If 'a'
  #   is boolean then a description of the model is returned. 
  
  if (missing(X)){   
    if(length(a)==1){
      list(Pn=5L, Mod='P5')
    }else{
      Yest<-a[1]+a[2]*exp(-x*a[3])+ a[4]*ifelse(x<a[5],0, x-a[5]) 
      sum((y-Yest)^2)
    }	 
  }else{
    a[1]+a[2]*exp(-X*a[3])+ a[4]*ifelse(X<a[5],0, X-a[5]) 
  }
}
############### Six Param Exp + Exp
# taking the switch outside the exponent is nonsensical
P6<- function(a,X){
  # uses a step function to switch between systems flag (X) is an array at which
  # to calculate f(X), if missing then the function calls the outer environment
  # and calculates the SSE for 'a'. If 'a' is boolean then a description of the
  # model is returned.
  
  if (missing(X)){ 
    if(length(a)==1){
      list(Pn=6L, Mod='P6')
    }else{
      Yest<- a[1]+a[2]*exp(-x*a[3]) +a[5]*exp(-ifelse(x<a[4],0, x-a[4])*a[6])
      sum((y-Yest)^2)
    }   
  }else{
    a[1]+a[2]*exp(-X*a[3]) +a[5]*exp(-ifelse(X<a[4],0, X-a[4])*a[6])
  }
}
############### Bi linear step 
PBls<- function(a,X){
  
  #   flag (X) is an array at which to calculate f(X), if missing then the 
  #   function calls the outer environment and calculates the SSE for 'a'. If 'a'
  #   is boolean then a description of the model is returned. 
  
  if (missing(X)){
    if(length(a)==1){
      list(Pn=4L, Mod='PBls')
    }else{
      Yest<- a[1]+a[2]*x +a[3]*ifelse(x<a[4],0, x-a[4])		
      sum((y-Yest)^2)
    }}else{
      a[1]+a[2]*X +a[3]*ifelse(X<a[4],0, X-a[4])
      
    }
}
############### Polynomial 2 
Pn2<- function(a,X){
  #   flag (X) is an array at which to calculate f(X), if missing then the 
  #   function calls the outer environment and calculates the SSE for 'a'. If 'a'
  #   is boolean then a description of the model is returned. 
  
  if(missing(X)){
    if(length(a)==1){
      list(Pn=3L, Mod='Pn2')
    }else{
      Yest<- a[1]+a[2]*x +a[3]*x^2	
      sum((y-Yest)^2)
    }		}else{
      a[1]+a[2]*X +a[3]*X^2
    }
}
############### Seven Param Exp + Bilinear
# uses a step function to switch between systems
P7<-function (a,X){
  # uses a step function to switch between systems flag (X) is an array at which
  # to calculate f(X), if missing then the function calls the outer environment
  # and calculates the SSE for 'a'. If 'a' is boolean then a description of the
  # model is returned.
  if(missing(X)){
    if(length(a)==1){
      list(Pn=7L,Mod="P7")
    }else{
      Yest<-a[1]+a[2]*exp(-x*a[3])+ a[4]*ifelse(x<a[5],0, x-a[5]) + a[6]*ifelse(x<a[7],0, x-a[7])
      sum((y-Yest)^2)
    }  }else{
      a[1]+a[2]*exp(-X*a[3])+ a[4]*ifelse(X<a[5],0, X-a[5]) + a[6]*ifelse(X<a[7],0, X-a[7])
    }
}
#########################################################################		
############### Continuous
H<-function(x,k=100,t){
  # x is the measured time
  # k is the transition constant, set arbitrarily high
  # t is the time at which the transition occurs
  round(1/(1+exp(-2*k*(x-t))),1)
}
# Five Param Exp + Bilinear and two fixed transition rates
P5c<-function (a,X){
  H<-function(x,k=100,t){
    # x is the measured time
    # k is the transition constant, set arbitrarily high
    # t is the time at which the transition occurs
    round(1/(1+exp(-2*k*(x-t))),1)
  }
  # to calculate f(X), if missing then the function calls the outer environment
  # and calculates the SSE for 'a'. If 'a' is boolean then a description of the
  # model is returned.
  if(missing(X)){
    if(length(a)==1){
      list(Pn=5L,Mod='P5c')
    }else{
      Yest<-a[1]+a[2]*exp(-x*a[3])+ a[4]*(x-a[5])*H(x,10,a[5]) 
      sum((y-Yest)^2)
    }  }else{
      a[1]+a[2]*exp(-X*a[3])+ a[4]*(X-a[5])*H(X,10,a[5])
    }
}
# Seven Param Exp + Bilinear and two fixed transition rates
P7c<-function (a,X){
  H<-function(x,k=100,t){
    # x is the measured time
    # k is the transition constant, set arbitrarily high
    # t is the time at which the transition occurs
    round(1/(1+exp(-2*k*(x-t))),1)
  }
  # to calculate f(X), if missing then the function calls the outer environment
  # and calculates the SSE for 'a'. If 'a' is boolean then a description of the
  # model is returned.
  if(missing(X)){
    if(length(a)==1){
      list(Pn=7L,Mod='P7c')
    }else{
      Yest<-a[1]+a[2]*exp(-x*a[3])+ a[4]*(x-a[5])*H(x,10,a[5]) + a[6]*(x-a[7])*H(x,10,a[7])
      sum((y-Yest)^2)
    }  }else{
      a[1]+a[2]*exp(-X*a[3])+ a[4]*(X-a[5])*H(X,10,a[5]) + a[6]*(X-a[7])*H(X,10,a[7])
    }
}
# Seven Param Exp + Bilinear and two transition rates
P9c<-function (a,X){
  H<-function(x,k=100,t){
    # x is the measured time
    # k is the transition constant, set arbitrarily high
    # t is the time at which the transition occurs
    round(1/(1+exp(-2*k*(x-t))),1)
  }
  # to calculate f(X), if missing then the function calls the outer environment
  # and calculates the SSE for 'a'. If 'a' is boolean then a description of the
  # model is returned.
  if(missing(X)){
    if(length(a)==1){
      list(Pn=9L,Mod='P9c')
    }else{
      Yest<-a[1]+a[2]*exp(-x*a[3])+ a[4]*(x-a[5])*H(x,a[8],a[5]) + a[6]*(x-a[7])*H(x,a[9],a[7])
      sum((y-Yest)^2)
    }  }else{
      
      a[1]+a[2]*exp(-X*a[3])+ a[4]*(X-a[5])*H(X,a[8],a[5]) + a[6]*(X-a[7])*H(X,a[9],a[7])
      
    }
}