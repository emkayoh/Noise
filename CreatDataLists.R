# Create a test data listing that can be used and re-used
rm(list=ls())
sse<-round(log10(seq(1.01,1.5,length.out=20)),3)
S2=-0.02*1:14
for(kk in 1:14){
theta<- c(-1.5, 1,1,S2[kk], 6, -S2[kk]/4, 15)
x<- seq(0,20, by=0.5)
File<- paste('data/Testdata_',sse,theta[4],'.Rdata', sep='')

source('~/GitHub/Noise/TestData.R')
for (jj in 1:20){
	List <- list()

for (ii in 1:1000){
 List[[length(List)+1]] <-TestData(x, theta = theta, sse=sse[jj])}
 save(file=File[jj], List)
 }
 }