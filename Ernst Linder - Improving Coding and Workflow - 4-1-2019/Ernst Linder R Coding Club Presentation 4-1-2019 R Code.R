#######################################################
#######  Session 4   Looping and Writing Functions
########################################################


options(digits = 8)
##########################################################################
# Conditional statements  with if-else
#  Careful, make sure you don't jump to new lines instead block of statements
##########################################################################
a = 10  ## try a = 30
a = 30

if (a < 20) {
	cat("a is less than 20") }  else  { 
	cat("a is not less than 20") 
	}

#  The following also works: (note:  {..} not needed if there is only ONE expression)

if (a < 20) cat("a is less than 20") else cat("a is not less than 20") 

y = 15:24

yy = ifelse(y<20,print("yes"),print("no"))
yy

##########################################################################
# Conditional statements  with switch (one of several possibilities)
##########################################################################

# The following command produces the square root if x>0, -sqrt(-x)  if x < 0,
#  and a message saying "x is zero" if x = 0. 

## choose one x of the following
x = 4 ;  x = 0  ;  x = -4

switch(as.character(sign(x)), "1" = sqrt(x), 
    "-1" = print(-sqrt(-x)), "0" = cat("x is zero","\n"))
#  how about   x = -3:5   Does not work for vectors  

#################################################################
### Additional Topics:   Subsetting,  classes of objects
#################################################################

dat[dat$year %in% c(1,2,4,8,9) ,c("soil","water" )  ]   ## matrix  A[3,5]

dat[is.na(dat)] <- 0 

aa <- c("a","b","d34")
class(aa)

#vectors
xx = c(1:5, 9, 23.6)
zz = 1:5

# arrays:   higher dimension configuration of objects of one class
#  2-d arrays == matrices  (rows and columns)

A = matrix(1:15,nrow=5,ncol=3)
A = array(1:36, dim=c(2,3,6))

data(mtcars)
dim(mtcars)

###########################################################################
## Regression / ANOVA Diagnostics:  Residual plots
###########################################################################

M1 = lm(mpg ~., data = mtcars)
par(mfrow=c(2,2))
plot(M1)

summary(M1)

###################################################################
## Model Selection with a stepwise "backwards elimination"
###################################################################

M2 = step(M1)
summary(M2)
par(mfrow=c(2,2))
plot(M2)



##########################################################################
#### for loops  ####
##########################################################################

## in large data you may have to run things recursively
x = runif(10000)  #  calculate the mean in a loop (recursively)
n = length(x)
#  does the mean function work?
mean(x)
mean = 0
for (i in 1:n) {mean = mean + x[i]/n
                print(i)}
mean   #  of course same as mean(x)


#  another version
avg = 0 
for (mm in 1:length(x)) avg = avg + x[mm]/length(x)
avg

##########################################################################
####  while loops   ###############
##########################################################################

###  sum all non-negative integers until the sum is a million
sumint = 0 ;   # initializing
int = 0     # start with first integer
while (sumint < 1000000) {
       int = int + 1
      sumint = sumint + int	
}
int
sumint   # shows the first sum over a million
    

##########################################################################
###  Repeat loops
##########################################################################

sumint = 0 ;
int = 0
repeat{
	int = int + 1
  sumint = sumint + int
  if (sumint > 1000000) break
}
int
sumint

  
##########################################################################
### Write my own distance function for a set of two locations
##########################################################################

pairdist = function(x,y)  { 
  m = nrow(x); n = nrow(y)
	dd = matrix(nrow=m,ncol=n)
	for (i in 1:m)  { for (j in 1:n) 
		dd[i,j] = sqrt( (x[i,1]-y[j,1])^2 + (x[i,2]-y[j,2])^2 ) }
		return(dd)   #  could have just written:   dd
	}

save(pairdist,file="pairdist.fsave")

#  Lets try it: 3 x-points, 2 y-points

x1 = c(0,0); x2 = c(0,1); x3 = c(1,0)
y1 = c(1/2,1/2); y2 = c(1,1)
x=rbind(x1,x2,x3)
y=rbind(y1,y2)

all = rbind(x,y)
plot(all,type="n",xlab="s.x",ylab="s.y",asp=1) # sets up the graph
text(x,label = c("x1","x2","x3") )
text(y,label=c("y1","y2") )

pairdist(x,y)  #  or
ddd = pairdist(x,y)
ddd

##########################################################################
### clear the workspace;  Sourcing a function  (could also be a URL  or a connection)
##########################################################################

rm(list=ls())
#  source("/Users/localadmin/Documents/1 - Teaching Current/1 - M796 Intro to R/Course Notes 2012/makefunction.pairdist.R")
# source("E:/1 - M796 R Intro 2013/Scripts/pairdist.fsave")

wd = getwd()
load(paste(wd,"pairdist.fsave",sep="/"))

##########################################################################
### Creating a Plotting Function
##########################################################################

plotbinom = function(n,p){   #  n=15 ; p=.3
  x = 0:n
  plot(x,dbinom(x,size=n,prob=p),type="h",ylab="p(x)",lwd=6,col="red")
  title(paste("Binomial Distribution with n =", n ,", p =",p ),cex.main=0.9 )
}

par(mfrow=c(1,2))
plotbinom(15,.3)
plotbinom(15,.7)

##########################################################################
#### Pairwise distances and plotting
##########################################################################

pairdistplot = function(x,y,...)  { 
  m = nrow(x); n = nrow(y)
	dd = matrix(nrow=m,ncol=n)
	for (i in 1:m)  { for (j in 1:n) 
		dd[i,j] = sqrt( (x[i,1]-y[j,1])^2 + (x[i,2]-y[j,2])^2 ) }
		print(dd)
		## make plot
		all = (rbind(x,y))
		plot(all,type="n",xlab="x",ylab="y",asp=1,...)  # setup the graph
		text(x,label = paste("x",1:m,sep=""),...)
		text(y,label = paste("y",1:n,sep=""),...)
	}

x1 = c(0,0); x2 = c(0,1); x3 = c(1,0)
y1 = c(1/2,1/2); y2 = c(1,1)
a=rbind(x1,x2,x3)
b=rbind(y1,y2)

pairdistplot(a,b,col="red",main = "pairwise distances")

###
### separate colors for x and y points:  default same color black
###


pairdistplot = function(x,y,colx=1,coly=1)  { m = nrow(x); n = nrow(y)
	dd = matrix(nrow=m,ncol=n)
	for (i in 1:m)  { for (j in 1:n) 
		dd[i,j] = sqrt( (x[i,1]-y[j,1])^2 + (x[i,2]-y[j,2])^2 ) }
		print(dd)
		## make plot
		all = (rbind(x,y))
		plot(all,type="n",xlab="",ylab="")  # setup the graph
		text(x,label = paste("x",1:m,sep=""),col=colx)
		text(y,label = paste("y",1:n,sep=""),col=coly)
            dd 
	}
pairdistplot(a,b)
myd = pairdistplot(a,b,2,3)
myd

pairdistplot(a,b,"red","blue")

##########################################################################
### A function where more than one object is returned;  just provide a list
##########################################################################

pairdistplot = function(x,y,colx,coly)  { m = nrow(x); n = nrow(y)
	dd = matrix(nrow=m,ncol=n)
	for (i in 1:m)  { for (j in 1:n) 
		dd[i,j] = sqrt( (x[i,1]-y[j,1])^2 + (x[i,2]-y[j,2])^2 ) }
		print(dd)
		## make plot
		all = (rbind(x,y))
		plot(all,type="n",xlab="",ylab="")  # setup the graph
		text(x,label = paste("x",1:m,sep=""),col=colx)
		text(y,label = paste("y",1:n,sep=""),col=coly)
            # list(dd,x,y,m,n)
		list(crdist=dd,vec1=x,length1=m)
}

pairdistplot(a,b,2,3)

##########################################################################
#  Examples of Nesting functions within functions
##########################################################################

##########################################################################
## What does the following do?
##########################################################################

mytest = function(n,fun){
             u = runif(n)-.5
             fun(u)
}

mytest(10,sqrt)

mytest(10,sin)

mytest(10,exp)

##########################################################################
## What does the following do?
##########################################################################

myfct = function(x,fun,...) {
              m = length(x)
              x[m] = NA
              fun(x,...)
                     }
## Example:

x = runif(10)
myfct(x,mean,na.rm=T)
myfct(x,mean)
                 
##########################################################################
## What does the following do?
##########################################################################

myf = function(x,y)
 { 
    z1 = sin(x)
    z2 = cos(y)
    if(z1 < 0) 
      {return( list(z1,z2))} 
   else  
      { return( z1 + z2)} 
  }

# Examples:

myf(-pi/4,2*pi)   
myf(pi/4,pi)  
  

##########################################################################
## same as above:
##########################################################################

##
myf = function(x,y) { 
	z1 = sin(x)
	z2 = cos(y)
	ifelse (z1 < 0, return(list(z1,z2)),return(z1+z2) )
	}


## looking at my functions

pairdist

## look at an R function

dist
