source("DBDA2E-utilities.R")  # Load definitions of graphics functions etc.
source("BernBeta.R")          # Load the definition of the BernBeta function

# Specify the prior:
t = 0.75             # Specify the prior MODE.
n = 25               # Specify the effective prior sample size.
a = t*(n-2) + 1      # Convert to beta shape parameter a.
b = (1-t)*(n-2) + 1  # Convert to beta shape parameter b.

Prior = c(a,b)       # Specify Prior as vector with the two shape parameters.

# Specify the data:
N = 20                         # The total number of flips.
z = 17                         # The number of heads.
Data = c(rep(0,N-z),rep(1,z))  # Convert N and z into vector of 0's and 1's.

openGraph(width=5,height=7)
posterior = BernBeta( priorBetaAB=Prior, Data=Data , plotType="Bars" , 
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE )
saveGraph(file="BernBetaExample",type="png")
###
# Ex1 use mode=0.75 kappa = 12  as prior to run C vs python experiment
#


#####
#Ex11.1
#Exercise 11.1

z = 17 ; N = 28
theta = 0.5
pDgTheta = theta^z * (1-theta)^(N-z)
print( pDgTheta )

# Two-tail probability:
TwoTailP = 2 * lowTailP
show( TwoTailP )

# Cumulative low tail probability is now given by negative binomial, but now the
# tail is over n>=N, which is computed as 1-p(n<N), as explained in Footnote 2,
# p. 307.
complN = z:(N-1)
complP = sum( (z/complN) *
                choose(complN,z) * theta^z * (1-theta)^(complN-z) )
lowTailP = 1-complP
# Two-tail probability:
TwoTailP = 2 * lowTailP
show( TwoTailP )

#Exercise 11.2

N = 45 ; z = 3
theta = 1/6
# z/N = 3/45 = 0.06666…
# For candidate theta values from 0.170 to 0.190, which are greater than z/N observed,
# compute the left-tail p value:
lowTailZ = 0:z
for ( theta in seq( 0.170 , 0.190 , 0.001) ) {
  show( c(
    theta ,
    2*sum( choose(N,lowTailZ) * theta^lowTailZ * (1-theta)^(N-lowTailZ) )
  ))
}


# use scripts from Ex12.1 to calculate BF for Exp CvsPhython
a = 2000; b = 2000  #use spike prior
openGraph(width=5,height=7)
BernBeta( c(a,b) , c(rep(0,N-z),rep(1,z)) , ROPE=c(0.48,0.52) ,
          plotType="Bars" , showCentTend="Mode" , showHDI=TRUE , showpD=TRUE )
print(3.73e-09/3.72529e-09)

a = 0.01; b = 0.01  #use Haldane prior
openGraph(width=5,height=7)
BernBeta( c(a,b) , c(rep(0,N-z),rep(1,z)) , ROPE=c(0.48,0.52) ,
          plotType="Bars" , showCentTend="Mode" , showHDI=TRUE , showpD=TRUE )

print(3.44e-11/3.72529e-9)

a = 1; b = 1  #use flat prior
openGraph(width=5,height=7)
BernBeta( c(a,b) , c(rep(0,N-z),rep(1,z)) , ROPE=c(0.48,0.52) ,
          plotType="Bars" , showCentTend="Mode" , showHDI=TRUE , showpD=TRUE )
print(1.61e-09/3.732529e-09)


a = 8.5; b = 3.5  #use 0.75 mode prior
openGraph(width=5,height=7)
BernBeta( c(a,b) , c(rep(0,N-z),rep(1,z)) , ROPE=c(0.48,0.52) ,
          plotType="Bars" , showCentTend="Mode" , showHDI=TRUE , showpD=TRUE )
print(2.99e-09/3.72529e-09)
############### Erdogmu Exp1 use prior Beta a=1 b=1 mode=0.5 kappa=2 uniform distribution

# Specify the prior:
t = 0.5             # Specify the prior MODE.
n = 2              # Specify the effective prior sample size.
a = t*(n-2) + 1      # Convert to beta shape parameter a.
b = (1-t)*(n-2) + 1  # Convert to beta shape parameter b.

Prior = c(a,b)       # Specify Prior as vector with the two shape parameters.

# Specify the data:
N = 28                         # The total number of flips.
z = 17                         # The number of heads.
Data = c(rep(0,N-z),rep(1,z))  # Convert N and z into vector of 0's and 1's.

openGraph(width=5,height=7)
posterior = BernBeta( priorBetaAB=Prior, Data=Data , plotType="Bars" , 
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE )
saveGraph(file="BernBetaEx4",type="png")

#### repeat 1  19 / 25

a = 18    
b = 12  

Prior = c(a,b)       # Specify Prior as vector with the two shape parameters.

# Specify the data:
N = 25                        # The total number of flips.
z = 19                         # The number of heads.
Data = c(rep(0,N-z),rep(1,z))  # Convert N and z into vector of 0's and 1's.

openGraph(width=5,height=7)
posterior = BernBeta( priorBetaAB=Prior, Data=Data , plotType="Bars" , 
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE )
saveGraph(file="BernBetaEx4Rep1",type="png")


#### repeat 2  8 / 12

a = 37  
b = 18

Prior = c(a,b)       # Specify Prior as vector with the two shape parameters.

# Specify the data:
N = 12                       # The total number of flips.
z = 8                        # The number of heads.
Data = c(rep(0,N-z),rep(1,z))  # Convert N and z into vector of 0's and 1's.

openGraph(width=5,height=7)
posterior = BernBeta( priorBetaAB=Prior, Data=Data , plotType="Bars" , 
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE )
saveGraph(file="BernBetaEx4Rep2",type="png")









#### repeat 3  9 / 12

a = 45 
b = 22

Prior = c(a,b)       # Specify Prior as vector with the two shape parameters.

# Specify the data:
N = 12                       # The total number of flips.
z = 9                        # The number of heads.
Data = c(rep(0,N-z),rep(1,z))  # Convert N and z into vector of 0's and 1's.

openGraph(width=5,height=7)
posterior = BernBeta( priorBetaAB=Prior, Data=Data , plotType="Bars" , 
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE )
saveGraph(file="BernBetaEx4Rep3",type="png")

#### repeat 4  8 / 11

a = 54 
b = 25

Prior = c(a,b)       # Specify Prior as vector with the two shape parameters.

# Specify the data:
N = 11                       # The total number of flips.
z = 8                        # The number of heads.
Data = c(rep(0,N-z),rep(1,z))  # Convert N and z into vector of 0's and 1's.

openGraph(width=5,height=7)
posterior = BernBeta( priorBetaAB=Prior, Data=Data , plotType="Bars" , 
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE )
saveGraph(file="BernBetaEx4Rep4",type="png")


#### repeat 5  7 out of 10

a = 62 
b = 28

Prior = c(a,b)       # Specify Prior as vector with the two shape parameters.

# Specify the data:
N = 10                       # The total number of flips.
z = 7                        # The number of heads.
Data = c(rep(0,N-z),rep(1,z))  # Convert N and z into vector of 0's and 1's.

openGraph(width=5,height=7)
posterior = BernBeta( priorBetaAB=Prior, Data=Data , plotType="Bars" , 
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE )
saveGraph(file="BernBetaEx4Rep5",type="png")

#### repeat 6  8 of 10

a = 69 
b = 31

Prior = c(a,b)       # Specify Prior as vector with the two shape parameters.

# Specify the data:
N = 10                       # The total number of flips.
z = 8                        # The number of heads.
Data = c(rep(0,N-z),rep(1,z))  # Convert N and z into vector of 0's and 1's.

openGraph(width=5,height=7)
posterior = BernBeta( priorBetaAB=Prior, Data=Data , plotType="Bars" , 
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE )
saveGraph(file="BernBetaEx4Rep6",type="png")

#### repeat 7  4 / 5

a = 77 
b = 33

Prior = c(a,b)       # Specify Prior as vector with the two shape parameters.

# Specify the data:
N = 5                       # The total number of flips.
z = 4                        # The number of heads.
Data = c(rep(0,N-z),rep(1,z))  # Convert N and z into vector of 0's and 1's.

openGraph(width=5,height=7)
posterior = BernBeta( priorBetaAB=Prior, Data=Data , plotType="Bars" , 
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE )
saveGraph(file="BernBetaEx4Rep7",type="png")

####################
# try Kruschke p270 code to cal Bayes Factor for Beta
#
a=3.5
b=8.5
N=9
z=6
pData = exp(lbeta(z+a,N-z+b) - lbeta(a,b))


a=8.5
b=3.5
N=9
z=6
pData1 = exp(lbeta(z+a,N-z+b) - lbeta(a,b))

####################
# try Ex4 from Beta(1,1) prior , calculate BF
#

##Experiment 17 out of 28
a=1
b=1
N=28
z=17
pData = exp(lbeta(z+a,N-z+b) - lbeta(a,b))


a=18
b=12
N=28
z=17
pData1 = exp(lbeta(z+a,N-z+b) - lbeta(a,b))
bayesfactor = pData1/pData


##Repeat 1 :  19 out of 25
a=1
b=1
N=25
z=19
pData = exp(lbeta(z+a,N-z+b) - lbeta(a,b))


a=37
b=18
N=25
z=19
pData1 = exp(lbeta(z+a,N-z+b) - lbeta(a,b))
bayesfactor = pData1/pData

##Repeat 2 :  8 out of 12
a=1
b=1
N=12
z=8
pData = exp(lbeta(z+a,N-z+b) - lbeta(a,b))


a=45
b=22
N=12
z=8
pData1 = exp(lbeta(z+a,N-z+b) - lbeta(a,b))
bayesfactor = pData1/pData


##Repeat 3 :  9 out of 12
a=1
b=1
N=12
z=9
pData = exp(lbeta(z+a,N-z+b) - lbeta(a,b))


a=54
b=25
N=12
z=9
pData1 = exp(lbeta(z+a,N-z+b) - lbeta(a,b))
bayesfactor = pData1/pData

##Repeat 6 :  8 out of 10
a=1
b=1
N=10
z=8
pData = exp(lbeta(z+a,N-z+b) - lbeta(a,b))


a=54
b=25
N=10
z=8
pData1 = exp(lbeta(z+a,N-z+b) - lbeta(a,b))
bayesfactor = pData1/pData


##Repeat 7 :  4 out of 5
a=1
b=1
N=5
z=4
pData = exp(lbeta(z+a,N-z+b) - lbeta(a,b))


a=81
b=34
N=5
z=4
pData1 = exp(lbeta(z+a,N-z+b) - lbeta(a,b))
bayesfactor = pData1/pData