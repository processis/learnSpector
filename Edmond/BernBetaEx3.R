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



############### Erdogmu Exp1 use prior Beta mode=0.75 kappa=12

# Specify the prior:
t = 0.75             # Specify the prior MODE.
n = 12              # Specify the effective prior sample size.
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
saveGraph(file="BernBetaEx3",type="png")

#### repeat 1  19 / 25

a = 25.5    
b = 14.5  

Prior = c(a,b)       # Specify Prior as vector with the two shape parameters.

# Specify the data:
N = 25                        # The total number of flips.
z = 19                         # The number of heads.
Data = c(rep(0,N-z),rep(1,z))  # Convert N and z into vector of 0's and 1's.

openGraph(width=5,height=7)
posterior = BernBeta( priorBetaAB=Prior, Data=Data , plotType="Bars" , 
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE )
saveGraph(file="BernBetaEx3Rep1",type="png")


#### repeat 2  8 / 12

a = 44.5  
b = 20.5

Prior = c(a,b)       # Specify Prior as vector with the two shape parameters.

# Specify the data:
N = 12                       # The total number of flips.
z = 8                        # The number of heads.
Data = c(rep(0,N-z),rep(1,z))  # Convert N and z into vector of 0's and 1's.

openGraph(width=5,height=7)
posterior = BernBeta( priorBetaAB=Prior, Data=Data , plotType="Bars" , 
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE )
saveGraph(file="BernBetaRepEx3Rep2",type="png")









#### repeat 3  9 / 12

a = 52.5 
b = 24.5

Prior = c(a,b)       # Specify Prior as vector with the two shape parameters.

# Specify the data:
N = 12                       # The total number of flips.
z = 9                        # The number of heads.
Data = c(rep(0,N-z),rep(1,z))  # Convert N and z into vector of 0's and 1's.

openGraph(width=5,height=7)
posterior = BernBeta( priorBetaAB=Prior, Data=Data , plotType="Bars" , 
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE )
saveGraph(file="BernBetaEx3Rep3",type="png")

#### repeat 4  8 / 11

a = 61.5 
b = 27.5

Prior = c(a,b)       # Specify Prior as vector with the two shape parameters.

# Specify the data:
N = 11                       # The total number of flips.
z = 8                        # The number of heads.
Data = c(rep(0,N-z),rep(1,z))  # Convert N and z into vector of 0's and 1's.

openGraph(width=5,height=7)
posterior = BernBeta( priorBetaAB=Prior, Data=Data , plotType="Bars" , 
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE )
saveGraph(file="BernBetaEx3Rep4",type="png")


#### repeat 5  7 out of 10

a = 69.5 
b = 30.5

Prior = c(a,b)       # Specify Prior as vector with the two shape parameters.

# Specify the data:
N = 10                       # The total number of flips.
z = 7                        # The number of heads.
Data = c(rep(0,N-z),rep(1,z))  # Convert N and z into vector of 0's and 1's.

openGraph(width=5,height=7)
posterior = BernBeta( priorBetaAB=Prior, Data=Data , plotType="Bars" , 
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE )
saveGraph(file="BernBetaEx3Rep5",type="png")

#### repeat 6  8 of 10

a = 76.5 
b = 33.5

Prior = c(a,b)       # Specify Prior as vector with the two shape parameters.

# Specify the data:
N = 10                       # The total number of flips.
z = 8                        # The number of heads.
Data = c(rep(0,N-z),rep(1,z))  # Convert N and z into vector of 0's and 1's.

openGraph(width=5,height=7)
posterior = BernBeta( priorBetaAB=Prior, Data=Data , plotType="Bars" , 
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE )
saveGraph(file="BernBetaEx3Rep6",type="png")

#### repeat 7  4 / 5

a = 84.5 
b = 35.5

Prior = c(a,b)       # Specify Prior as vector with the two shape parameters.

# Specify the data:
N = 5                       # The total number of flips.
z = 4                        # The number of heads.
Data = c(rep(0,N-z),rep(1,z))  # Convert N and z into vector of 0's and 1's.

openGraph(width=5,height=7)
posterior = BernBeta( priorBetaAB=Prior, Data=Data , plotType="Bars" , 
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE )
saveGraph(file="BernBetaEx3Rep7",type="png")