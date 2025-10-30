#Exercise 6.1

source("BernBeta.R")
source("DBDA2E-utilities.R")

openGraph()
# Exercise 6.1A:
post = BernBeta( priorBetaAB=c(4,4) , Data=c(1) )

# Exercise 6.1B:
post = BernBeta( priorBetaAB=post , Data=c(1) )

# Exercise 6.1C:
post = BernBeta( priorBetaAB=post , Data=c(0) )

# Exercise 6.1D:
post = BernBeta( priorBetaAB=c(4,4) , Data=c(0) )
post = BernBeta( priorBetaAB=post , Data=c(1) )
post = BernBeta( priorBetaAB=post , Data=c(1) )

#Exercise 6.2

source("DBDA2E-utilities.R") # Load definitions of graphics functions etc.
source("BernBeta.R") # Load the definition of the BernBeta function
openGraph()
# Exercise 6.2A:
post = BernBeta( priorBetaAB=c(1,1) , Data=c(rep(1,58),rep(0,100-58)) ,
                 showHDI=TRUE , showCentTend="Mode" )

# Exercise 6.2B:
post = BernBeta( priorBetaAB=post , Data=c(rep(1,57),rep(0,100-57)) ,
                 showHDI=TRUE , showCentTend="Mode" )

##Exercise 6.3

source("DBDA2E-utilities.R") # Load definitions of graphics functions etc.
source("BernBeta.R") # Load the definition of the BernBeta function
openGraph()
# Response F is y=1, response J is y=0.
# "radio":
post = BernBeta( priorBetaAB=c(1,1) , Data=c(rep(1,40),rep(0,10)) ,
                 showHDI=TRUE , showCentTend="Mode" )


# "ocean":
post = BernBeta( priorBetaAB=c(1,1) , Data=c(rep(1,15),rep(0,35)) ,
                 showHDI=TRUE , showCentTend="Mode" )

#Exercise 6.4

source("DBDA2E-utilities.R") # Load definitions of graphics functions etc.
source("BernBeta.R") # Load the definition of the BernBeta function
openGraph()
# Exercise 6.4:
post = BernBeta( priorBetaAB=c(1,1)/100 , Data=c(rep(1,4),rep(0,1)) ,
                 showHDI=TRUE , showCentTend="Mode" )

#Exercise 6.5

source("DBDA2E-utilities.R") # Load definitions of graphics functions etc.
source("BernBeta.R") # Load the definition of the BernBeta function
openGraph()
post = BernBeta( priorBetaAB=c(1,1)*500 , Data=c(rep(1,9),rep(0,1)) ,
                 showHDI=TRUE , showCentTend="Mean" )

post = BernBeta( priorBetaAB=c(1,1)/100 , Data=c(rep(1,9),rep(0,1)) ,
                 showHDI=TRUE , showCentTend="Mean" )


#Exercise 7.1

proposalSD =
  c(0.02,0.2,2.0)[1]

proposalSD =
  c(0.02,0.2,2.0)[2]

proposalSD =
  c(0.02,0.2,2.0)[3]




#Exercise 7.2

openGraph(height=7,width=3.5) # Open a tall graphics window.
layout(matrix(1:2,nrow=2)) # Graphics window has two panels.
# Compute autocorrelation function of accepted trjectory, with maximum lag of
# 30, and plot the function using skyblue color and linewidth of 3:
acf( acceptedTraj , lag.max=30 , col="skyblue" , lwd=3 )
# Now make a scatter plot of the MCMC trjectory plotted against the its values
# 10 steps later. Call the initial values trajHead, and call the lagged values
# trajTail.
Len = length( acceptedTraj ) # Store length of trajectory for convenience.
Lag = 10 # Specify the lag.
trajHead = acceptedTraj[ 1 : (Len-Lag) ] # Extract all but the last Lag steps.
trajTail = acceptedTraj[ (1+Lag) : Len ] # Extract all but the 1st Lag steps.
# Make a scatter plot of original values on x axis against values Lag steps
# later on y axis. Also display the correlation in the main title of the plot.
plot( trajHead , trajTail , pch="." , col="skyblue" ,
      main=bquote( list( "Prpsl.SD" == .(proposalSD) ,
                         lag == .(Lag) ,
                         cor == .(round(cor(trajHead,trajTail),3)))) )



#Exercise 7.3

source("DBDA2E-utilities.R")
openGraph()
theta = seq(0,1,length=501)
plot( theta , (cos(4*pi*theta)+1)^2/1.5 ,
      type="l" , lwd=3 , col="skyblue" )

# Define the prior density function.
prior = function( theta ) {
  # pTheta = dbeta( theta , 1 , 1 )
  pTheta = (cos(4*pi*theta)+1)^2/1.5
  # The theta values passed into this function are generated at random,
  # and therefore might be inadvertently greater than 1 or less than 0.
  # The prior for theta > 1 or for theta < 0 is zero:
  pTheta[ theta > 1 | theta < 0 ] = 0
  return( pTheta )
}

# myData = c(rep(0,6),rep(1,14))
myData = c()


#Exercise 8.1

source("BernBeta.R")
source("DBDA2E-utilities.R")

# Here is one way to set up simulated data in R and save it in a file. You can
# use a text editor or Excel or whatever. For subject names, I've used "A", "B",
# and "C", merely for simplicity.
y = c( rep(1,9),rep(0,3) , rep(1,45),rep(0,15) , rep(1,3),rep(0,9) )
s = c( rep("A",12) , rep("B",60) , rep("C",12) )
write.csv( data.frame(y=y,s=s) , file="Exercise.08.1.csv" , row.names=FALSE )

# Below is just the essential lines of Jags-Ydich-XnomSsubj-MbernBeta-Example.R
# with the data file changed:
graphics.off()
rm(list=ls(all=TRUE))
fileNameRoot="Exercise.08.1" # for output filenames
source("DBDA2E-utilities.R")
# Load The data from the file:
myData = read.csv("Exercise.08.1.csv")
# Load the relevant model into R's working memory:
source("Jags-Ydich-XnomSsubj-MbernBeta.R")
# Generate the MCMC chain:
mcmcCoda = genMCMC( data=myData , numSavedSteps=50000 , saveName=fileNameRoot )
parameterNames = varnames(mcmcCoda) # get all parameter names
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName )
}
# Get summary statistics of chain:
summaryInfo = smryMCMC( mcmcCoda , compVal=NULL , #rope=c(0.45,0.55) ,
                        compValDiff=0.0 , #ropeDiff = c(-0.05,0.05) ,
                        saveName=fileNameRoot )
# Display posterior information:
plotMCMC( mcmcCoda , data=myData , compVal=NULL , #rope=c(0.45,0.55) ,
          compValDiff=0.0 , #ropeDiff = c(-0.05,0.05)
)



#Exercise 8.2

summaryInfo = smryMCMC( mcmcCoda , compVal=0.5 , rope=c(0.45,0.55) ,
                        compValDiff=0.0 , ropeDiff = c(-0.05,0.05) )

#Exercise 8.3

fileNameRoot = "Jags-Ydich-XnomSsubj-MbernBeta-"
graphFileType = "eps"

# Generate the MCMC chain:
mcmcCoda = genMCMC( data=myData , numSavedSteps=50000 , saveName=fileNameRoot )

# Display diagnostics of chain, for specified parameters:
parameterNames = varnames(mcmcCoda) # get all parameter names
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName ,
            saveName=fileNameRoot , saveType=graphFileType )
}

# Get summary statistics of chain:
summaryInfo = smryMCMC( mcmcCoda , compVal=0.5 , rope=c(0.45,0.55) ,
                        compValDiff=0.0 , ropeDiff = c(-0.05,0.05) ,
                        saveName=fileNameRoot )

# Display posterior information:
plotMCMC( mcmcCoda , data=myData , compVal=NULL , #rope=c(0.45,0.55) ,
          compValDiff=0.0 , #ropeDiff = c(-0.05,0.05) ,
          saveName=fileNameRoot , saveType=graphFileType )


##Exercise 8.4

dataList = list(
  # y = y ,
  s = s ,
  Ntotal = Ntotal ,
  Nsubj = Nsubj
)

model {
  for ( i in 1:Ntotal ) {
    y[i] ~ dbern( theta[s[i]] )
  }
  for ( s in 1:Nsubj ) {
    theta[s] ~ dbeta( 1 , 1 )
  }
}

model {
  for ( i in 1:Ntotal ) {
    y[i] ~ dbern( theta[s[i]] )
  }
  for ( s in 1:Nsubj ) {
    theta[s] ~ dbeta( 0.5 , 0.5 )
  }
}

#Exercise 9.1

source("DBDA2E-utilities.R")
gammaShRaFromMeanSD( mean=1.0 , sd=10.0 )

gammaShRaFromModeSD( mode=1.0 , sd=10.0 )

openGraph(height=7,width=7)
layout(matrix(1:3,ncol=1))
k=seq(0,200,length=10001)
plot( k , dgamma(k,1.105125,0.105125) , ylab="dgamma(k)" ,
      type="l" , main="Gamma Distrib’s (SD=10)" )
lines( k , dgamma(k,0.01,0.01) , col="skyblue" )
legend( "topright" , c("Mode 1","Mean 1") ,
        lty=c(1,1) , col=c("black","skyblue") , text.col=c("black", "skyblue") )
plot( k , dgamma(k,1.105125,0.105125) , ylab="dgamma(k)" ,
      type="l" , ylim=c(.07,.08) , main="Gamma Distrib’s (SD=10), zoomed in" )
lines( k , dgamma(k,0.01,0.01) , col="skyblue" )
legend( "topright" , c("Mode 1","Mean 1") ,
        lty=c(1,1) , col=c("black","skyblue") , text.col=c("black", "skyblue") )
plot( k , dgamma(k,1.105125,0.105125) , ylab="dgamma(k)" ,
      type="l" , ylim=c(0,8.0e-5) , main="Gamma Distrib’s (SD=10), zoomed in" )
lines( k , dgamma(k,0.01,0.01) , col="skyblue" )
legend( "topright" , c("Mode 1","Mean 1") ,
        lty=c(1,1) , col=c("black","skyblue") , text.col=c("black", "skyblue") )


kappaMinusTwo ~ dgamma( 0.01 , 0.01 ) # mean=1 , sd=10

kappaMinusTwo ~ dgamma( 1.105125 , 0.1051249 ) # mode=1 , sd=10

#Exercise 9.3

# Generate the data frame:
# N.B.: The functions below expect the data to be a data frame,
# with one component being a vector of integer 0,1 values,
# and one component being a factor of subject identifiers.
headsTails = c( rep(1,30),rep(0,100-30),
                rep(1,40),rep(0,100-40),
                rep(1,50),rep(0,100-50),
                rep(1,60),rep(0,100-60),
                rep(1,70),rep(0,100-70) )
subjID = factor( c( rep("A",100),
                    rep("B",100),
                    rep("C",100),
                    rep("D",100),
                    rep("E",100) ) )
myData = data.frame( y=headsTails , s=subjID )
#-------------------------------------------------------------------------------
# Load the relevant model into R's working memory:
source("Jags-Ydich-XnomSsubj-MbernBetaOmegaKappa.R")
fileNameRoot = "Exercise.09.3-"
graphFileType = "eps"
# Generate the MCMC chain:
mcmcCoda = genMCMC( data=myData , sName="s" , yName="y" ,
                    numSavedSteps=10000 , saveName=fileNameRoot , thinSteps=10 )
# Display diagnostics of chain, for specified parameters:
parameterNames = varnames(mcmcCoda) # get all parameter names for reference
for ( parName in parameterNames[c(1:3,length(parameterNames))] ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName ,
            saveName=fileNameRoot , saveType=graphFileType )
}
# Get summary statistics of chain:
summaryInfo = smryMCMC( mcmcCoda , compVal=0.5 ,
                        diffIdVec=c(1,2,3,4,5), compValDiff=0.0,
                        saveName=fileNameRoot )
# Display posterior information:
plotMCMC( mcmcCoda , data=myData , sName="s" , yName="y" ,
          compVal=0.5 , #rope=c(0.45,0.55) ,
          diffIdVec=c(1,2,3,4,5), compValDiff=0.0, #ropeDiff = c(-0.05,0.05) ,
          saveName=fileNameRoot , saveType=graphFileType )


#Exercise 9.4


# Generate the MCMC chain:
startTime = proc.time()
mcmcCoda = genMCMC( data=myData , sName="s" , yName="y" ,
                    numSavedSteps=20000 , saveName=fileNameRoot ,
                    thinSteps=10 )
stopTime = proc.time()
show( stopTime-startTime )


nChains = 3
useRunjags = TRUE
if ( useRunjags ) {
  runJagsOut <- run.jags( method=c("rjags","parallel")[2] ,
                          model="TEMPmodel.txt" ,
                          monitor=parameters ,
                          data=dataList ,
                          inits=initsList ,
                          n.chains=nChains ,
                          adapt=adaptSteps ,
                          burnin=burnInSteps ,
                          sample=ceiling(numSavedSteps/nChains) ,
                          thin=thinSteps ,
                          summarise=FALSE ,
                          plots=FALSE )
  codaSamples = as.mcmc.list( runJagsOut )
}# else { …
  
  
#  runJagsOut <- run.jags( method=c("rjags","parallel")[1] 
                        
                          
                          
#Exercise 10.1                          

# Define function for computing the marginal likelihood, as on p. 270 of book:
pD = function(z,N,a,b) { exp( lbeta(z+a,N-z+b) - lbeta(a,b) ) }
# Specify parameter values of the factories:
omega1 = 0.25
omega2 = 0.75
kappa = 6 # 6 for 10.1A, 202 for 10.1B
# Compute corresponding a,b values:
a1 = omega1*(kappa-2) + 1
b1 = (1-omega1)*(kappa-2) + 1
a2 = omega2*(kappa-2) + 1
b2 = (1-omega2)*(kappa-2) + 1
# Specify the data:
z = 7
N = 10
# Compute the marginal likelihoods:
pDg1 = pD(z,N,a1,b1)
pDg2 = pD(z,N,a2,b2)
# Compute the Bayes factor:
BF12 = pDg1/pDg2
# Specify prior probabilities:
p1 = 0.5
p2 = 1-p1
# Compute posterior probabilities as on p. 271 of book:
BF12xPriorOdds = (pDg1/pDg2)*(p1/p2)
p1gD = BF12xPriorOdds/(1.0+BF12xPriorOdds)
p2gD = 1.0-p1gD
# Display results:
show(BF12)
show(p1gD)
show(p2gD)

show(BF12)

show(p1gD)

show(p2gD)

#Exercise 10.2

openGraph(height=4,width=7)
plotPost( theta , main="theta" )


N=10
z=7
y = c( rep(0,N-z) , rep(1,z) )
dataList = list(
  y = y ,
  N = N
)
#------------------------------------------------------------------------------
# THE MODEL.
modelString = "
model {
for ( i in 1:N ) {

y[i] ~ dbern( theta )
}
theta ~ dbeta( omega[m]*(kappa-2)+1 , (1-omega[m])*(kappa-2)+1 )
omega[1] <- .25
omega[2] <- .75
kappa <- 6
}

modelString = "
model {
  for ( i in 1:N ) {
    y[i] ~ dbern( theta )
  }
  theta ~ dbeta( omega[m]*(kappa-2)+1 , (1-omega[m])*(kappa-2)+1 )
  omega[1] <- .25
  omega[2] <- .75
  kappa <- 52
  m ~ dcat( mPriorProb[] )
  mPriorProb[1] <- .95
  mPriorProb[2] <- .05
}"


" # close quote for modelString
writeLines( modelString , con="TEMPmodel.txt" )
#------------------------------------------------------------------------------
# INTIALIZE THE CHAINS.
initsList = list( list(theta=0.5,m=1) ,
list(theta=0.5,m=2) ,
list(theta=0.5,m=1) ,
list(theta=0.5,m=2) )
#------------------------------------------------------------------------------
# RUN THE CHAINS


parameters = c("theta","m")
adaptSteps = 10000 # Number of steps to "tune" the samplers.
burnInSteps = 10000 # Number of steps to "burn-in" the samplers.
nChains = 4 # Number of chains to run.
numSavedSteps=10000 # Total number of steps in chains to save.
thinSteps=10 # Number of steps to "thin" (1=keep every step).
nPerChain = ceiling( ( numSavedSteps * thinSteps ) / nChains ) # Steps per chain.
# Create, initialize, and adapt the model:
jagsModel = jags.model( "TEMPmodel.txt" , data=dataList , inits=initsList ,
                        n.chains=nChains , n.adapt=adaptSteps )


#Exercise 10.3

model {
  for ( i in 1:N ) {
    y[i] ~ dbern( theta )
  }
  theta <- equals(m,1)*theta1 + equals(m,2)*theta2
  theta1 ~ dbeta( omega1[m]*(kappa1[m]-2)+1 , (1-omega1[m])*(kappa1[m]-2)+1 )
  omega1[1] <- .10 # true prior value
  omega1[2] <- .10 # pseudo prior value
  kappa1[1] <- 20 # true prior value
  kappa1[2] <- 20 # pseudo prior value
  theta2 ~ dbeta( omega2[m]*(kappa2[m]-2)+1 , (1-omega2[m])*(kappa2[m]-2)+1 )
  omega2[1] <- .90 # pseudo prior value
  omega2[2] <- .90 # true prior value
  kappa2[1] <- 20 # pseudo prior value
  kappa2[2] <- 20 # true prior value
  m ~ dcat( mPriorProb[] )
  mPriorProb[1] <- .5
  mPriorProb[2] <- .5
}

model {
  for ( i in 1:N ) {
    y[i] ~ dbern( theta )
  }
  theta <- equals(m,1)*theta1 + equals(m,2)*theta2
  theta1 ~ dbeta( omega1[m]*(kappa1[m]-2)+1 , (1-omega1[m])*(kappa1[m]-2)+1 )
  omega1[1] <- .10 # true prior value
  omega1[2] <- .40 # pseudo prior value
  kappa1[1] <- 20 # true prior value
  kappa1[2] <- 50 # pseudo prior value
  theta2 ~ dbeta( omega2[m]*(kappa2[m]-2)+1 , (1-omega2[m])*(kappa2[m]-2)+1 )
  omega2[1] <- .70 # pseudo prior value
  omega2[2] <- .90 # true prior value
  kappa2[1] <- 50 # pseudo prior value
  kappa2[2] <- 20 # true prior value
  m ~ dcat( mPriorProb[] )
  mPriorProb[1] <- .5
  mPriorProb[2] <- .5
}

model {
  for ( i in 1:N ) {
    y[i] ~ dbern( theta )
  }
  theta <- equals(m,1)*theta1 + equals(m,2)*theta2
  theta1 ~ dbeta( omega1[m]*(kappa1[m]-2)+1 , (1-omega1[m])*(kappa1[m]-2)+1 )
  omega1[1] <- .10 # true prior value
  omega1[2] <- .50 # pseudo prior value
  kappa1[1] <- 20 # true prior value
  kappa1[2] <- 2.1 # pseudo prior value
  theta2 ~ dbeta( omega2[m]*(kappa2[m]-2)+1 , (1-omega2[m])*(kappa2[m]-2)+1 )
  omega2[1] <- .50 # pseudo prior value
  omega2[2] <- .90 # true prior value
  kappa2[1] <- 2.1 # pseudo prior value
  kappa2[2] <- 20 # true prior value
  m ~ dcat( mPriorProb[] )
  mPriorProb[1] <- .5
  mPriorProb[2] <- .5
}












#Exercise 11.1

# Data:
N = 45 ; z = 3
# p(y=1):
theta = 1/6
# Consider the low tail because z/N = 3/45 is less than expected p=1/6:
lowTailZ = 0:z
# Cumulative low tail probability:
lowTailP = sum( choose(N,lowTailZ) * theta^lowTailZ * (1-theta)^(N-lowTailZ) )
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

#For candidate theta values from 0.005 to 0.020, which are less than z/N observed,
# compute the right-tail p value:
 highTailZ = z:N
 for ( theta in seq( 0.005 , 0.020 , 0.001) ) {
   show( c(
     theta ,
     2*sum( choose(N,highTailZ) * theta^highTailZ * (1-theta)^(N-highTailZ) )
     ))
   }

 # For candidate theta values GREATER than z/N observed, compute the LEFT-tail p
  # value:
    complN = z:(N-1)
  for ( theta in seq( 0.150 , 0.160 , 0.001) ) {
    show( c(
      theta ,
      2*(1-sum( (z/complN) * choose(complN,z) * theta^z * (1-theta)^(complN-z) ) )
      ) )
  }
    
    # For candidate theta values LESS than z/N observed, compute the RIGHT-tail p
     # value:
       highTailN = z:N # Notice N not N-1
     for ( theta in seq( 0.005 , 0.020 , 0.001) ) {
       show( c(
         theta ,
         2*sum(
           (z/highTailN) * choose(highTailN,z) * theta^z * (1-theta)^(highTailN-z)
           )
         ) )
     }
       
#Exercise 11.3
       
       # Data:
        N = 45 ; z = 3
        # Hypothetical value of parameter:
          theta = 1/6
        # Specify possible N values:
          Nposs = 40:50
        # Specify probability of each N (here all equal):
          Nprob = rep(1,length(Nposs)) # All Nposs get relative probability of 1.
        Nprob = Nprob/sum(Nprob) # Normalize to get actual probability mass.
        # Initialize total tail probability to zero.
          totalP = 0
        # For each N, compute its p value, and accumulate the weighted total p:
          for ( i in 1:length(Nposs) ) {
            # For convenience, rename the N that is presently being considered:
              thisN = Nposs[i]
              # For this N, determine the max z that is in the low tail.
                # It must satisfy thisZ/thisN <= z/N.
                thisZ = max( (0:thisN)[ (0:thisN)/thisN <= z/N ] )
                # Now compute tail probability, i.e., sum of binomial probabilities from z/N
                  # down to zero.
                  lowTailZ = 0:thisZ
                  thisP = 2*sum(
                    choose(thisN,lowTailZ) * theta^lowTailZ * (1-theta)^(thisN-lowTailZ) )
                  # Accumulate to totalP the value of thisP weight by the probability of thisN:
                    totalP = totalP + Nprob[i] * thisP
                    # Display progress through the loop:
                      show( c( thisN , thisP ) )
          }
          
# Display the final result:
 show( totalP )
 
 #Exercise 12.1
 
 source("BernBeta.R")
 source("DBDA2E-utilities.R")
 z = 7 ; N = 24
 theta = 0.5
 pDgTheta = theta^z * (1-theta)^(N-z)
 print( pDgTheta )


 
 a = 2000 ; b = 2000
 openGraph(width=5,height=7)
 BernBeta( c(a,b) , c(rep(0,N-z),rep(1,z)) , ROPE=c(0.48,0.52) ,
           plotType="Bars" , showCentTend="Mode" , showHDI=TRUE , showpD=TRUE )
 
 
 a = 0.01 ; b = 0.01
 openGraph(width=5,height=7)
 BernBeta( c(a,b) , c(rep(0,N-z),rep(1,z)) , ROPE=c(0.48,0.52) ,
           plotType="Bars" , showCentTend="Mode" , showHDI=TRUE , showpD=TRUE )

 print( 2.87e-09 / 5.96e-08 )
 
 a = 2 ; b = 4
 openGraph(width=5,height=7)
 BernBeta( c(a,b) , c(rep(0,N-z),rep(1,z)) , ROPE=c(0.48,0.52) ,
           plotType="Bars" , showCentTend="Mode" , showHDI=TRUE , showpD=TRUE )
 
 print( 2.22e-07 / 5.96e-08 )
 
 print( 5.0 / 1.38 )
 
 #Exercise 12.2
 

 
 source("OneOddGroupModelComp2E.R")
 
 # Use omega[j] for model index 1, omega0 for model index 2:
 aBeta[j] <- ( equals(mdlIdx,1)*omega[j]
               + equals(mdlIdx,2)*omega0 ) * (kappa[j]-2)+1
 bBeta[j] <- ( 1 - ( equals(mdlIdx,1)*omega[j]
                     + equals(mdlIdx,2)*omega0 ) ) * (kappa[j]-2)+1
 
 
 