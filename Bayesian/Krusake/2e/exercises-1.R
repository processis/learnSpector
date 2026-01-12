#Exercise 14.1

source("Stan-Ydich-XnomSsubj-MbernBetaOmegaKappa-Example.R")

source("BernBeta.R")
source("DBDA2E-utilities.R")

transformed parameters {
  real<lower=0> kappa ;
  kappa <- kappaMinusTwo + 2 ;
}
model {
  omega ~ beta( 1 , 1 ) ;
  kappaMinusTwo ~ gamma( 0.01 , 0.01 ) ; // mean=1 , sd=10 (generic vague)
  theta ~ beta( omega*(kappa-2)+1 , (1-omega)*(kappa-2)+1 ) ; // vectorized
  for ( i in 1:Ntotal ) {
    y[i] ~ bernoulli( theta[s[i]] ) ;
  }
}


startTime = proc.time()
mcmcCoda = genMCMC( data=myData , sName="s" , yName="y" ,
                    numSavedSteps=20000 , saveName=fileNameRoot , thinSteps=10 )
stopTime = proc.time()
duration = stopTime - startTime
show(duration)


#Exercise 14.2

# Stan-Ydich-Xnom1subj-MbernBeta-Power.R
graphics.off() # This closes all of R's graphics windows.
rm(list=ls()) # Careful! This clears all of R's memory!
source("DBDA2E-utilities.R")
require(rstan)
fileNameRoot = "Stan-Ydich-Xnom1subj-MbernBeta-Power-" # for future use
# Create the Stan DSO:
modelString = "
data {
int<lower=0> Ntotal ;
int<lower=0,upper=1> y[Ntotal] ;
}
parameters {
real<lower=0,upper=1> theta ;
}

model {
theta ~ beta(1,1) ;
y ~ bernoulli(theta) ; // implicitly vectorized
}
" # close quote for modelString
# Translate to C++ and compile to DSO:
stanDso <- stan_model( model_code=modelString )
# Define specialized genMCMC that uses pre-made Stan DSO
genMCMC = function( stanDso=stanDso , data , numSavedSteps=50000 ,
                    saveName=NULL ) {
  #-----------------------------------------------------------------------------
  # THE DATA.
  if ( class(data)=="data.frame" ) { # If data is a data.frame
    y = myData$y # then pull out the column named y
  } else { # else
    y = data # rename the data as y.
  }
  # Do some checking that data make sense:
  if ( any( y!=0 & y!=1 ) ) { stop("All y values must be 0 or 1.") }
  Ntotal = length(y)
  # Specify the data in a list, for later shipment to JAGS:
  dataList = list(
    y = y ,
    Ntotal = Ntotal
  )
  #-----------------------------------------------------------------------------
  # INTIALIZE THE CHAINS.
  initsList = function() {
    resampledY = sample( y , replace=TRUE )
    thetaInit = sum(resampledY)/length(resampledY)
    thetaInit = 0.001+0.998*thetaInit # keep away from 0,1
    return( list( theta=thetaInit ) )
  }
  #-----------------------------------------------------------------------------
  # RUN THE CHAINS
  parameters = c( "theta") # The parameters to be monitored
  burnInSteps = 500 # Stan defaults to iter/2 for overdispersed inits
  nChains = 4 # nChains should be 2 or more for diagnostics
  thinSteps = 4 # In Stan there is autocorrelation, so thin
  stanFit <- sampling( object=stanDso ,
                       data = dataList ,
                       pars = parameters , # optional
                       chains = nChains ,
                       iter = ( ceiling(numSavedSteps/nChains)*thinSteps
                                +burnInSteps ) ,
                       warmup = burnInSteps ,
                       thin = thinSteps ,
                       init = initsList ) # optional
  # For consistency with JAGS-oriented functions in DBDA2E collection,
  # convert stan format to coda format:
  codaSamples = mcmc.list(lapply(1:ncol(stanFit) ,
                                 function(x){mcmc(as.array(stanFit)[,x,])}) )
  # resulting codaSamples object has these indices:
  # codaSamples[[ chainIdx ]][ stepIdx , paramIdx ]
  if ( !is.null(saveName) ) {
    save( codaSamples , file=paste(saveName,"Mcmc.Rdata",sep="") )
    save( stanFit , file=paste(saveName,"StanFit.Rdata",sep="") )
    save( stanDso , file=paste(saveName,"StanDso.Rdata",sep="") )
  }
  return( codaSamples )
} # end function

# Define function that assesses goal achievement for a single set of data:
goalAchievedForSample = function( data ) {
  # Generate the MCMC chain:
  mcmcCoda = genMCMC( stanDso=stanDso , data=data , numSavedSteps=10000 ,
                      saveName=NULL )
  # Check goal achievement. First, compute the HDI:
  thetaHDI = HDIofMCMC( as.matrix(mcmcCoda[,"theta"]) )
  # Define list for recording results:
  goalAchieved = list()
  # Goal: Exclude ROPE around null value:
  thetaROPE = c(0.48,0.52)
  goalAchieved = c( goalAchieved ,
                    "ExcludeROPE"=( thetaHDI[1] > thetaROPE[2]
                                    | thetaHDI[2] < thetaROPE[1] ) )
  # Goal: HDI less than max width:
  thetaHDImaxWid = 0.2
  goalAchieved = c( goalAchieved ,
                    "NarrowHDI"=( thetaHDI[2]-thetaHDI[1] < thetaHDImaxWid ) )
  # More goals can be inserted here if wanted...
  # Return list of goal results:
  return(goalAchieved)
}
# Specify mode and concentration of hypothetical parameter distribution:
omega = 0.70
kappa = 2000
# Specify sample size for each simulated data set:
sampleN = 74
# Run a bunch of simulated experiments:
nSimulatedDataSets = 100 # An arbitrary large number.
for ( simIdx in 1:nSimulatedDataSets ) {
  # Generate random value from hypothesized parameter distribution:
  genTheta = rbeta( 1 , omega*(kappa-2)+1 , (1-omega)*(kappa-2)+1 )
  # Generate random data based on parameter value:
  sampleZ = rbinom( 1 , size=sampleN , prob=genTheta )
  # Convert to vector of 0's and 1's for delivery to Stan function:
  simulatedData = c(rep(1,sampleZ),rep(0,sampleN-sampleZ))
  # Do Bayesian analysis on simulated data:
  goalAchieved = goalAchievedForSample( simulatedData )
  # Tally the results:
  if (!exists("goalTally")) { # if goalTally does not exist, create it
    goalTally=matrix( nrow=0 , ncol=length(goalAchieved) )
  }
  goalTally = rbind( goalTally , goalAchieved )
  # save( goalTally ,
  # file="Stan-Ydich-Xnom1subj-MbernBeta-Power-goalTally.Rdata" )
}
# For each goal...
for ( goalIdx in 1:NCOL(goalTally) ) {
  # Extract the goal name for subsequent display:
  goalName = colnames(goalTally)[goalIdx]
  # Compute number of successes:
  goalHits = sum(unlist(goalTally[,goalIdx]))
  # Compute number of attempts:
  goalAttempts = NROW(goalTally)
  # Compute proportion of successes:
  goalEst = goalHits/goalAttempts
  # Compute HDI around proportion:
  goalEstHDI = HDIofICDF( qbeta ,
                          shape1=1+goalHits ,
                          shape2=1+goalAttempts-goalHits )
  
  # Display the result:
  show( paste0( goalName,
                ": Est.Power=" , round(goalEst,3) ,
                "; Low Bound=" , round(goalEstHDI[1],3) ,
                "; High Bound=" , round(goalEstHDI[2],3) ) )
}



#Exercise 16.1

source("Jags-Ymet-Xnom2grp-MrobustHet-Example.R")

myDataFrame = read.csv( file="ShohatOphirKAMH2012dataReduced.csv" )
xName="Group"
yName="PreferenceIndex"
fileNameRoot="ShohatOphirKAMH2012data-PI-"
RopeMuDiff=c(-0.1,0.1) ; RopeSdDiff=c(-0.1,0.1) ; RopeEff=c(-0.1,0.1)

myDataFrame = read.csv( file="ShohatOphirKAMH2012dataReduced.csv" )
xName="Group"
yName="GrandTotal"
fileNameRoot="ShohatOphirKAMH2012data-GT-"
RopeMuDiff=c(-0.1,0.1) ; RopeSdDiff=c(-0.1,0.1) ; RopeEff=c(-0.1,0.1)

#Exercise 16.2



myDataFrame = read.csv( file="RatLives.csv" )
xName="Group"
yName="DaysLive"
fileNameRoot = "RatLives-"
RopeMuDiff=c(-10,10) ; RopeSdDiff=c(-10,10) ; RopeEff=c(-0.1,0.1)


myDataFrame = read.csv( file="RatLives.csv" )
xName="Group"
myDataFrame = cbind( myDataFrame , DaysLiveSq = myDataFrame$DaysLive^2 )
yName="DaysLiveSq"
fileNameRoot = "RatLives-DaySq-"
RopeMuDiff=c(-100,100) ; RopeSdDiff=c(-100,100) ; RopeEff=c(-0.1,0.1)


#Exercise 16.3

model {
  sigma ~ uniform( unifLo , unifHi ) ; # vectorized
  mu ~ normal( meanY , normalSigma ) ; # vectorized
  nuMinusOne ~ exponential( expLambda ) ;
  # for ( i in 1:Ntotal ) {
    # y[i] ~ student_t( nu , mu[x[i]] , sigma[x[i]] ) ;
    # }
}
  
  dataList = list(
    # y = y ,
    x = x ,
    Ntotal = Ntotal ,
    meanY = mean(y) ,
    sdY = sd(y)
  )

  
#Exercise 17.1
  
source("Jags-Ymet-XmetSsubj-MrobustHier-Example.R")

  myData = read.csv( file="IncomeFamszState.csv" )
  xName = "Famsz" ; yName = "Income" ; sName="State"
  
  
#Exercise 17.2
  
  fileNameRoot = "Exercise.17.2-HtWtData30-Jags-"
  source("Exercise.17.2-Jags-Ymet-Xmet-Mrobust.R")
  
  # Standardize the data:
  data {
    Ntotal <- length(y)
    xm <- mean(x)
    ym <- mean(y)
    xsd <- sd(x)
    ysd <- sd(y)
    for ( i in 1:length(y) ) {
      # zx[i] <- ( x[i] - xm ) / xsd
      # zy[i] <- ( y[i] - ym ) / ysd
      zx[i] <- x[i] # original scale
      zy[i] <- y[i] # original scale
    }
  }
  # Specify the model for standardized data:
  model {
    for ( i in 1:Ntotal ) {
      zy[i] ~ dt( zbeta0 + zbeta1 * zx[i] , 1/zsigma^2 , nu )
    }
    # Priors vague on standardized scale:
    # zbeta0 ~ dnorm( 0 , 1/(10)^2 )
    # zbeta1 ~ dnorm( 0 , 1/(10)^2 )
    # zsigma ~ dunif( 1.0E-3 , 1.0E+3 )
    # Priors vague on original scale:
    zbeta0 ~ dnorm( 0 , 1/(10*abs(xm*ysd/xsd))^2 ) # same as Stan version
    zbeta1 ~ dnorm( 0 , 1/(10*abs(ysd/xsd))^2 ) # same as Stan version
    zsigma ~ dunif( ysd/1000 , ysd*1000 ) # same as Stan version
    nu <- nuMinusOne+1
    nuMinusOne ~ dexp(1/29.0)
    # Transform to original scale:
    # beta1 <- zbeta1 * ysd / xsd
    # beta0 <- zbeta0 * ysd + ym - zbeta1 * xm * ysd / xsd
    # sigma <- zsigma * ysd
    beta1 <- zbeta1 # original scale
    beta0 <- zbeta0 # original scale
    sigma <- zsigma # original scale
  }
  
  thinSteps = 50 # instead of thinSteps = 1
  
  #Exercise 17.3
  
  source("Jags-Ymet-XmetSsubj-MrobustHierQuadWt.R")
  #-------------------------------------------------------------------------------
  # Generate the MCMC chain:
  startTime = proc.time()
  mcmcCoda = genMCMC( data=myData ,
                      xName=xName , yName=yName , sName=sName , wName=wName ,
                      numSavedSteps=20000 , thinSteps=15 , saveName=fileNameRoot )
  stopTime = proc.time()
  duration = stopTime - startTime
  show(duration)
  
  
  source("Stan-Ymet-XmetSsubj-MrobustHierQuadWt.R")
  
  # Generate the MCMC chain:
  startTime = proc.time()
  mcmcCoda = genMCMC( data=myData ,
                      xName=xName , yName=yName , sName=sName , wName=wName ,
                      numSavedSteps=12000 , thinSteps=5 , saveName=fileNameRoot )
  stopTime = proc.time()
  duration = stopTime - startTime
  show(duration)
  
  
  #Exercise 18.2
  
  source("Jags-Ymet-XmetMulti-Mrobust-Example.R")
  
  myData = read.csv( file="MultLinRegrPlotUnif.csv" )
  yName = "y" ; xName = c("x1","x2")
  fileNameRoot = "MultLinRegrPlotUnif-"
  numSavedSteps=11000 ; thinSteps=2
  
  myData = read.csv( file="MultLinRegrPlotUnif.csv" )
  yName = "y" ; xName = c("x1")
  fileNameRoot = "MultLinRegrPlotUnif-"
  numSavedSteps=11000 ; thinSteps=2
  
  myData = read.csv( file="MultLinRegrPlotUnif.csv" )
  myData = myData[101:150,]
  yName = "y" ; xName = c("x1","x2")
  fileNameRoot = "MultLinRegrPlotUnif-"
  numSavedSteps=11000 ; thinSteps=2
  
  myData = read.csv( file="MultLinRegrPlotUnif.csv" )
  myData = myData[101:150,]
  yName = "y" ; xName = c("x1")
  fileNameRoot = "MultLinRegrPlotUnif-"
  numSavedSteps=11000 ; thinSteps=2
  
  #Exercise 18.3
  
  myData = read.csv( file="Guber1999data.csv" )
  PropNotTake = (100-myData[,"PrcntTake"])/100
  myData = cbind( myData , PropNotTake )
  yName = "SATT" ; xName = c("Spend","PrcntTake","PropNotTake")
  fileNameRoot = "Guber1999data-Jags-Redund-"
  numSavedSteps=15000 ; thinSteps=15
  
  data {
    ym <- mean(y)
    ysd <- sd(y)
    # for ( i in 1:Ntotal ) {
    # zy[i] <- ( y[i] - ym ) / ysd
    # }
    for ( j in 1:Nx ) {
      xm[j] <- mean(x[,j])
      xsd[j] <- sd(x[,j])
      for ( i in 1:Ntotal ) {
        zx[i,j] <- ( x[i,j] - xm[j] ) / xsd[j]
      }
    }
  }
  
  

  
  
  
  