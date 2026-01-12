############### Erdogmu Exp1 use prior Beta a=1 b=1 mode=0.5 kappa=2 uniform distribution
# calculate B.F.

source("BernBeta.R")
source("DBDA2E-utilities.R")

#log零假设概率

z = 17 ; N = 28
theta = 0.5
pDgTheta = theta^z * (1-theta)^(N-z)
print( pDgTheta )


# use scripts from Ex12.1 to calculate BF for Exp CvsPhython
a = 1; b = 1  #use flat prior
openGraph(width=5,height=7)
BernBeta( c(a,b) , c(rep(0,N-z),rep(1,z)) , ROPE=c(-Inf,0.50) ,
          plotType="Bars" , showCentTend="Mode" , showHDI=TRUE , showpD=TRUE )
print(1.61e-09/3.72529e-09)

#Repeat1
# calculate B.F.
z = 19 ; N = 25
theta = 0.5
pDgTheta = theta^z * (1-theta)^(N-z)
print( pDgTheta )


# use scripts from Ex12.1 to calculate BF for Exp CvsPhython
a = 18; b = 12  #use 
openGraph(width=5,height=7)
BernBeta( c(a,b) , c(rep(0,N-z),rep(1,z)) , ROPE=c(0.48,0.52) ,
          plotType="Bars" , showCentTend="Mode" , showHDI=TRUE , showpD=TRUE )
print(3.57e-07/2.980232e-08)

#Repeat2
# calculate B.F.
z = 8 ; N = 12
theta = 0.5
pDgTheta = theta^z * (1-theta)^(N-z)
print( pDgTheta )


# use scripts from Ex12.1 to calculate BF for Exp CvsPhython
a = 37; b = 18  #use 
openGraph(width=5,height=7)
BernBeta( c(a,b) , c(rep(0,N-z),rep(1,z)) , ROPE=c(0.48,0.52) ,
          plotType="Bars" , showCentTend="Mode" , showHDI=TRUE , showpD=TRUE )
print(0.000435/0.000244)

#Repeat3
# calculate B.F.
z = 9 ; N = 12
theta = 0.5
pDgTheta = theta^z * (1-theta)^(N-z)
print( pDgTheta )


# use scripts from Ex12.1 to calculate BF for Exp CvsPhython
a = 45; b = 22  #use 
openGraph(width=5,height=7)
BernBeta( c(a,b) , c(rep(0,N-z),rep(1,z)) , ROPE=c(0.48,0.52) ,
          plotType="Bars" , showCentTend="Mode" , showHDI=TRUE , showpD=TRUE )
print(0.000939/0.000244)

#Repeat4
# calculate B.F.
z = 8 ; N = 11
theta = 0.5
pDgTheta = theta^z * (1-theta)^(N-z)
print( pDgTheta )


# use scripts from Ex12.1 to calculate BF for Exp CvsPhython
a = 54; b = 25  #use 
openGraph(width=5,height=7)
BernBeta( c(a,b) , c(rep(0,N-z),rep(1,z)) , ROPE=c(0.48,0.52) ,
          plotType="Bars" , showCentTend="Mode" , showHDI=TRUE , showpD=TRUE )
print(0.00143/0.0004883)

#Repeat5
# calculate B.F.
z = 7 ; N = 10
theta = 0.5
pDgTheta = theta^z * (1-theta)^(N-z)
print( pDgTheta )


# use scripts from Ex12.1 to calculate BF for Exp CvsPhython
a = 62; b = 28  #use 
openGraph(width=5,height=7)
BernBeta( c(a,b) , c(rep(0,N-z),rep(1,z)) , ROPE=c(0.48,0.52) ,
          plotType="Bars" , showCentTend="Mode" , showHDI=TRUE , showpD=TRUE )
print(0.00211/0.0009765)

#Repeat6
# calculate B.F.
z = 8 ; N = 10
theta = 0.5
pDgTheta = theta^z * (1-theta)^(N-z)
print( pDgTheta )


# use scripts from Ex12.1 to calculate BF for Exp CvsPhython
a = 69; b = 31  #use 
openGraph(width=5,height=7)
BernBeta( c(a,b) , c(rep(0,N-z),rep(1,z)) , ROPE=c(0.48,0.52) ,
          plotType="Bars" , showCentTend="Mode" , showHDI=TRUE , showpD=TRUE )
print(0.00487/0.00097656)

#Repeat7
# calculate B.F.
z = 4 ; N = 5
theta = 0.5
pDgTheta = theta^z * (1-theta)^(N-z)
print( pDgTheta )


# use scripts from Ex12.1 to calculate BF for Exp CvsPhython
a = 77; b = 33  #use 
openGraph(width=5,height=7)
BernBeta( c(a,b) , c(rep(0,N-z),rep(1,z)) , ROPE=c(-Inf,0.50) ,
          plotType="Bars" , showCentTend="Mode" , showHDI=TRUE , showpD=TRUE )
print(0.0711/0.03125)

#####################
#try again use mode=0.75 initial prior
# use scripts from Ex12.1 to calculate BF for Exp CvsPhython
# calculate B.F.
z = 17 ; N = 28
theta = 0.5
pDgTheta = theta^z * (1-theta)^(N-z)
print( pDgTheta )


a = 8.5; b = 3.5  #use f0.75 prior
openGraph(width=5,height=7)
BernBeta( c(a,b) , c(rep(0,N-z),rep(1,z)) , ROPE=c(-Inf,0.50) ,
          plotType="Bars" , showCentTend="Mode" , showHDI=TRUE , showpD=TRUE )
print(3.725e-09/2.99e-09)

#Repeat1
# calculate B.F.
z = 19 ; N = 25
theta = 0.5
pDgTheta = theta^z * (1-theta)^(N-z)
print( pDgTheta )


# use scripts from Ex12.1 to calculate BF for Exp CvsPhython
a = 25.5; b = 14.5  #use 
openGraph(width=5,height=7)
BernBeta( c(a,b) , c(rep(0,N-z),rep(1,z)) , ROPE=c(0.48,0.52) ,
          plotType="Bars" , showCentTend="Mode" , showHDI=TRUE , showpD=TRUE )
print(4.87e-07/2.980232e-08)

#Repeat2
# calculate B.F.
z = 8 ; N = 12
theta = 0.5
pDgTheta = theta^z * (1-theta)^(N-z)
print( pDgTheta )


# use scripts from Ex12.1 to calculate BF for Exp CvsPhython
a = 44.5; b = 20.5  #use 
openGraph(width=5,height=7)
BernBeta( c(a,b) , c(rep(0,N-z),rep(1,z)) , ROPE=c(0.48,0.52) ,
          plotType="Bars" , showCentTend="Mode" , showHDI=TRUE , showpD=TRUE )
print(0.000438/0.000244)

#Repeat3
# calculate B.F.
z = 9 ; N = 12
theta = 0.5
pDgTheta = theta^z * (1-theta)^(N-z)
print( pDgTheta )


# use scripts from Ex12.1 to calculate BF for Exp CvsPhython
a = 52.5; b = 24.5  #use 
openGraph(width=5,height=7)
BernBeta( c(a,b) , c(rep(0,N-z),rep(1,z)) , ROPE=c(0.48,0.52) ,
          plotType="Bars" , showCentTend="Mode" , showHDI=TRUE , showpD=TRUE )
print(0.000978/0.000244)

#Repeat4
# calculate B.F.
z = 8 ; N = 11
theta = 0.5
pDgTheta = theta^z * (1-theta)^(N-z)
print( pDgTheta )


# use scripts from Ex12.1 to calculate BF for Exp CvsPhython
a = 61.5; b = 27.5  #use 
openGraph(width=5,height=7)
BernBeta( c(a,b) , c(rep(0,N-z),rep(1,z)) , ROPE=c(0.48,0.52) ,
          plotType="Bars" , showCentTend="Mode" , showHDI=TRUE , showpD=TRUE )
print(0.00146/0.0004883)

#Repeat5
# calculate B.F.
z = 7 ; N = 10
theta = 0.5
pDgTheta = theta^z * (1-theta)^(N-z)
print( pDgTheta )


# use scripts from Ex12.1 to calculate BF for Exp CvsPhython
a = 69.5; b = 30.5  #use 
openGraph(width=5,height=7)
BernBeta( c(a,b) , c(rep(0,N-z),rep(1,z)) , ROPE=c(0.48,0.52) ,
          plotType="Bars" , showCentTend="Mode" , showHDI=TRUE , showpD=TRUE )
print(0.00212/0.0009765)

#Repeat6
# calculate B.F.
z = 8 ; N = 10
theta = 0.5
pDgTheta = theta^z * (1-theta)^(N-z)
print( pDgTheta )


# use scripts from Ex12.1 to calculate BF for Exp CvsPhython
a = 76.5; b = 33.5  #use 
openGraph(width=5,height=7)
BernBeta( c(a,b) , c(rep(0,N-z),rep(1,z)) , ROPE=c(0.48,0.52) ,
          plotType="Bars" , showCentTend="Mode" , showHDI=TRUE , showpD=TRUE )
print(0.005/0.00097656)

#Repeat7
# calculate B.F.
z = 4 ; N = 5
theta = 0.5
pDgTheta = theta^z * (1-theta)^(N-z)
print( pDgTheta )


# use scripts from Ex12.1 to calculate BF for Exp CvsPhython
a = 84.5; b = 35.5  #use 
openGraph(width=5,height=7)
BernBeta( c(a,b) , c(rep(0,N-z),rep(1,z)) , ROPE=c(-Inf,0.50) ,
          plotType="Bars" , showCentTend="Mode" , showHDI=TRUE , showpD=TRUE )
print(0.0718/0.03125)




#Repeat z = 17 ; N = 28  a = 0.01; b = 0.01
# calculate B.F.
z = 17 ; N = 28
theta = 0.5
pDgTheta = theta^z * (1-theta)^(N-z)
print( pDgTheta )


# use scripts from Ex12.1 to calculate BF for Exp CvsPhython
a = 0.01; b = 0.01  #use 
openGraph(width=5,height=7)
BernBeta( c(a,b) , c(rep(0,N-z),rep(1,z)) , ROPE=c(0.48,0.52) ,
          plotType="Bars" , showCentTend="Mode" , showHDI=TRUE , showpD=TRUE )

print(3.44e-11/3.72529e-09)



#Repeat z = 17 ; N = 28  a = 0.01; b = 0.01
# shiyan wuxuxian
z = 17 ; N = 28
theta = 0.5
pDgTheta = theta^z * (1-theta)^(N-z)
print( pDgTheta )


# use scripts from Ex12.1 to calculate BF for Exp CvsPhython
a = 0.01; b = 0.01  #use 
openGraph(width=5,height=7)
BernBeta( c(a,b) , c(rep(0,N-z),rep(1,z)) , ROPE=c(-Inf,0.50) ,
          plotType="Bars" , showCentTend="Mode" , showHDI=TRUE , showpD=TRUE )

print(3.44e-11/3.72529e-09)