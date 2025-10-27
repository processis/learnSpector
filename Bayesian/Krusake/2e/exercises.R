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
 
 