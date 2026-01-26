# simulate a pancake and return randomly ordered sides
sim_pancake <- function() {
  pancake <- sample(1:3,1)
  sides <- matrix(c(1,1,1,0,0,0),2,3)[,pancake]
  sample(sides)
}
# sim 10,000 pancakes
pancakes <- replicate( 1e4 , sim_pancake() )
up <- pancakes[1,]
down <- pancakes[2,]
# compute proportion 1/1 (BB) out of all 1/1 and 1/0
num_11_10 <- sum( up==1 )
num_11 <- sum( up==1 & down==1 )
num_11/num_11_10


library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce
# points
plot( d$Divorce ~ d$MedianAgeMarriage , ylim=c(4,15) ,
      xlab="Median age marriage" , ylab="Divorce rate" )
# standard errors
for ( i in 1:nrow(d) ) {
  ci <- d$Divorce[i] + c(-1,1)*d$Divorce.SE[i]
  x <- d$MedianAgeMarriage[i]
  lines( c(x,x) , ci )
}

dlist <- list(
  D_obs = standardize( d$Divorce ),
  D_sd = d$Divorce.SE / sd( d$Divorce ),
  M = standardize( d$Marriage ),
  A = standardize( d$MedianAgeMarriage ),
  N = nrow(d)
)
m15.1 <- ulam(
  alist(
    D_obs ~ dnorm( D_true , D_sd ),
    vector[N]:D_true ~ dnorm( mu , sigma ),
    mu <- a + bA*A + bM*M,
    a ~ dnorm(0,0.2),
    bA ~ dnorm(0,0.5),
    bM ~ dnorm(0,0.5),
    sigma ~ dexp(1)
  ) , data=dlist , chains=4 , cores=4 )

precis( m15.1 , depth=2 )

dlist <- list(
  D_obs = standardize( d$Divorce ),
  D_sd = d$Divorce.SE / sd( d$Divorce ),
  M_obs = standardize( d$Marriage ),
  M_sd = d$Marriage.SE / sd( d$Marriage ),
  A = standardize( d$MedianAgeMarriage ),
  N = nrow(d)
)
m15.2 <- ulam(
  alist(
    D_obs ~ dnorm( D_true , D_sd ),
    vector[N]:D_true ~ dnorm( mu , sigma ),
    mu <- a + bA*A + bM*M_true[i],
    M_obs ~ dnorm( M_true , M_sd ),
    vector[N]:M_true ~ dnorm( 0 , 1 ),
    a ~ dnorm(0,0.2),
    bA ~ dnorm(0,0.5),
    bM ~ dnorm(0,0.5),
    sigma ~ dexp( 1 )
  ) , data=dlist , chains=4 , cores=4 )

post <- extract.samples( m15.2 )
D_true <- apply( post$D_true , 2 , mean )
M_true <- apply( post$M_true , 2 , mean )
plot( dlist$M_obs , dlist$D_obs , pch=16 , col=rangi2 ,
      xlab="marriage rate (std)" , ylab="divorce rate (std)" )
points( M_true , D_true )
for ( i in 1:nrow(d) )
  lines( c( dlist$M_obs[i] , M_true[i] ) , c( dlist$D_obs[i] , D_true[i] ) )

N <- 500
A <- rnorm(N)
M <- rnorm(N,-A)
D <- rnorm(N,A)
A_obs <- rnorm(N,A)

N <- 100
S <- rnorm( N )
H <- rbinom( N , size=10 , inv_logit(S) )

D <- rbern( N ) # dogs completely random
Hm <- H
Hm[D==1] <- NA

D <- ifelse( S > 0 , 1 , 0 )
Hm <- H
Hm[D==1] <- NA

set.seed(501)
N <- 1000
X <- rnorm(N)
S <- rnorm(N)
H <- rbinom( N , size=10 , inv_logit( 2 + S - 2*X ) )

D <- ifelse( X > 1 , 1 , 0 )
Hm <- H
Hm[D==1] <- NA

dat_list <- list(
  H = H,
  S = S )
m15.3 <- ulam(
  alist(
    H ~ binomial( 10 , p ),
    logit(p) <- a + bS*S,
    a ~ normal( 0 , 1 ),
    bS ~ normal( 0 , 0.5 )
  ), data=dat_list , chains=4 )
precis( m15.3 )

dat_list0 <- list( H = H[D==0] , S = S[D==0] )
m15.4 <- ulam(
  alist(
    H ~ binomial( 10 , p ),
    logit(p) <- a + bS*S,
    a ~ normal( 0 , 1 ),
    bS ~ normal( 0 , 0.5 )
  ), data=dat_list0 , chains=4 )
precis( m15.4 )

D <- ifelse( abs(X) < 1 , 1 , 0 )

N <- 100
S <- rnorm(N)
H <- rbinom( N , size=10 , inv_logit(S) )
D <- ifelse( H < 5 , 1 , 0 )
Hm <- H; Hm[D==1] <- NA

library(rethinking)
data(milk)
d <- milk
d$neocortex.prop <- d$neocortex.perc / 100
d$logmass <- log(d$mass)
dat_list <- list(
  K = standardize( d$kcal.per.g ),
  B = standardize( d$neocortex.prop ),
  M = standardize( d$logmass ) )

m15.5 <- ulam(
  alist(
    K ~ dnorm( mu , sigma ),
    mu <- a + bB*B + bM*M,
    B ~ dnorm( nu , sigma_B ),
    c(a,nu) ~ dnorm( 0 , 0.5 ),
    c(bB,bM) ~ dnorm( 0, 0.5 ),
    sigma_B ~ dexp( 1 ),
    sigma ~ dexp( 1 )
  ) , data=dat_list , chains=4 , cores=4 )

precis( m15.5 , depth=2 )

obs_idx <- which( !is.na(d$neocortex.prop) )
dat_list_obs <- list(
  K = dat_list$K[obs_idx],
  B = dat_list$B[obs_idx],
  M = dat_list$M[obs_idx] )
m15.6 <- ulam(
  alist(
    K ~ dnorm( mu , sigma ),
    mu <- a + bB*B + bM*M,
    B ~ dnorm( nu , sigma_B ),
    c(a,nu) ~ dnorm( 0 , 0.5 ),
    c(bB,bM) ~ dnorm( 0, 0.5 ),
    sigma_B ~ dexp( 1 ),
    sigma ~ dexp( 1 )
  ) , data=dat_list_obs , chains=4 , cores=4 )
precis( m15.6 )

plot( coeftab(m15.5,m15.6) , pars=c("bB","bM") )

post <- extract.samples( m15.5 )
B_impute_mu <- apply( post$B_impute , 2 , mean )
B_impute_ci <- apply( post$B_impute , 2 , PI )
# B vs K
plot( dat_list$B , dat_list$K , pch=16 , col=rangi2 ,
      xlab="neocortex percent (std)" , ylab="kcal milk (std)" )
miss_idx <- which( is.na(dat_list$B) )
Ki <- dat_list$K[miss_idx]
points( B_impute_mu , Ki )
for ( i in 1:12 ) lines( B_impute_ci[,i] , rep(Ki[i],2) )
# M vs B

plot( dat_list$M , dat_list$B , pch=16 , col=rangi2 ,
      ylab="neocortex percent (std)" , xlab="log body mass (std)" )
Mi <- dat_list$M[miss_idx]
points( Mi , B_impute_mu )
for ( i in 1:12 ) lines( rep(Mi[i],2) , B_impute_ci[,i] )

m15.7 <- ulam(
  alist(
    # K as function of B and M
    K ~ dnorm( mu , sigma ),
    mu <- a + bB*B_merge + bM*M,
    # M and B correlation
    MB ~ multi_normal( c(muM,muB) , Rho_BM , Sigma_BM ),
    matrix[29,2]:MB <<- append_col( M , B_merge ),
    # define B_merge as mix of observed and imputed values
    vector[29]:B_merge <- merge_missing( B , B_impute ),
    # priors
    c(a,muB,muM) ~ dnorm( 0 , 0.5 ),
    c(bB,bM) ~ dnorm( 0, 0.5 ),
    sigma ~ dexp( 1 ),
    Rho_BM ~ lkj_corr(2),
    Sigma_BM ~ dexp(1)
  ) , data=dat_list , chains=4 , cores=4 )
precis( m15.7 , depth=3 , pars=c("bM","bB","Rho_BM" ) )

B_missidx <- which( is.na( dat_list$B ) )

data(Moralizing_gods)
str(Moralizing_gods)

table( Moralizing_gods$moralizing_gods , useNA="always" )

symbol <- ifelse( Moralizing_gods$moralizing_gods==1 , 16 , 1 )
symbol <- ifelse( is.na(Moralizing_gods$moralizing_gods) , 4 , symbol )
color <- ifelse( is.na(Moralizing_gods$moralizing_gods) , "black" , rangi2 )
plot( Moralizing_gods$year , Moralizing_gods$population , pch=symbol ,
      col=color , xlab="Time (year)" , ylab="Population size" , lwd=1.5 )

with( Moralizing_gods ,
      table( gods=moralizing_gods , literacy=writing , useNA="always" ) )

haw <- which( Moralizing_gods$polity=="Big Island Hawaii" )
columns <- c("year","writing","moralizing_gods")
t( Moralizing_gods[ haw , columns ] )

set.seed(9)
N_houses <- 100L
alpha <- 5
beta <- (-3)
k <- 0.5
r <- 0.2

cat <- rbern( N_houses , k )
notes <- rpois( N_houses , alpha + beta*cat )
R_C <- rbern( N_houses , r )
cat_obs <- cat
cat_obs[R_C==1] <- (-9L)
dat <- list(
  notes = notes,
  cat = cat_obs,
  RC = R_C,
  N = as.integer(N_houses) )

m15.8 <- ulam(
  alist(
    # singing bird model
    ## cat known present/absent:
    notes|RC==0 ~ poisson( lambda ),
    log(lambda) <- a + b*cat,
    ## cat NA:
    notes|RC==1 ~ custom( log_sum_exp(
      log(k) + poisson_lpmf( notes | exp(a + b) ),
      log(1-k) + poisson_lpmf( notes | exp(a) )
    ) ),
    # priors
    a ~ normal(0,1),
    b ~ normal(0,0.5),
    # sneaking cat model
    cat|RC==0 ~ bernoulli(k),
    k ~ beta(2,2)
  ), data=dat , chains=4 , cores=4 )

m15.9 <- ulam(
  alist(
    # singing bird model
    notes|RC==0 ~ poisson( lambda ),
    notes|RC==1 ~ custom( log_sum_exp(
      log(k) + poisson_lpmf( notes | exp(a + b) ),
      log(1-k) + poisson_lpmf( notes | exp(a) )
    ) ),
    log(lambda) <- a + b*cat,
    a ~ normal(0,1),
    b ~ normal(0,0.5),
    # sneaking cat model
    cat|RC==0 ~ bernoulli(k),
    k ~ beta(2,2),
    # imputed values
    gq> vector[N]:PrC1 <- exp(lpC1)/(exp(lpC1)+exp(lpC0)),
    gq> vector[N]:lpC1 <- log(k) + poisson_lpmf( notes[i] | exp(a+b) ),
    gq> vector[N]:lpC0 <- log(1-k) + poisson_lpmf( notes[i] | exp(a) )
  ), data=dat , chains=4 , cores=4 )

library(rethinking)
data(Primates301)
d <- Primates301
cc <- complete.cases( d$brain , d$body )
B <- d$brain[cc]
M <- d$body[cc]
B <- B / max(B)
M <- M / max(M)

Bse <- B*0.1
Mse <- M*0.1

dat_list <- list( B = B , M = M )
m15H4 <- ulam(
  alist(
    B ~ dlnorm( mu , sigma ),
    mu <- a + b*log(M),
    a ~ normal(0,1),
    b ~ normal(0,1),
    sigma ~ exponential(1)
  ) , data=dat_list )

start=list( M_true=dat_list$M , B_true=dat_list$B )

library(rethinking)
data(Primates301)
d <- Primates301
colSums( is.na(d) )

cc <- complete.cases( d$body )
M <- d$body[cc]
M <- M / max(M)
B <- d$brain[cc]
B <- B / max( B , na.rm=TRUE )

start=list( B_impute=rep(0.5,56) )

