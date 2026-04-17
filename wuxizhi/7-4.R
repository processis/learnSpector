dotR <- file.path(Sys.getenv("HOME"), ".R")
if (!file.exists(dotR)) dir.create(dotR)
M <- file.path(dotR, ifelse(.Platform$OS.type == "windows", "Makevars.win", "Makevars"))

if (file.exists(M)) file.create(M)
cat("\nCXX14FLAGS=-O3 -march=native -mtune=native",
    if(grepl("^darwin", R.version$os))
    "CXX14FLAGS += -arch x86_64 -ftemplate-depth-256"

else
  if (.Platform$OS.type == "windows") 
    "CXX11FLAGS=-O3 -march=native -mtune=native"
  
else
  "CXX14FLAGS += -fPIC",
file = M, sep = "\n", append = TRUE)

dat = scan("toy.txt")
toy_data = list(Y = data, N = length(dat))

toy = "data{
  int N;
  vector [N] Y;
}

parameters{
  real <lower=0> lambda;
}

model{
  lambda ~ gamma (1,1);
  Y ~ exponential(lambda);
}

generated quantities{
  real pred;
  pred = exponential_rng(lambda);
"



library(rstan)

fit=stan(model_code = toy,data = toy_data,
         warmup=1000,iter=2000,chain=2)

print(fit)

chain=as.matrix(fit)
acf(chain[,"lambda"],500)

traceplot(fit)

hist(toy_data$Y,prob=T)
lines(density(chain[,'pred']))

library(hdrcde)

hdr.den(chain[,'pred'])

library(gridExtra)

grid.arrange(traceplot(fit,pars="lambda"),
             traceplot(fit2,pars="lambda"),
             traceplot(fit3,pars="lambda"),
             traceplot(fit4,pars="lambda"),nrow=1)

hist(toy_data$Y,probability=T)
lines(density(as.matrix(fit)[,"pred"]),lwd=2)

lines(density(as.matrix(fit2)[,"pred"]),lwd=2,col=2,lty=2)
lines(density(as.matrix(fit3)[,"pred"]),lwd=2,col=3,lty=3)
lines(density(as.matrix(fit4)[,"pred"]),lwd=2,col=4,lty=4)

legend(5.2,.75,c("Gamma prior","Normal prior",
                 "Uniform prior"),lwd=2,lty=1:4,col=1:4)

