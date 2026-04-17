P=matrix(c(.5,0,.5,0,.8,.2,.6,.2,.2),3,3,b=T)
p0=c(.3,.1,.6)
p1=p0
for(i in 1:55)
  p1=p1%*%P
p1




M=20000;k=floor(M/2)

X=NULL
x=1

set.seed(1010)
for(i in 1:M){
  u=rnorm(1,0,1)
  alpha=dgamma(u,5,5)/dgamma(x,5,5)
  if(runif(1)<min(alpha,1))x=u
  X[i]=x
}

layout(t(1:2))
hist(X[-(1:k)],20,prob=TRUE,xlim=c(0,8),xlab='X',ylab="",main="")

curve(dgamma(x,5,5),from=0,to=8,add=TRUE,col=2,lwd=3)
legend('topright',c('true density'),col=2,lty=1)

plot(1:k,X[1:k],type='l',col=2,lty=2,ylab='X',xlab="index",
     xlim=c(1,M),ylim=range(X))

lines((k+1):M,X[(k+1):M])

legend('top',c('after buron-in','burn-in'),lty=1:2,col=1:2,cex=.6)


#################################

M=20000;k=floor(M/2)

set.seed(1010)

X=vector()
for(i in 1:M){
  ch<-rchisq(1,3)
  alpha<-(dgamma(ch,5,5)/dgamma(x,5,5))*(dchisq(ch,3)/dchisq(x,3))
  if(runif(1)<min(alpha,1))x=ch
  X[i]=x
  
         
}

par(mfrow=c(1,2))

hist(X[-(1:k)],15,prob=TRUE,xlim=c(0,8),xlab='X',ylab="",ylim=c(0,1),
     main="")

curve(dgamma(x,5,5),from=0,to=8,add=TRUE,col=2,lwd=3)
legend('topright',c('ture density'),col=2,lty=1)

plot(1:k,X[1 :k],type='l',col= 2,lty=2,ylab='X',xlab="index",xlim=c(l,M),
     ylim=range(X))

plot(1:k, X[1:k], type = 'l', col = 2, lty = 2, 
     ylab = 'X', xlab = "index", xlim = c(1, M), ylim = range(X))


lines((k+1):M, X[(k+1):M])
legend('top', c('after burn-in', 'burn-in'), lty = 1:2, col = 1:2, cex = 0.6)



############################

n=70;xbar=8;s2=4
M-99999;K=5000
mu=vector()-> tau
tau[1]=1
set.seed(1010)
for(i in 2:M){
  
  mu[i]=rnorm(n=1,mean =xbar,sd=sqrt(1/(n*tau[i-1])))
  tau[i]=rgamma(n=1,shape=n/2,scale=2/((n-1)*s2+n*(mu[i]-xbar)^2))
}
                    

par(mfrow=c(1,2))
hist(mu[-(1:k)],co1=4)
hist(tau[-(1:k)],co1=4)



########################

Hmc <- function(f, delta, q0, L, M) {
  require(numDeriv)          # 确保可用
  q <- numeric(M)
  U <- numeric(M)
  q[1] <- q0
  U[1] <- f(q0)
  
  for (i in 2:M) {
    # 1. 随机抽取初始动量
    p <- rnorm(1)            # 一维情况
    current_q <- q[i-1]
    current_p <- p
    current_U <- U[i-1]
    current_K <- sum(current_p^2) / 2
    
    # 2. Leapfrog 积分 L 步
    q_temp <- current_q
    p_temp <- current_p
    # 半步更新动量
    p_temp <- p_temp - 0.5 * delta * grad(f, q_temp)
    for (j in 1:(L-1)) {
      q_temp <- q_temp + delta * p_temp
      p_temp <- p_temp - delta * grad(f, q_temp)
    }
    q_temp <- q_temp + delta * p_temp
    # 最后半步更新动量
    p_temp <- p_temp - 0.5 * delta * grad(f, q_temp)
    # 可选：翻转动量（此处不翻转，接受率已对称）
    
    # 3. 计算接受概率
    proposed_U <- f(q_temp)
    proposed_K <- sum(p_temp^2) / 2
    accept_prob <- exp(current_U - proposed_U + current_K - proposed_K)
    
    # 4. Metropolis 接受/拒绝
    if (runif(1) < accept_prob) {
      q[i] <- q_temp
      U[i] <- proposed_U
    } else {
      q[i] <- q[i-1]
      U[i] <- U[i-1]
    }
  }
  return(list(chain = q, U = U))
}

library(numDeriv)
set.seed(101010)
f1 <- function(x) { x^(-6/2) * exp(-4/(2*x)) }   # 即 x^{-3} * exp(-2/x)
res <- Hmc(f1, delta = 0.3, q0 = 19, L = 16, M = 5000)
hist(res$chain)