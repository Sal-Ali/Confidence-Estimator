bootstrap.simulator <- function(somevector = NOAA[,2]){
  norm <- rlnorm(somevector)
  
  
  
  sim.function()
  
  
  
  sim.function(F)
  
}


sim.function <- function(check = T){
  if(check == T){
    print("computing alpha = 0.1")
    list(
      Alpha.One.N.is.Five = sim(3,5,1000,F),
      Alpha.One.N.is.Ten = sim(3,10,1000,F),
      Alpha.One.N.is.Fifteen = sim(3,15,1000,F),
      Alpha.One.N.is.Thirty = sim(3,30,1000,F)
    )
  }
  
  
  
  if(check == F){
    print("computing alpha = 0.5")
    list(
      Alpha.Five.N.is.Five = sim(3,5,1000,T),
      Alpha.Five.N.is.Ten = sim(3,10,1000,T),
      Alpha.Five.N.is.Fifteen = sim(3,15,1000,T),
      Alpha.Five.N.is.Thirty= sim(3,30,1000,T) 
    )
  }
  
  
}


my.bootstrapci.ml <- function(vec0 = norm,nboot=10000,alpha = 0.1 )
{
  #extract sample size, mean and standard deviation from the original data
  n0<-length(vec0)
  mean0<-mean(vec0)
  sd0<-sqrt(var(vec0))
  # create a vector to store the location of the bootstrap studentized deviation vector
  bootvec<-NULL
  #create the bootstrap distribution using a for loop
  for( i in 1:nboot){
    vecb<-sample(vec0,replace=T)
    #create mean and standard deviation to studentize
    meanb<-mean(vecb)
    sdb<-sqrt(var(vecb))
    #note since resampling full vector we can use n0 for sample size of vecb
    bootvec<-c(bootvec,(meanb-mean0)/(sdb/sqrt(n0)))
  }
  #Calculate lower and upper quantile of the bootstrap distribution
  lq<-quantile(bootvec,alpha/2)
  uq<-quantile(bootvec,1-alpha/2)
  #incorporate into the bootstrap confidence interval (what algebra supports this?) and output result
  LB<-mean0-(sd0/sqrt(n0))*uq
  UB<-mean0-(sd0/sqrt(n0))*lq
  #since I have the mean and standard deviation calculate the normal confidence interval here as well
  NLB<-mean0-(sd0/sqrt(n0))*qnorm(1-alpha/2)
  NUB<-mean0+(sd0/sqrt(n0))*qnorm(1-alpha/2)
  list(bootstrap.confidence.interval=c(LB,UB),normal.confidence.interval=c(NLB,NUB))
}

sim <- function(mu.val=3,n=30,nsim=1000, check = F)
{
  #create coverage indicator vectors for bootstrap and normal
  cvec.boot<-NULL
  cvec.norm<-NULL
  #calculate real mean
  mulnorm<-(exp(mu.val+1/2))
  #run simulation
  for(i in 1:nsim){
    if((i/500)==floor(i/500)){
      print(i)
      print("working")
      #let me know computer hasnt died
    }
    #sample the simulation vector
    vec.sample<-rlnorm(n,mu.val)
    #bootstrap it
    if(check == F){
      boot.list<-my.bootstrapci.ml(vec.sample)
    }
    if(check == T){
      boot.list<-my.bootstrapci.ml(vec.sample, 10000, 0.5)
    }
    boot.conf<-boot.list$bootstrap.confidence.interval
    norm.conf<-boot.list$normal.confidence.interval
    #calculate if confidence intervals include mu
    #count up the coverage by the bootstrap interval
    cvec.boot<-c(cvec.boot,(boot.conf[1]<mulnorm)*(boot.conf[2]>mulnorm))
    #count up the coverage by the normal theory interval
    cvec.norm<-c(cvec.norm,(norm.conf[1]<mulnorm)*(norm.conf[2]>mulnorm))
  }
  #calculate and output coverage probability estimates
  list(boot.coverage=(sum(cvec.boot)/nsim),norm.coverage=(sum(cvec.norm)/nsim))	
}

