binary.HMM.pn2pw <- function(m,pi,gamma)
  # Transforms the natural parameters for a binary-HMM
  # to a vector of working parameters, 'parvect'.
{
  tpi <- log(pi/(1-pi))
  tgamma <- NULL
  if(m > 1)
  {
    tau <- log(gamma/diag(gamma))
    tgamma <- tau[!diag(m)]
    # vector of non-diagonal elements of tau
  }
  parvect <- c(tpi,tgamma)
  parvect
}

binary.HMM.pw2pn <- function(m,parvect)
  # Transforms the vector of working parameters for a binary-HMM
  # to the natural parameters.
  # Calculates the stationary distribution for the Markov chain.
{
  epar <- exp(parvect)
  pi <- epar[1:m]/(1 + epar[1:m])
  gamma <- diag(m)
  if(m > 1)
  {
    gamma[!gamma] <- epar[(m+1):(m*m)]
    # fills in non-diagonal elements of gamma
    gamma <- gamma/apply(gamma,1,sum)
  }
  delta <- solve(t(diag(m) - gamma + 1),rep(1,m))
  list(pi=pi,gamma=gamma,delta=delta)
}

binary.HMM.mllk <- function(parvect,x,m)
  # Computes minus the log-likelihood for a stationary binary-HMM,
  # for a given vector 'parvect' of working parameters
  # and a given vector 'x' of observations.
{
  pn <- binary.HMM.pw2pn(m,parvect)   # the natural parameters
  if(m==1) return(-sum(x*log(pn$pi) + (1 - x)*log(1 - pn$pi)))
  n <- length(x)
  dbinary <- function(x,p){return(p*x + (1-p)*(1-x))}
  # binary probability function           
  allprobs <- outer(x,pn$pi,dbinary)
  # n x m outer product matrix of probabilities            
  allprobs <- ifelse(!is.na(allprobs),allprobs,1)
  # dealing with missing values
  # Now implements the algorithm:  
  lnw <- 0                                    
  phi <- pn$delta                             
  for (i in 1:n)                                    
  {                                                
    phi <- phi%*%pn$gamma*allprobs[i,]            
    sumphi <- sum(phi)                               
    lnw <- lnw + log(sumphi)                    
    phi <- phi/sumphi                            
  }                                               
  mllk <- -lnw                            
  mllk                                              
}    

binary.HMM.mle <- function(x,m,pi0,gamma0)
  # ML estimation for a stationary binary-HMM,
  # given the initial values 'pi0' and 'gamma0' for the natural parameters.
{                                                      
  parvect0 <- binary.HMM.pn2pw(m,pi0,gamma0)  # the working parameters         
  mod <- nlm(binary.HMM.mllk,parvect0,x=x,m=m)
  # using the nlm function to minimize minus the log-likelihood       
  pn <- binary.HMM.pw2pn(m,mod$estimate)
  # the MLE of the natural parameters        
  mllk <- mod$minimum   # the minimum attained                           
  np <- length(parvect0)                          
  AIC <- 2*(mllk+np)   # Akaike information criterion                            
  n <- sum(!is.na(x))                            
  BIC <- 2*mllk+np*log(n)   # Bayesian information criterion                  
  list(pi=pn$pi,gamma=pn$gamma,delta=pn$delta,   
       code=mod$code,mllk=mllk,AIC=AIC,BIC=BIC) 
  # 'code' indicates how nlm terminated  
}

binary.HMM.lalphabeta <- function(x,m,pi,gamma)
  # For a given vector 'x' of observations,
  # computes the logarithms of the forward and backward
  # probabilities in the form of m x n matrices.
{                                                           
  delta <- solve(t(diag(m)-gamma+1),rep(1,m))   
  n <- length(x)                                    
  lalpha <- lbeta <- matrix(NA,m,n)
  dbinary <- function(x,p){return(p*x + (1-p)*(1-x))}                       
  allprobs <- outer(x,pi,dbinary)                        
  # allprobs an n x m matrix                                     
  lnw <- 0                                    
  phi <- delta                             
  for (i in 1:n)                                           
  {                                                        
    phi <- phi%*%gamma*allprobs[i,]                   
    sumphi <- sum(phi)                                   
    lnw <- lnw + log(sumphi)                         
    phi <- phi/sumphi                                 
    lalpha[,i] <- log(phi) + lnw                            
  }                                                        
  lbeta[,n]  <- rep(0,m)                                     
  psi <- rep(1/m,m)                                   
  lns <- log(m)                                       
  for (i in (n-1):1)                                         
  {                                                        
    psi <- gamma%*%(allprobs[i+1,]*psi)               
    lbeta[,i] <- log(psi) + lns                            
    sumpsi <- sum(psi)                                   
    psi <- psi/sumpsi                                  
    lns <- lns + log(sumpsi)                         
  }                                                        
  list(la=lalpha,lb=lbeta)                                   
}

binary.HMM.state_probs <- function(x,m,pi,gamma)    
  # Calculates the conditional state probabilities       
{                                                           
  delta <- solve(t(diag(m)-gamma+1),rep(1,m)) 
  n <- length(x)                                    
  fb <- binary.HMM.lalphabeta(x,m,pi,gamma)                               
  la <- fb$la                                        
  lb <- fb$lb                                        
  c <- max(la[,n])
  # c introduced to reduce the chances of underflow                                   
  llk <- c+log(sum(exp(la[,n]-c)))                   
  stateprobs <- matrix(NA,ncol=n,nrow=m)                     
  for (i in 1:n) stateprobs[,i] <- exp(la[,i]+lb[,i]-llk)     
  stateprobs                                                
}                                                           

binary.HMM.local_decoding <- function(x,m,pi,gamma)   
  # Performs local decoding
{                                                           
  n <- length(x)
  stateprobs <- binary.HMM.state_probs(x,m,pi,gamma)      
  ild <- rep(NA,n)                                           
  for (i in 1:n) ild[i] <- which.max(stateprobs[,i])           
  ild                                                        
} 

binary.HMM.viterbi <- function(x,m,pi,gamma)
  # Viterbi algorithm for global decoding 
  # The xi scaled to have row sums one
  # in order to avoid problems of underflow
{                                                           
  delta <- solve(t(diag(m)-gamma+1),rep(1,m))   
  n <- length(x)
  dbinary <- function(x,p){return(p*x + (1-p)*(1-x))}                                     
  allprobs <- outer(x,pi,dbinary)                         
  xi <- matrix(0,n,m)                                
  phi <- delta*allprobs[1,]                          
  xi[1,] <- phi/sum(phi)                                 
  for (i in 2:n)                                             
  {                                                        
    phi <- apply(xi[i-1,]*gamma,2,max)*allprobs[i,]      
    xi[i,] <- phi/sum(phi)                                   
  }                                                        
  iv <- numeric(n)                                             
  iv[n] <- which.max(xi[n,])                             
  for (i in (n-1):1)                                         
    iv[i] <- which.max(gamma[,iv[i+1]]*xi[i,])               
  iv                                                         
}         

