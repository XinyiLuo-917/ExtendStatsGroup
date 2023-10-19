# TeamMember: Baosong Shi(S2530846); Xinyi Luo(S2518917); Ran Huo(S2559670)
# TeamWork Contribution: Baosong Shi(S2530846):Mainly responsible for code-writing and part of comments(40%);
# Xinyi Luo(S2518917)& Ran Huo(S2559670):Sorting out and construct code structures,debugging and improving details both in code and comments (30%vs30%);

# Description:
# Create a function 'qsim' which contains eight parameters to simulate cars passing through French and then British passport control stations at the French ferry terminal.
# 'mf' denoting the number of French stations and 'mb' denoting the British ones.
# 'a.rate' is the probability of a car arriving each second.
# 'trb, trf, tmb, tmf' are parameters related to the processing time at the stations, 'maxb' denotes the maximum queue length at the British stations.
# The result of this function contains three elements 'nf', 'nb' and 'eq', where 'nf' and 'nb' denote the average length of the French and British queues respectively.' eq' denotes the average expected waiting time of a car at the start of the French queue over the whole process in each second.

qsim <- function(mf, mb, a.rate, trb, trf, tmb, tmf, maxb) {
  ##Initialisation
  Nf <- rep(0, 2*60*60)  
  Nb <- rep(0, 2*60*60)
  Eq <- rep(0, 2*60*60)
  b <- c() #Create a null vector for checking
  
  fqueue <- rep(0, mf) 
  bqueue <- rep(0, mb) #Length of queues in each French/British station
  fcount <- 0 
  bcount <- 0 #Total vehicles in French/British station
  
  #"arrival" represents the number of vehicle arrivals per second, following a Poisson distribution with parameter a.rate
  arrival <- c(rpois(n=1.5*60*60,lambda=a.rate))
  
  fdeparture <- rep(0, mf)  
  bdeparture <- rep(0, mb)  #Corresponding moment for each vehicle checked in French/British single station
  maxf <- round(1.5*60*60*a.rate/2)  #Maximum french queue length (per station)
  fprocess <- matrix(rep(0,mf*maxf),nrow=mf,ncol=maxf) #Processing time at French stations for each car
  maxb <- maxb + 1 #Consider simultaneous vehicle arrivals and departures when British queues are full
  bprocess <- matrix(rep(0,mb*maxb),nrow=mb,ncol=maxb) #Processing time at British stations for each car
  
  
  ## Loop iterations on t in one-second units for the first 1.5 hours
  for(t in 1:(1.5*60*60)){
    #If a car arrives at the French station
    if (arrival[t]>=1){ 
      fcount <- fcount + 1
      fq_num <- order(fqueue,fdeparture)[1] #Which French queue should be added
      ordf <- fqueue[fq_num] <- fqueue[fq_num] + 1 #Shortest queue vehicles +1
      fprocess[fq_num,ordf] <- runif(1, tmf, tmf + trf) #Renew process time
      if (fqueue[fq_num]==1) {fdeparture[fq_num] <- fprocess[fq_num,1] + fdeparture[fq_num]} #Renew fdeparture
    }
    #Ensure that when the empty queue is updated, the current time point is aligned with the iteration time step
    fdeparture[fqueue==0] <- bdeparture[bqueue==0] <- t + 0.0001 
    
    ##Whether the car checking completed or not in French when the new car arrived.
    #If no completion
    if (all(fdeparture > t)) {  
      
      #Cars ready to leave British station existing
      if(any(bdeparture <= t)){  
        bcount <- bcount - length(bdeparture[bdeparture <= t])
        bqueue[bdeparture <= t] <- bqueue[bdeparture <= t] - 1 #Relating queue vehicles -1
        #Updating the processing time matrix to indicate that the remaining cars in the queue as a whole progress by one step
        bprocess[(bdeparture <= t),-maxb] <- bprocess[(bdeparture <= t),-1]
        bprocess[(bdeparture <= t),maxb] <- 0
        bdeparture[bdeparture <= t] <- bprocess[bdeparture <= t,1] + bdeparture[bdeparture <= t] #Renew bdeparture
      }
    } 
    
    else {
      ##Updates the bqueue and determines if there are still empty slots available
      #No empty slots available
      if(all((bqueue-ifelse((bdeparture < t),1,0)) >=20 )){ 
        (fdeparture[order(fdeparture)][1:length(fdeparture[fdeparture <= t])] <- 
           bdeparture[order(bdeparture)][1:length(fdeparture[fdeparture <= t])]) #Update the time at which congested vehicles could leave France

        if(any(bdeparture <= t)){ #Cars ready to leave British station existing 
          bcount <- bcount - length(bdeparture[bdeparture <= t])
          bqueue[bdeparture <= t] <- bqueue[bdeparture <= t] - 1 
          #Updating the processing time matrix to indicate that the remaining cars in the queue as a whole progress by one step
          bprocess[(bdeparture <= t),-maxb] <- bprocess[(bdeparture <= t),-1]
          bprocess[(bdeparture <= t),maxb] <- 0
          bdeparture[bdeparture <= t] <- bprocess[bdeparture <= t,1] + bdeparture[bdeparture <= t] #Renew bdeparture
        }
      } 
      #Empty slots existing
      else {  
        nbi <- mb*(maxb-1) - (bcount- length(bdeparture[bdeparture < t])) #Available slots number
        nfi <- min(length(fdeparture[fdeparture <= t]),nbi) #Max number could leave from French station(comparing with slots number)
        fcount <- fcount - nfi
        ordf1 <- order(fdeparture)[1:nfi] #Identify cars with longer waiting times (if slots are less than cars needed to leave)
        fqueue[ordf1] <- fqueue[ordf1] - 1 #Renew fqueue
        fprocess[ordf1,-maxf] <- fprocess[ordf1,-1]
        fprocess[ordf1,maxf] <- 0
        fdeparture[ordf1] <- fprocess[ordf1,1]  + fdeparture[ordf1] #Renew fdeparture
        
        ##The car transfer into British station
        ##Check if there is any cars could leave British
        #No cars could leave
        if(all(bdeparture>t)){ 
          bcount <- bcount + nfi
          for (j in 1:nfi) {
            bq_num <- order(bqueue,bdeparture)[1] #Which British queue should be added
            ordb <- bqueue[bq_num] <- bqueue[bq_num] + 1 #Shortest queue vehicles +1
            bprocess[bq_num,ordb] <- runif(1, tmb, tmb + trb) #Renew process time
            if (bqueue[bq_num]==1) {bdeparture[bq_num] <- bprocess[bq_num,1] + bdeparture[bq_num]} #Renew bdeparture
          }
        } 
        
        #Cars leaving the British queue
        else {
          bcount <- bcount + nfi
          for (j in 1:nfi) {
            bq_num <- order(bqueue,bdeparture)[1] #Which British queue should be added
            ordb <- bqueue[bq_num] <- bqueue[bq_num] + 1 #Shortest queue vehicles +1
            bprocess[bq_num,ordb] <- runif(1, tmb, tmb + trb) #Renew process time
            if (bqueue[bq_num]==1) {bdeparture[bq_num] <- bprocess[bq_num,1] + bdeparture[bq_num]} #Renew bdeparture
          }
          
          bcount <- bcount - length(bdeparture[bdeparture <= t])
          bqueue[bdeparture <= t] <- bqueue[bdeparture <= t] - 1
          #Updating the processing time matrix to indicate that the remaining cars in the queue as a whole progress by one step
          bprocess[(bdeparture <= t),-maxb] <- bprocess[(bdeparture <= t),-1]
          bprocess[(bdeparture <= t),maxb] <- 0
          bdeparture[bdeparture <= t] <- bprocess[bdeparture <= t,1] + bdeparture[bdeparture <= t] #Renew bdeparture
        }
        
        if (length(fdeparture[fdeparture <= t]) >= 1) {
          (fdeparture[order(fdeparture)][1:length(fdeparture[fdeparture <= t])] <- 
             bdeparture[order(bdeparture)][1:length(fdeparture[fdeparture <= t])]) }#Update the time at which congested vehicles could leave France
      }
    }
    
    ###Calculate the required outcome per simulated second
    Nf[t] <- sum(fqueue)/mf
    Nb[t] <- sum(bqueue)/mb
    Eq[t] <- Nf[t]*(tmf+tmf+trf)/2  + Nb[t]*(tmb+tmb+trb)/2 
    b[t]<-(bqueue[which.max(bqueue)]<=20)
  }
  
  
  ## Loop iterations on t in one-second units for the last 0.5 hours, when no car enters France
  for(t in (1.5*60*60+1):(2*60*60)){
    
    fdeparture[fqueue==0] <- bdeparture[bqueue==0] <- t + 0.0001
    
    ##Whether the car checking completed or not in French when the new car arrived.
    #If no completion
    if (all(fdeparture > t)) {
      if(any(bdeparture <= t)){  
        bcount <- bcount - length(bdeparture[bdeparture <= t])
        bqueue[bdeparture <= t] <- bqueue[bdeparture <= t] - 1 
        bprocess[(bdeparture <= t),-maxb] <- bprocess[(bdeparture <= t),-1]
        bprocess[(bdeparture <= t),maxb] <- 0
        bdeparture[bdeparture <= t] <- bprocess[bdeparture <= t,1] + bdeparture[bdeparture <= t] ##Renew bdeparture
      }
    } 
    
    else {
      if(all((bqueue-ifelse((bdeparture < t),1,0)) >=20 )){
        (fdeparture[order(fdeparture)][1:length(fdeparture[fdeparture <= t])] <- 
           bdeparture[order(bdeparture)][1:length(fdeparture[fdeparture <= t])]) 
        
        if(any(bdeparture <= t)){ 
          bcount <- bcount - length(bdeparture[bdeparture <= t])
          bqueue[bdeparture <= t] <- bqueue[bdeparture <= t] - 1 
          bprocess[(bdeparture <= t),-maxb] <- bprocess[(bdeparture <= t),-1]
          bprocess[(bdeparture <= t),maxb] <- 0
          bdeparture[bdeparture <= t] <- bprocess[bdeparture <= t,1] + bdeparture[bdeparture <= t] #Renew bdeparture
        }
      } 
      
      #Empty slots existing
      else {
        nbi <- mb*(maxb-1) - (bcount- length(bdeparture[bdeparture < t])) 
        nfi <- min(length(fdeparture[fdeparture <= t]),nbi) #Max number could leave from French station(comparing with slots number)
        fcount <- fcount - nfi
        ordf1 <- order(fdeparture)[1:nfi] #Identify cars with longer waiting times (if slots are less than cars needed to leave)
        fqueue[ordf1] <- fqueue[ordf1] - 1 
        fprocess[ordf1,-maxf] <- fprocess[ordf1,-1]
        fprocess[ordf1,maxf] <- 0
        fdeparture[ordf1] <- fprocess[ordf1,1]  + fdeparture[ordf1] #Renew fdeparture
        
        ##The car transfer into British station
        ##Check if there is any cars could leave British
        #No cars could leave
        if(all(bdeparture>t)){ 
          bcount <- bcount + nfi
          for (j in 1:nfi) {
            bq_num <- order(bqueue,bdeparture)[1]  
            ordb <- bqueue[bq_num] <- bqueue[bq_num] + 1 
            bprocess[bq_num,ordb] <- runif(1, tmb, tmb + trb) 
            if (bqueue[bq_num]==1) {bdeparture[bq_num] <- bprocess[bq_num,1] + bdeparture[bq_num]} 
          }
        } 
        
        #Cars leaving the British queue
        else {
          bcount <- bcount + nfi
          for (j in 1:nfi) {
            bq_num <- order(bqueue,bdeparture)[1]  
            ordb <- bqueue[bq_num] <- bqueue[bq_num] + 1 
            bprocess[bq_num,ordb] <- runif(1, tmb, tmb + trb) 
            if (bqueue[bq_num]==1) {bdeparture[bq_num] <- bprocess[bq_num,1] + bdeparture[bq_num]} 
          }
          bcount <- bcount - length(bdeparture[bdeparture <= t])
          bqueue[bdeparture <= t] <- bqueue[bdeparture <= t] - 1 
          bprocess[(bdeparture <= t),-maxb] <- bprocess[(bdeparture <= t),-1]
          bprocess[(bdeparture <= t),maxb] <- 0
          bdeparture[bdeparture <= t] <- bprocess[bdeparture <= t,1] + bdeparture[bdeparture <= t] 
        }
        
        if (length(fdeparture[fdeparture <= t]) >= 1) {
          (fdeparture[order(fdeparture)][1:length(fdeparture[fdeparture <= t])] <- 
             bdeparture[order(bdeparture)][1:length(fdeparture[fdeparture <= t])]) }
      }
    }
    
    ##Calculate the required outcome per simulated second
    Nf[t] <- sum(fqueue)/mf
    Nb[t] <- sum(bqueue)/mb
    Eq[t] <- Nf[t]*(tmf+tmf+trf)/2  + Nb[t]*(tmb+tmb+trb)/2 
    b[t]<-(bqueue[which.max(bqueue)]<=20)
  }
  
  fb_fin <- (fcount==0&bcount==0)#Determine whether cars are stranded at the end of the two-hour loop
  return(list(nf = Nf, nb = Nb,Eq=Eq,Time=c(1:t)))
}


##Plotting an image showing: 
#The average French/British queue length 
#The average expected waiting time for a car to pass through two inspections
sim1 <- qsim(mf = 5, mb = 5, a.rate = 0.1, trb = 40, trf = 40, tmb = 30, tmf = 30, maxb = 20) 
sim2 <- qsim(mf = 5, mb = 5, a.rate = 0.1, trb = 40, trf = 40, tmb = 40, tmf = 30, maxb = 20) 

layout(matrix(c(1,2,3,4),2,2,byrow=TRUE), widths= )

#'Red' refers to 'French' and 'Blue' refers to 'British'
plot(y=sim1$nf,x=sim1$Time,ylim=c(0,max(c(sim1$nf,sim1$nb)*1.1)),type="l",col='red',xlab = "Time (s)",lty=1,ylab = "Queue Lengths")
lines(y=sim1$nb,x=sim1$Time,type="l",col='blue',lty=2)
legend("topleft",inset =0,title="Queue Type", c("French","British"),lty=c(1,2),col=c("red","blue"),cex = 0.50,lwd=0.7, text.width=0.5,horiz=F,bty="n")

plot(y=sim1$Eq,x=sim1$Time,ylim=c(0,max(sim1$Eq)*1.1),type="l",col='black',xlab = "Time (s)",lty=1,ylab = "The Expected Queuing Time (s)")

plot(y=sim2$nf,x=sim2$Time,ylim=c(0,max(c(sim2$nf,sim2$nb)*1.1)),type="l",col='red',xlab = "Time (s)",lty=1,ylab = "Queue Lengths")
lines(y=sim2$nb,x=sim2$Time,type="l",col='blue',lty=2)
legend("topleft",inset =0,title="Queue Type", c("French","British"),lty=c(1,2),col=c("red","blue"),cex = 0.50,lwd=0.7, text.width=0.5,horiz=F,bty="n")

plot(y=sim2$Eq,x=sim2$Time,ylim=c(0,max(sim2$Eq)*1.1),type="l",col='black',xlab = "Time (s)",lty=1,ylab = "The Expected Queuing Time (s)")


##Estimate the probability that at least one car misses the ferry departures by using 100 runs
sim_result <- NULL
set.seed(10)

for (simi in 1:100) {
    simui <- qsim(mf = 5, mb = 5, a.rate = 0.10, trb = 40, trf = 40, tmb = 30, tmf = 30, maxb = 20) #simulation with default parameters
    sim_result <- c(sim_result,simui$fb_fin)
}

Fai0 <- ifelse(sim_result,0,1) #Returns the determination of 100 runs of the function
p_value <- mean(Fai0) #Calculate the probability
p_value

length(Fai0[Fai0==1])#Number of delays 


## Discussion on the implications of small extra delays in British checking:
# The change from a 30-second to a 40-second minimum processing time at the British station led to a notable increase in both the average british queue length (nf) and the average expected waiting time (eq). 
# Even though no cars missed the ferry departures, the time to process all remaining cars at the British station increased from 5500-6000 seconds to over 6000 seconds when no new cars arrived. 
# However, the data for the French station remained relatively stable.


