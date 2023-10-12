qsim <- function(mf, mb, a.rate, trb, trf, tmb, tmf, maxb) {
  Nf <- rep(0, 2*60*60)  #initialization
  Nb <- rep(0, 2*60*60)
  Eq <- rep(0, 2*60*60)
  
  fqueue <- rep(0, mf)
  bqueue <- rep(0, mb)
  
  fcount <- 0
  bcount <- 0
  
  farrival <- rexp(1, a.rate)  
  fdeparture <- Inf
  bdeparture <- Inf
  #如此设置下面t不可能大于fdeparture
  
  
  for (t in 1:(2*60*60)) {       # run simulation
    
    if (t >= farrival) {    # check for new arrivals at the French station
      fcount <- fcount + 1
      fqueue[which.min(fqueue)] <- fcount
      farrival <- t + rexp(1, a.rate)
    }
    
    if (t >= fdeparture) {          # check for departures from the French station
      if (min(bqueue) == 0) {       # move a vehicle from the French to the British station
        #第25行：但英国火车站排队的人最多只能排到20辆汽车，当这种情况发生时，（英国单个检查站的队列长度限制为20）
        #在至少一个英国队列中再次出现空位之前，没有一辆车可以从法国车站继续前进。
        #此时法国车站队列已有的车辆数量不变，但队列中仍然在增加新的车辆-队列长度的增加
        bcount <- bcount + 1
        bqueue[which.min(bqueue)] <- bcount
      } else {
        fqueue[which.min(fqueue)] <- fcount
      }
      
      fdeparture <- t + runif(1, tmf, tmf + trf)      # next departure time from the French station
      #第33和41行 要求处理时间服从均匀分布
      Nb[t] <- sum(bqueue > 0)      # update the number of vehicles at the British station
      #表示英国队列在每模拟秒内的平均长度
    }
    
    if (t >= bdeparture) {    # departures from the British station
      bqueue[which.min(bqueue)] <- 0
      
      bdeparture <- t + runif(1, tmb, tmb + trb)      # the next departure time from the British station
    }
    
    #英国和法国的队列都要考虑进车和出车两端的情况，进车是第4条模型假设的描述；出车是基于两个国家检查站的处理效率
    #进车给的数据是每秒到达概率0.1 出车的判断标准给的是平均处理时间--二者描述尺度不同，写入if循环的判断标准可能不同
    
    Nf[t] <- sum(fqueue > 0)
    Nb[t] <- sum(bqueue > 0)    # the queue lengths at the French and British stations
    
    Eq[t] <- sum(fqueue[1:min(fcount, length(fqueue))] * (t - (1:min(fcount, length(fqueue))))) / min(fcount, length(fqueue))    # the average expected waiting time at the front of the French queue
  }
  
  return(list(nf = Nf, nb = Nb, eq = Eq))
}

sim_def <- qsim(mf = 5, mb = 5, a.rate = 0.1, trb = 40, trf = 40, tmb = 30, tmf = 30, maxb = 20) # simulation with default parameters

sim40 <- qsim(mf = 5, mb = 40, a.rate = 0.1, trb = 40, trf = 40, tmb = 30, tmf = 30, maxb = 20) # simulation with mb set to 40

par(mfrow = c(2, 2))

plot(sim_def$nf, type = "l", xlab = "Time (seconds)", ylab = "French Queue Length", main = "French Queue Length")
lines(sim_def$nb, col = "blue")
legend("topright", c("French", "British"), col = c("black", "blue"), lty = 1)# Plot of French and British queue lengths over time (default parameters)

plot(sim_def$eq, type = "l", xlab = "Time (seconds)", ylab = "Expected Queuing Time", main = "Expected Queuing Time")# Plot of expected queuing time over time (default parameters)

plot(sim40$nf, type = "l", xlab = "Time (seconds)", ylab = "French Queue Length", main = "French Queue Length (mb = 40)")# Plot of French and British queue lengths over time (mb = 40 seconds)
lines(sim40$nb, col = "blue")

run <- function() {           # Function to run simulation and check if at least one car misses the ferry
  result <- qsim(mf = 5, mb = 5, a.rate = 0.1, trb = 40, trf = 40, tmb = 30, tmf = 30, maxb = 20)
  return(max(result$nf[length(result$nf)], result$nb[length(result$nb)]) > 0)
}

num <- 100

results <- replicate(num, run())

probability <- sum(results) / num # calculate probability of at least one car missing the ferry

cat("Probability of at least one car missing the ferry:", probability, "\n") # result
