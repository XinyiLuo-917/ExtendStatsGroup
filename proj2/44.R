#### 1. qsim函数
#创造一个名为qsim的function，用来模拟汽车在法国轮渡码头先后通过法国和英国的护照检查。
qsim <- function(mf, mb, a.rate, trb, trf, tmb, tmf, maxb) { 
  #该函数包含八个参数，mf表示法国检查站的数量，mb表示英国检查站的数量，a.rate表示每秒到达一辆汽车的概率，
  #trb，trf，tmb，tmf是与检查站处理时间有关的时间常数，maxb表示英国检查站最大排队长度
  Nf <- rep(0, 2*60*60)  #initialization，Nf表示每一模拟秒法国队列的平均长度
  Nb <- rep(0, 2*60*60)  #Nb表示每一模拟秒英国队列的平均长度
  
  ################
  Nf1 <- rep(0, 2*60*60)  #initialization 不太知道咋说，好像没有用到？
  Nb1 <- rep(0, 2*60*60)  #同上
  #################
  
  Eq <- rep(0, 2*60*60)  #Eq表示模拟过程中每秒法国检查站队列起点处一辆车的平均预期等待时间
  b <- c()  #看看英国队伍中是否有超过20个的,TRUE表示不超过，符合要求
  
  fqueue <- rep(0, mf)  #法国每个检查站的队列长度
  bqueue <- rep(0, mb)  #英国每个检查站的队列长度
  fcount <- 0  #法国检查站排队车辆总数
  bcount <- 0  #英国检查站排队车辆总数
  
  arrival <- c(rpois(1.5*60*60,a.rate),rep(0, 0.5*60*60))  #arrival包含前1.5小时内以a.rate的速率到达的符合泊松分布的事件次数，在后0.5小时内无车到达
  ###########此处有个问题，print后发现arrival的值存在2，即一秒内有两辆车到达，请看35行注释
  
  fdeparture <- rep(0, mf)  #法国每个检查站每检查完一辆车的时间点
  bdeparture <- rep(0, mb)  #英国每个检查站每检查完一辆车的时间点
  maxf <- round(2*60*60*a.rate/1.5)  #法国检查站最大排队长度
  fprocess <- matrix(rep(0,mf*maxf),nrow=mf,ncol=maxf)  #法国检查站对每个位置的车的处理时间矩阵
  bprocess <- matrix(rep(0,mb*maxb),nrow=mb,ncol=maxb)  #英国检查站对每个位置的车的处理时间矩阵
  
  #在前1.5小时内，按照一秒为单位对t进行迭代循环
  for(t in 1:(1.5*60*60)){
    if (arrival[t]>=1){ #one car come in 如果该秒内有车抵达检查站
      fcount <- fcount + arrival[t]  #fcount由于新车的加入而增加
      ################问题为，如果arrival为2，此处fcount会加上2，包括下方代码也都会按照两辆车来进行，但是题目中说不考虑一秒内有两辆车到达的情况，是否需要将此处的arrival[t]直接修改为1，使arrival[t]的值只在进行判断if条件是否成立时才会用到
      fq_num <- order(fqueue)[1:arrival[t]]  #使用order选择出最短的arrival[t]个法国队列
      ordf <- fqueue[fq_num] <- fqueue[fq_num] + 1 #将选择出来的最短队列车辆数+1
      for (j in 1:arrival[t]) {
        fprocess[fq_num[j],ordf[j]] <- runif(1, tmf, tmf + trf)  #随机生成一个符合U(tmf, tmf + trf)的数字作为该车辆的处理时间
      }
      fdeparture[(fqueue[fq_num]==1)] <- fprocess[(fqueue[fq_num]==1),1] + fdeparture[(fqueue[fq_num]==1)]  #更新法国站车辆检查完的时间点
    }
    
    fdeparture[fqueue==0] <- bdeparture[bqueue==0] <- t + 0.0001  ############使时间步一致（忘了这句话咋说来着）
    
    if (all(fdeparture > t)) { #所有法国检查站在新车辆抵达时还未检查完前车
      if(any(bdeparture <= t)){  #此时存在英国检查站检查完前车，车辆要离开英国
        bcount <- bcount - length(bdeparture[bdeparture <= t])  #bcount由于车辆的离开而减少
        bqueue[bdeparture <= t] <- bqueue[bdeparture <= t] - 1  #对于有车辆离开的英国队列车辆数-1
        bprocess[(bdeparture <= t),-maxb] <- bprocess[(bdeparture <= t),-1]  ############不知道咋说
        bprocess[(bdeparture <= t),maxb] <- 0
        bdeparture[bdeparture <= t] <- bprocess[bdeparture <= t,1] + bdeparture[bdeparture <= t]  #更新英国站车辆检查完的时间点
      }
    } 
    else { #存在法国检查站在新车辆抵达时已经检查完前车
      if(all(bqueue >=20 )){ #英国检查站所有队列长度均>=20
        if(any(bdeparture <= t)){ #此时存在英国检查站检查完前车，车辆要离开英国
          bcount <- bcount - length(bdeparture[bdeparture <= t])  #bcount由于车辆的离开而减少
          bqueue[bdeparture <= t] <- bqueue[bdeparture <= t] - 1  #对于有车辆离开的英国队列车辆数-1
          bprocess[(bdeparture <= t),-maxb] <- bprocess[(bdeparture <= t),-1]  ############不知道咋说
          bprocess[(bdeparture <= t),maxb] <- 0
          bdeparture[bdeparture <= t] <- bprocess[bdeparture <= t,1] + bdeparture[bdeparture <= t]  #更新bdeparture
        }
      } 
      else {  #车辆满足离开法国进入英国的条件
        nbi <- mb*maxb - bcount  #英国队列最多可进入车辆数
        nfi <- min(length(fdeparture[fdeparture <= t]),nbi)  #法国队列最多可离开车辆
        fcount <- fcount - nfi  #fcount由于车辆离开法国进入英国而减少
        ord1 <- order(fdeparture[fdeparture <= t])[1:nfi]  #当英国可进入车辆数<=法国需要转出的车辆数时，遵循先到先得原则，排序并找出等待时间更长的车辆
        fqueue[fdeparture <= t][ord1] <- fqueue[fdeparture <= t][ord1] - 1  #更新fqueue
        if (nfi > 1){ #法国队列可离开车辆多于一辆
          fprocess[(fdeparture <= t),-maxf][ord1,] <- fprocess[(fdeparture <= t),-1][ord1,]  ##########不知道咋说
          fprocess[(fdeparture <= t),maxf][ord1] <- 0
        } 
        else { #法国队列可离开车辆不多于一辆
          fprocess[(fdeparture <= t),-maxf][ord1] <- fprocess[(fdeparture <= t),-1][ord1]  ##########不知道咋说
          fprocess[(fdeparture <= t),maxf][ord1] <- 0
        }
        fdeparture[fdeparture <= t][ord1] <- (fprocess[(fdeparture <= t),1][ord1]  + fdeparture[fdeparture <= t][ord1])  #更新fdeparture
        
        #车辆进入英国队列
        if(all(bdeparture>t)){ #英国站在法国车辆抵达时未检查完前车，即英国站有车进入但无车离开
          bcount <- bcount + nfi  #bcount由于法国车辆的到来而增加
          bq_num <- order(bqueue)[1:nfi]  #使用order选择出最短的nfi个英国队列
          ordb <- bqueue[bq_num] <- bqueue[bq_num] + 1  #将选择出来的最短队列车辆数+1
          for (j in 1:nfi) {
            bprocess[bq_num[j],ordb[j]] <- runif(1, tmb, tmb + trb)  #随机生成一个符合U(tmb, tmb + trb)的数字作为该车辆的处理时间
          }
          bdeparture[(bqueue[bq_num]==1)] <- bprocess[(bqueue[bq_num]==1),1] + bdeparture[(bqueue[bq_num]==1)]  #更新bdeparture
        } 
        else { #英国站在法国车辆抵达时已经检查完前车，即英国站有车进入且有车离开
          bcount <- bcount + nfi  #bcount由于法国车辆的到来而增加
          bq_num <- order(bqueue)[1:nfi]  #使用order选择出最短的nfi个英国队列
          ordb <- bqueue[bq_num] <- bqueue[bq_num] + 1  #将选择出来的最短队列车辆数+1
          for (j in 1:nfi) {
            bprocess[bq_num[j],ordb[j]] <- runif(1, tmb, tmb + trb)  #随机生成一个符合U(tmb, tmb + trb)的数字作为该车辆的处理时间
          }
          bdeparture[(bqueue[bq_num]==1)] <- bprocess[(bqueue[bq_num]==1),1] + bdeparture[(bqueue[bq_num]==1)]  #更新bdeparture
          
          #车辆离开英国队列
          bcount <- bcount - length(bdeparture[bdeparture <= t])  #bcount由车辆的离开而减少
          bqueue[bdeparture <= t] <- bqueue[bdeparture <= t] - 1  #对于有车辆离开的英国队列车辆数-1
          bprocess[(bdeparture <= t),-maxb] <- bprocess[(bdeparture <= t),-1]  ###########不知道咋说
          bprocess[(bdeparture <= t),maxb] <- 0
          bdeparture[bdeparture <= t] <- bprocess[bdeparture <= t,1] + bdeparture[bdeparture <= t]  #更新bdeparture
        }
      }
    }
    
    Nf[t] <- sum(fqueue)/mf  #计算每一模拟秒法国队列平均长度
    Nb[t] <- sum(bqueue)/mb  #计算每一模拟秒英国队列平均长度
    Nf1[t] <- fcount/mf  ##########好像没用到？
    Nb1[t] <- bcount/mb
    Eq[t] <- Nf[t]*mean(tmf,tmf+trf)  + Nb[t]*mean(tmb,tmb+trb)  #根据每个车站的平均队列长度和平均处理时间计算每秒法国站队列起点处一辆车的平均预期等待时间
    b[t]<-(bqueue[which.max(bqueue)]<=20)  #看看英国队伍中是否有超过20个的,TRUE表示不超过，符合要求
  }
  
  #在最后0.5小时内，按照一秒为单位对t进行迭代循环，此时无车辆进入法国
  for(t in (1.5*60*60+1):(2*60*60)){
    
    fdeparture[fqueue==0] <- bdeparture[bqueue==0] <- t + 0.0001  #########
    
    if (all(fdeparture > t)) { #所有法国检查站在新车辆抵达时还未检查完前车
      if(any(bdeparture <= t)){  #此时存在英国检查站检查完前车，车辆要离开英国
        bcount <- bcount - length(bdeparture[bdeparture <= t])  #bcount由车辆的离开而减少
        bqueue[bdeparture <= t] <- bqueue[bdeparture <= t] - 1   #对于有车辆离开的英国队列车辆数-1
        bprocess[(bdeparture <= t),-maxb] <- bprocess[(bdeparture <= t),-1]  ################
        bprocess[(bdeparture <= t),maxb] <- 0
        bdeparture[bdeparture <= t] <- bprocess[bdeparture <= t,1] + bdeparture[bdeparture <= t]   #更新bdeparture
      }
    } 
    else {  #存在法国检查站在新车辆抵达时已经检查完前车
      if(all(bqueue >=20 )){  #英国检查站所有队列长度均>=20
        if(any(bdeparture <= t)){  #此时存在英国检查站检查完前车，车辆要离开英国
          bcount <- bcount - length(bdeparture[bdeparture <= t])  #bcount由车辆的离开而减少
          bqueue[bdeparture <= t] <- bqueue[bdeparture <= t] - 1   #对于有车辆离开的英国队列车辆数-1
          bprocess[(bdeparture <= t),-maxb] <- bprocess[(bdeparture <= t),-1]  ####################3
          bprocess[(bdeparture <= t),maxb] <- 0
          bdeparture[bdeparture <= t] <- bprocess[bdeparture <= t,1] + bdeparture[bdeparture <= t]  #更新bdeparture
        }
      } 
      else {  #车辆满足离开法国进入英国的条件
        nbi <- mb*maxb - bcount  #英国队列最多可进入车辆数
        nfi <- min(length(fdeparture[fdeparture <= t]),nbi)  #法国队列最多可离开车辆
        fcount <- fcount - nfi  #fcount由于车辆离开法国进入英国而减少
        ord1 <- order(fdeparture[fdeparture <= t])[1:nfi]  #当英国可进入车辆数<=法国需要转出的车辆数时，遵循先到先得原则，排序并找出等待时间更长的车辆
        fqueue[fdeparture <= t][ord1] <- fqueue[fdeparture <= t][ord1] - 1  #更新fqueue
        if (nfi > 1){
          fprocess[(fdeparture <= t),-maxf][ord1,] <- fprocess[(fdeparture <= t),-1][ord1,]  ###########
          fprocess[(fdeparture <= t),maxf][ord1] <- 0
        } 
        else {
          fprocess[(fdeparture <= t),-maxf][ord1] <- fprocess[(fdeparture <= t),-1][ord1]  ##################
          fprocess[(fdeparture <= t),maxf][ord1] <- 0
        }
        fdeparture[fdeparture <= t][ord1] <- (fprocess[(fdeparture <= t),1][ord1]  + fdeparture[fdeparture <= t][ord1])  #更新fdeparture
        
        #车辆进入英国队列
        if(all(bdeparture>t)){  #英国站在法国车辆抵达时未检查完前车，即英国站有车进入但无车离开
          bcount <- bcount + nfi  #bcount由于法国车辆的到来而增加
          bq_num <- order(bqueue)[1:nfi]  #使用order选择出最短的nfi个英国队列
          ordb <- bqueue[bq_num] <- bqueue[bq_num] + 1  #将选择出来的最短队列车辆数+1
          for (j in 1:nfi) {
            bprocess[bq_num[j],ordb[j]] <- runif(1, tmb, tmb + trb)  #随机生成一个符合U(tmb, tmb + trb)的数字作为该车辆的处理时间
          }
          bdeparture[(bqueue[bq_num]==1)] <- bprocess[(bqueue[bq_num]==1),1] + bdeparture[(bqueue[bq_num]==1)]   #更新bdeparture
        } 
        else {  #英国站在法国车辆抵达时已经检查完前车，即英国站有车进入且有车离开
          bcount <- bcount + nfi  #bcount由于法国车辆的到来而增加
          bq_num <- order(bqueue)[1:nfi]  #使用order选择出最短的nfi个英国队列
          ordb <- bqueue[bq_num] <- bqueue[bq_num] + 1  #将选择出来的最短队列车辆数+1
          for (j in 1:nfi) {
            bprocess[bq_num[j],ordb[j]] <- runif(1, tmb, tmb + trb)  #随机生成一个符合U(tmb, tmb + trb)的数字作为该车辆的处理时间
          }
          bdeparture[(bqueue[bq_num]==1)] <- bprocess[(bqueue[bq_num]==1),1] + bdeparture[(bqueue[bq_num]==1)]   #更新bdeparture
          
          ##车辆离开英国队列
          bcount <- bcount - length(bdeparture[bdeparture <= t])  #bcount由车辆的离开而减少
          bqueue[bdeparture <= t] <- bqueue[bdeparture <= t] - 1  #对于有车辆离开的英国队列车辆数-1
          bprocess[(bdeparture <= t),-maxb] <- bprocess[(bdeparture <= t),-1]  ##################
          bprocess[(bdeparture <= t),maxb] <- 0
          bdeparture[bdeparture <= t] <- bprocess[bdeparture <= t,1] + bdeparture[bdeparture <= t]  #更新bdeparture
        }
      }
    }
    
    Nf[t] <- sum(fqueue)/mf  #计算每一模拟秒法国队列平均长度
    Nb[t] <- sum(bqueue)/mb  #计算每一模拟秒英国队列平均长度
    Nf1[t] <- fcount/mf  ##########
    Nb1[t] <- bcount/mb  #############
    Eq[t] <- Nf[t]*mean(tmf,tmf+trf)  + Nb[t]*mean(tmb,tmb+trb)  #根据每个车站的平均队列长度和平均处理时间计算每秒法国站队列起点处一辆车的平均预期等待时间
    b[t]<-(bqueue[which.max(bqueue)]<=20)  #看看英国队伍中是否有超过20个的,TRUE表示不超过，符合要求
  }
  
  fb_fin <- (fcount==0&bcount==0)  #寻找fcount和bcount均为0的
  return(list(nf = Nf, nb = Nb,Eq=Eq,Time=c(1:t),fb_fin=fb_fin,bprocess=bprocess,fprocess=fprocess,b=b,bcount=bcount,fcount=fcount,fdeparture=fdeparture,bdeparture=bdeparture))
}

sim <- qsim(mf = 5, mb = 5, a.rate = 0.1, trb = 40, trf = 40, tmb = 30, tmf = 30, maxb = 20)  #simulation with default parameters


### 2. 使用 qsim 函数生成一个 4 面板 2 行 2 列的图
sim1 <- qsim(mf = 5, mb = 5, a.rate = 0.1, trb = 40, trf = 40, tmb = 30, tmf = 30, maxb = 20)  #simulation with default parameters
sim2 <- qsim(mf = 5, mb = 5, a.rate = 0.1, trb = 40, trf = 40, tmb = 40, tmf = 30, maxb = 20)  #simulation with modified parameters
#par(lwd=1.5,cex=1.5,font.lab=2)
layout(matrix(c(1,2,3,4),2,2,byrow=TRUE),widths = )
plot(y=sim1$nf,x=sim1$Time,ylim=c(0,max(c(sim1$nf+sim1$nb)+5)),type="l",col='red',xlab = "Time",lty=1,ylab = "Queue Lengths")
lines(y=sim1$nb,x=sim1$Time,type="l",col='blue',lty=2)
legend("topleft",title="Queue Type", c("French","British"),lty=c(1,2),col=c("red","blue"),cex = 0.20,lwd=2)

plot(y=sim1$Eq,x=sim1$Time,ylim=c(0,max(sim1$Eq+100)),type="l",col='black',xlab = "Time",lty=1,ylab = "The Expected Queuing Time (s)")

plot(y=sim2$nf,x=sim2$Time,ylim=c(0,max(c(sim2$nf+sim2$nb)+5)),type="l",col='red',xlab = "Time",lty=1,ylab = "Queue Lengths")
lines(y=sim2$nb,x=sim2$Time,type="l",col='blue',lty=2)
legend("topleft",title="Queue Type", c("French","British"),lty=c(1,2),col=c("red","blue"),cex = 0.20)

plot(y=sim2$Eq,x=sim2$Time,ylim=c(0,max(sim2$Eq+100)),type="l",col='black',xlab = "Time",lty=1,ylab = "The Expected Queuing Time (s)")


### 3. 通过运行100次你的qsim函数估计至少一辆汽车错过渡轮出发的概率（即在模拟结束时仍在队列中）
sim_result <- NULL
set.seed(10)
for (simi in 1:100) {  #Simulate 100 times.
  simui <- qsim(mf = 5, mb = 5, a.rate = 0.10, trb = 40, trf = 40, tmb = 30, tmf = 30, maxb = 20) #simulation with default parameters
  sim_result <- c(sim_result,simui$fb_fin)
}
Fai0 <- ifelse(sim_result,0,1)  #######这会儿脑子不转了
p_value <- mean(Fai0)
p_value

length(Fai0[Fai0==1])  #返回100次模拟中至少一辆汽车错过渡轮出发的次数