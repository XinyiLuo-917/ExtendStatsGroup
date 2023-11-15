

# This code mainly ....
# (set up a simple neural network for classification, and to train it using stochastic gradient descent.)
# The function set consists of four main functions:netup, forward, backward and train.
# 然后简要描述每个函数都做什么

# netup()
# Require a vector d containing the number of nodes in each layer of a network
# 输出什么
set.seed(1617)#设定种子数
netup <- function(d) {
  network <- list()#创建了一个空的列表network，用于存储神经网络的参数。
  #创建了一个具有length(d)个元素的列表，并赋予其赋值network$h。该列表将用于隐藏存储神经网络的层节点值。
  network$h <- vector("list", length(d))
  #创建了一个具有length(d)-1单个元素的列表，并赋予其赋值network$W。该列表将用于存储神经网络的权重
  network$W <- vector("list", length(d)-1)
  network$b <- vector("list", length(d)-1)
  
  # 初始化节点
  for (l in 1:(length(d)-1)) {
    #将一个大小为d[l]×1的全零矩阵分配给network$h第l一个元素。这个矩阵用于存储第l一个层的隐藏节点值。
    network$h[[l]] <- matrix(0, nrow = d[l], ncol = 1)
    #生成一个d[l+1]×d[l]大小的随机矩阵，并赋予其赋权的network$W第l一个元素。该矩阵表示连接第l一个层和l+1隐藏第一个层之间的权重。
    network$W[[l]] <- matrix(runif(d[l]*d[l+1], 0, 0.2), nrow = d[l+1], ncol = d[l])
    #network$b[[l]] <- matrix(runif(d[l]*d[l+1], 0, 0.2), nrow = d[l+1], ncol = 1)
    network$b[[l]] <- matrix(runif(d[l+1]*1, 0, 0.2), nrow = d[l+1], ncol = 1)
  }
  #将一个大小为d[4]×1的全零矩阵赋值给network$h最后一个元素。这个矩阵用于存储输出层的节点值。
  network$h[[length(d)]] <- matrix(0, nrow = d[4], ncol = 1) 
  
  return(network)
}

# forward(nn,inp)
# nn: a network list as returned by function netup(d)
# inp: a vector of input values for the first layer
# 输出什么 updated network list 
forward <- function(nn, inp) {
  nn$h[[1]] <- as.matrix(inp)  # 将输入数据转换为矩阵，并赋值给网络的第一个隐藏层节点值
  
  for (l in 1:(length(nn$h)-1)) {
    nn$h[[l+1]] <- pmax(0, nn$W[[l]] %*% nn$h[[l]] + nn$b[[l]])  # 计算下一层节点值
    nn$h[[l+1]] <- as.matrix(nn$h[[l+1]])  # 将下一层节点值转换为矩阵
  }
  
  return(nn)  # 返回更新后的神经网络
}

# backward(nn,k)
# nn: the updated network list returned from function forward(nn,inp)
# k: 指定的输出结果的类别数
backward <- function(nn, k) {
  n_layers <- length(nn$h)  # 神经网络的层数
  # 初始化导数
  dh <- vector("list", n_layers)  # 每一层的输出导数
  dW <- vector("list", n_layers-1)  # 权重的导数
  db <- vector("list", n_layers-1)  # 偏置的导数
  dd <- vector("list", n_layers)  # 临时矩阵d
  dd[[n_layers]] <- matrix(0, nrow = length(nn$h[[n_layers]]), ncol = 1)  # 临时矩阵d初始化为零矩阵
  dh[[n_layers]] <- matrix(0, nrow = length(nn$h[[n_layers]]), ncol = 1)  # 输出层的输出导数初始化为零矩阵
  
  # 计算损失的导数
  # 初始化损失导数的矩阵
  for (l in n_layers:2) {
    dd[[l-1]] <- matrix(0, nrow = length(nn$h[[l-1]]), ncol = 1)  # 初始化临时矩阵d为零矩阵
    dh[[l-1]] <- matrix(0, nrow = length(nn$h[[l-1]]), ncol = 1)  # 初始化上一层的输出导数为零矩阵
    dW[[l-1]] <- matrix(0, nrow = nrow(nn$W[[l-1]]), ncol = ncol(nn$W[[l-1]]))  # 初始化权重导数为零矩阵
    db[[l-1]] <- matrix(0, nrow = length(nn$h[[l-1]]), ncol = 1)  # 初始化偏置导数为零矩阵
  }
  
  # 反向传播
  # 根据公式计算输出类别 k 的损失导数
  # Compute the softmax and create the gradient for all outputs
  dh[[n_layers]] <- exp(nn$h[[n_layers]]) / sum(exp(nn$h[[n_layers]]))
  
  # Subtract 1 from the gradient of the correct class
  dh[[n_layers]][k] <- dh[[n_layers]][k] - 1
  
  # The gradients are now stored in dh[[n_layers]]
  dd[[n_layers]] <- dh[[n_layers]]
  
  # 计算其他层的损失导数
  for (l in n_layers:2) {
    dd[[l]] = ifelse(nn$h[[l]] <= 0, 0, dh[[l]])  # 根据激活函数的类型，计算上一层的输入导数
    dh[[l-1]] <- t(nn$W[[l-1]]) %*% dd[[l]]  # 计算上一层的输出导数
  }
  
  # 计算权重和偏置的导数
  # 仅仅计算梯度
  for (l in n_layers:2) {
    dW[[l-1]] <- dd[[l]] %*% t(nn$h[[l-1]])  # 计算权重的导数
    db[[l-1]] <- dd[[l]]  # 计算偏置的导数
  }
  
  nn$dh <- dh  # 将输出导数保存在神经网络对象中
  nn$dW <- dW  # 将权重导数保存在神经网络对象中
  nn$db <- db  # 将偏置导数保存在神经网络对象中
  
  return(nn)  # 返回更新后的神经网络对象
}

# train(nn,inp,k,eta=0.01, mb=10, nstep=10000)
# nn 是神经网络的列表，其中包含权重 W、偏置 b、梯度 dW 和 db
# eta 是学习率，mb 是批量大小
train <- function(nn, inp, k, eta = 0.01, mb = 10, nstep = 10000) {
  
  n_samples <- nrow(inp)  # 训练样本的数量
  for (step in 1:nstep) {
    # 随机抽取一个小批量样本
    indices <- sample(n_samples, mb)  # 生成随机索引
    inp_mb <- inp[indices, ]  # 从输入数据中抽取对应索引的小批量输入
    k_mb <- k[indices]  # 从标签数据中抽取对应索引的小批量标签
    
    for (i in 1:length(k_mb)) {
      # 前向传播
      nn <- forward(nn, inp_mb[i,])  # 将小批量输入传递给神经网络进行前向传播
      
      # 反向传播
      nn <- backward(nn, k_mb[i])  # 计算损失函数的梯度
      
      # 更新参数
      # 上述78-81行梯度计算乘以一个学习率eta，并且在计算梯度之后加上随机抽出的批量大小mb的归一化
      # 更新权重
      nn$W <- mapply(function(w, dw) w - eta * dw / mb, nn$W, nn$dW, SIMPLIFY = FALSE)
      
      # 更新偏置
      nn$b <- mapply(function(b, db) b - eta * db / mb, nn$b, nn$db, SIMPLIFY = FALSE)
      
    }
  }
  
  return(nn)  # 返回更新后的神经网络对象
}

#5: Train a network for iris classification
# 导入鸢尾花数据集
data(iris)
iris_train <- iris[-(1:150) %% 5 == 0, ]  # 从鸢尾花数据集中抽取训练集
iris_test <- iris[-(1:150) %% 5 != 0, ]  # 从鸢尾花数据集中抽取测试集

# 准备输入和输出数据
inp_train <- as.matrix(iris_train[, -5])  # 训练集输入数据
out_train <- as.numeric(iris_train[, 5])  # 训练集输出数据
inp_test <- as.matrix(iris_test[, -5])  # 测试集输入数据
out_test <- as.numeric(iris_test[, 5])  # 测试集输出数据

# 定义网络架构
layers <- c(4, 8, 7, 3)  # 输入层有4个节点，每个隐藏层有8个节点，输出层有3个节点

# 初始化网络
nn <- netup(layers)  # 创建一个具有给定层数和节点数的神经网络

# 训练网络
nn_trained <- train(nn, inp_train, out_train, eta = 0.1, mb = 10, nstep = 1000)
predictions <- vector()  # 创建一个向量存储预测结果
for (i in 1:nrow(inp_test)) {
  nn_test <- forward(nn_trained, inp_test[i,])  # 将测试集输入传递给训练好的网络进行前向传播
  predictions[i] <- apply(nn_test$h[[length(layers)]], 2, which.max)  # 根据输出层的激活值确定预测类别
}

accuracy <- sum(predictions == out_test) / length(out_test)  # 计算准确率
print(paste("error rate on test data:", 1-accuracy))  # 打印测试集的准确率

