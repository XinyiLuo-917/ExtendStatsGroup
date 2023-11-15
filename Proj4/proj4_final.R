#1
set.seed(2030)#设定种子数
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
    network$b[[l]] <- matrix(runif(d[l+1]*1, 0, 0.2), nrow = d[l+1], ncol = 1)
  }
  #将一个大小为d[4]×1的全零矩阵赋值给network$h最后一个元素。这个矩阵用于存储输出层的节点值。
  network$h[[length(d)]] <- matrix(0, nrow = d[4], ncol = 1)
  return(network)
}

#2: Define the forward function
forward <- function(nn, inp) {
  nn$h[[1]] <- as.matrix(inp)  # 将输入数据转换为矩阵，并赋值给网络的第一个隐藏层节点值
  
  for (l in 1:(length(nn$h)-1)) {
    nn$h[[l+1]] <- pmax(0, nn$W[[l]] %*% nn$h[[l]] + nn$b[[l]])  # 计算下一层节点值
    nn$h[[l+1]] <- as.matrix(nn$h[[l+1]])  # 将下一层节点值转换为矩阵
  }
  
  return(nn)  # 返回更新后的神经网络
}

#3: Define the backward function
# Backward propagation
backward <- function(nn, k) {
  n_layers <- length(nn$h)  # 获取神经网络的层数
  dh <- vector("list", n_layers)  # 初始化存储梯度的激活值列表
  dW <- vector("list", n_layers - 1)  # 初始化存储权重梯度的列表
  db <- vector("list", n_layers - 1)  # 初始化存储偏置梯度的列表
  
  # 计算输出层的 softmax 激活值
  softmax <- exp(nn$h[[n_layers]]) / sum(exp(nn$h[[n_layers]]))
  # 初始化 delta 用于计算误差
  delta <- softmax
  delta[k] <- delta[k] - 1 
  
  # 遍历网络的每一层（从后往前）
  for (l in seq(n_layers - 1, 1, by = -1)) {
    dh[[l + 1]] <- delta  # 存储当前层的梯度
    dW[[l]] <- delta %*% t(nn$h[[l]])  # 计算权重梯度
    db[[l]] <- delta  # 计算偏置梯度
    
    # 如果不是第一层，计算前一层的 delta
    if (l > 1) {
      delta <- (t(nn$W[[l]]) %*% delta) * (nn$h[[l]] > 0)  # 应用 ReLU 导数
    }
  }
  
  # 将计算出的梯度附加到网络结构上
  nn$dh <- dh
  nn$dW <- dW
  nn$db <- db
  
  # 返回包含梯度信息的网络结构
  return(nn)
}

# 4: Define the train function
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
    }
    # 更新权重
    nn$W <- mapply(function(w, dw) w - eta * dw / mb, nn$W, nn$dW, SIMPLIFY = FALSE)
    
    # 更新偏置
    nn$b <- mapply(function(b, db) b - eta * db / mb, nn$b, nn$db, SIMPLIFY = FALSE)
  }
  
  return(nn)  # 返回更新后的神经网络对象
}

#5: Train a network for iris classification
# 导入鸢尾花数据集
data(iris)
iris$Species <- as.numeric(factor(iris$Species))  # 将鸢尾花的种类转换为数值类型

test_idx <- seq(5, nrow(iris), by = 5)  # 创建测试集索引
train_idx <- setdiff(1:nrow(iris), test_idx)  # 创建训练集索引，排除测试集中的数据

train_data <- iris[train_idx, -5]  # 提取训练集的特征数据
train_labels <- iris[train_idx, 5]  # 提取训练集的标签
test_data <- iris[test_idx, -5]  # 提取测试集的特征数据
test_labels <- iris[test_idx, 5]  # 提取测试集的标签

num_classes <- length(unique(iris$Species))  # 计算数据集中类别的数量

nn <- netup(c(4, 8, 7, 3))  # 初始化神经网络，层数和每层节点数为 4, 8, 7, 3
nn <- train(nn, as.matrix(train_data), train_labels, eta = 0.01, mb = 10, nstep = 10000)  # 使用训练数据训练神经网络

# 函数对输入数据进行分类
classify <- function(nn, x) {
  nn <- forward(nn, x)
  # 返回输出层中最大值的索引
  return(which.max(nn$h[[length(nn$h)]]))
}

# 函数评估误分类率
evaluate <- function(nn, test_data, test_labels) {
  predictions <- apply(test_data, 1, function(x) classify(nn, x))
  misclass_rate <- mean(predictions != test_labels)
  return(misclass_rate)
}

# 对测试数据进行分类并评估误分类率
misclass_rate <- evaluate(nn, as.matrix(test_data), test_labels)

# 打印错误分类率
print(misclass_rate)
