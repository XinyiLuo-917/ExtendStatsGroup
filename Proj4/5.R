netup <- function(d) {
  network <- list() # 创建一个空的列表 network，用于存储神经网络的参数。
  
  # 初始化 h、W 和 b
  network$h <- vector("list", length(d))
  network$W <- vector("list", length(d) - 1)
  network$b <- vector("list", length(d) - 1)
  
  # 初始化节点
  for (l in 1:(length(d) - 1)) {
    # 将一个大小为 d[l] 的全零向量赋值给 network$h 的第 l 个元素。
    network$h[[l]] <- rep(0, d[l])
    
    # 生成一个 d[l + 1] × d[l] 大小的随机矩阵，赋值给 network$W 的第 l 个元素。
    network$W[[l]] <- matrix(runif(d[l + 1] * d[l], 0, 0.2), nrow = d[l + 1], ncol = d[l])
    
    # 生成一个 d[l + 1] 大小的随机向量，赋值给 network$b 的第 l 个元素。
    # 这是连接层 l 到层 l+1 的偏置向量。
    network$b[[l]] <- runif(d[l + 1], 0, 0.2)
  }
  
  # 将一个大小为 d[length(d)] 的全零向量赋值给 network$h 的最后一个元素。
  network$h[[length(d)]] <- rep(0, d[length(d)])
  
  return(network)
}

# Initialize the neural network structure
forward <- function(nn, inp) {
  # 将输入数据设置为网络的第一层（输入层）的激活值
  nn$h[[1]] <- inp  # 直接将向量 inp 赋值给第一层的激活值
  
  # 遍历神经网络的每一层（除了输入层）
  for (l in 1:(length(nn$h) - 1)) {
    # 计算下一层的激活值
    # 使用 nn$W[[l]] %*% nn$h[[l]] 计算当前层激活值和权重的矩阵乘法
    # 加上偏置 nn$b[[l]]，然后应用 ReLU 激活函数（pmax(0, x)）
    nn$h[[l + 1]] <- pmax(0, nn$W[[l]] %*% nn$h[[l]] + nn$b[[l]])
  }
  
  # 返回更新后的神经网络结构
  return(nn)
}

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

# Training function
train <- function(nn, inp, k, eta = 0.01, mb = 10, nstep = 10000) {
  n_samples <- nrow(inp)  # 获取训练样本的数量
  
  # 循环指定的训练步数
  for (step in 1:nstep) {
    indices <- sample(n_samples, mb)  # 随机选择一个小批量的样本索引
    inp_mb <- inp[indices, ]  # 提取小批量的输入数据
    k_mb <- k[indices]        # 提取小批量对应的类别标签
    
    # 处理小批量中的每个样本
    for (i in 1:nrow(inp_mb)) {
      nn <- forward(nn, inp_mb[i, ])  # 对当前样本执行前向传播
      nn <- backward(nn, k_mb[i])     # 对当前样本执行后向传播
      
      # 更新神经网络的每一层的权重和偏置
      for (l in 1:length(nn$W)) {
        nn$W[[l]] <- nn$W[[l]] - eta * nn$dW[[l]] / mb  # 更新权重
        nn$b[[l]] <- nn$b[[l]] - eta * nn$db[[l]] / mb  # 更新偏置
      }
    }
  }
  
  return(nn)  # 返回训练好的神经网络
}

set.seed(2023)  # Ensure reproducibility
# Load the iris dataset
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

# Function to classify input data
classify <- function(nn, x) {
  nn <- forward(nn, x)
  # Return the index of the max value in the output layer
  return(which.max(nn$h[[length(nn$h)]]))
}

# Function to evaluate misclassification rate
evaluate <- function(nn, test_data, test_labels) {
  predictions <- apply(test_data, 1, function(x) classify(nn, x))
  misclass_rate <- mean(predictions != test_labels)
  return(misclass_rate)
}

# Classify test data and evaluate misclassification rate
misclass_rate <- evaluate(nn, as.matrix(test_data), test_labels)

# Print the misclassification rate
print(misclass_rate)