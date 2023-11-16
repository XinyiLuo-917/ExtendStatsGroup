# TeamMember: Baosong Shi(S2530846); Xinyi Luo(S2518917); Ran Huo(S2559670)
# GitHub URL: https://github.com/XinyiLuo-917/ExtendStatsGroup/tree/main/Proj4
# TeamWork Contribution:
# Baosong Shi:Responsible for code-writing and debugging(40%);
# Xinyi Luo:Optimized code structure,debugged and wrote part of comments(30%);
# Ran Huo:Improved detailed aspects of the code and made comments clearer(30%).

# Description:
# This code is mainly for building a classification neural network and 
# training it via stochastic gradient descent. 
# It contains four main functions: netup, forward, backward and train.

# netup: Initialize the whole network.
# forward: Calculate the node values and update the network using given input.
# backward: Calculate each layer's loss gradient related to parameters 
# and update the network for backpropagation.
# train: Return the network that trained by stochastic gradient descent method.
# Application: Classify the iris dataset by using a 4-8-7-3 structure network.


# Set seed.
set.seed(2030)

# netup()
# Require a vector d containing the number of nodes in each layer of a network
# h: a list of nodes for each layer
# w: a list of weight matrices
# b: a list of offset vectors
netup <- function(d) {
  # Initialize network to store the parameters of the neural network.
  network <- list()
  network$h <- vector("list", length(d))
  network$W <- vector("list", length(d)-1)
  network$b <- vector("list", length(d)-1)

  for (l in 1:(length(d) - 1)) {
    # Initialize node values, weight matrices and offset vectors for each layer.
    network$h[[l]] <- rep(0, d[l])
    network$W[[l]] <- matrix(runif(d[l+1]*d[l], 0, 0.2), nrow=d[l+1], ncol=d[l])
    network$b[[l]] <- runif(d[l+1], 0, 0.2)
  }
  
  # Initialize node values for the output layer.
  network$h[[length(d)]] <- rep(0, d[length(d)])
  
  # Return the list representing the network.
  return(network)
}

# forward(nn,inp)
# nn: a network list as returned by function netup(d)
# inp: a vector of input values for the first layer
forward <- function(nn, inp) {
  # Set input values to the first layer of the network.
  nn$h[[1]] <- inp
  
  for (l in 1:(length(nn$h) - 1)) {
    # Calculate the next layer of node values.
    # Add offset nn$b[[l]] and apply the ReLU activation function.
    nn$h[[l + 1]] <- pmax(0, nn$W[[l]] %*% nn$h[[l]] + nn$b[[l]])
  }
  
  # Return the updated network list.
  return(nn)
}

# backward(nn,k)
# nn: the updated network list returned from function forward(nn,inp)
# k: target output class k(determine the partial derivatives of loss function)
backward <- function(nn, k) {
  # Get the number of layers of the neural network.
  n_layers <- length(nn$h)
  # Initialize the derivatives of the nodes, weights, offsets and d.
  dh <- vector("list", n_layers)
  dW <- vector("list", n_layers - 1)
  db <- vector("list", n_layers - 1)
  dd <- vector("list", n_layers)
  
  # Calculates the activation value of the output layer.
  dh[[n_layers]] <- exp(nn$h[[n_layers]]) / sum(exp(nn$h[[n_layers]]))
  dh[[n_layers]][k] <- dh[[n_layers]][k] - 1
  dd[[n_layers]] <- dh[[n_layers]]
  
  for (l in n_layers:2) {
    # Depending on the type of activation function, calculate d.
    dd[[l]] = ifelse(nn$h[[l]] <= 0, 0, dh[[l]])
    # Calculate the output derivative of the previous layer.
    dh[[l-1]] <- t(nn$W[[l-1]]) %*% dd[[l]]
  }

  for (l in n_layers:2) {
    # Calculate the weight and offsets gradient.
    dW[[l-1]] <- dd[[l]] %*% t(nn$h[[l-1]])
    db[[l-1]] <- dd[[l]]
  }
   
  # Attach the computed gradient to the network.
  nn$dh <- dh
  nn$dW <- dW
  nn$db <- db
  
  # Returns a network containing gradient information.
  return(nn)
}

# train(nn,inp,k,eta=0.01, mb=10, nstep=10000)
# nn: the network list; inp: imput data matrix; k: corresponding label vector
# eta: step size，mb: batch size; nstep: the number of optimization steps 
train <- function(nn, inp, k, eta = 0.01, mb = 10, nstep = 10000) {
  # Get the number of training samples.
  n_samples <- nrow(inp)
  
  for (step in 1:nstep) {
    # Randomly select samples of number mb.
    indices <- sample(n_samples, mb)
    inp_mb <- inp[indices, ]
    k_mb <- k[indices]
    
    for (i in 1:length(k_mb)) {
      # Passing samples to network for forward propagation.
      nn <- forward(nn, inp_mb[i, ])
      # Compute the gradient of the loss function.
      nn <- backward(nn, k_mb[i])
      # Update weights and offsets.
      nn$W <- mapply(function(w, dw) w-eta * dw/mb, nn$W, nn$dW, SIMPLIFY=FALSE)
      nn$b <- mapply(function(b, db) b-eta * db/mb, nn$b, nn$db, SIMPLIFY=FALSE)
    }
  }
  
  # Returns the updated network object.
  return(nn)
}

# Train a network for iris classification
# Importing the Iris dataset.
data(iris)
# Converting Iris species to numeric types.
iris$Species <- as.numeric(factor(iris$Species))

# Create test set indexes with every five rows as samples.
# Create training set indexes to exclude data from the test set.
test_idx <- seq(5, nrow(iris), by = 5)
train_idx <- setdiff(1:nrow(iris), test_idx)

# Extract feature data from the training/test set in addition to the species.
train_data <- iris[train_idx, -5]
test_data <- iris[test_idx, -5]

# Extract the label of the training/test set, i.e. species.
train_labels <- iris[train_idx, 5]
test_labels <- iris[test_idx, 5]

# Initialize network with number of nodes per layer 4, 8, 7, 3.
nn <- netup(c(4, 8, 7, 3))
# Train a network with training data.
nn <- train(nn, as.matrix(train_data), train_labels, 
            eta = 0.01, mb = 10, nstep = 10000)

# Function to sort the input data.
classify <- function(nn, x) {
  nn <- forward(nn, x)
  # Return the index of the maximum value in the output layer
  return(which.max(nn$h[[length(nn$h)]]))
}

# Function evaluates the mis-classification rate.
evaluate <- function(nn, test_data, test_labels) {
  predictions <- apply(test_data, 1, function(x) classify(nn, x))
  misclass_rate <- mean(predictions != test_labels)
  return(misclass_rate)
}

# Classify test data and evaluate mis-classification rates.
misclass_rate <- evaluate(nn, as.matrix(test_data), test_labels)

# Print mis-classification rate.
print(misclass_rate)
