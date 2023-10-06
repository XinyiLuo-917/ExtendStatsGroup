#TeamMember: Baosong Shi(S2530846); Xinyi Luo(S2518917); Ran Huo(S2559670)
#TeamWork Contribution: Baosong Shi(S2530846):Construct the main code framework(40%);
#Xinyi Luo(S2518917)& Ran Huo(S2559670):Debugging, improving details, and writing the probability section with loop function(30%vs30%);


#Step 3
#setwd("C:/Users/xinyi Luo/Documents/GitHub/ExtendStatsGroup") ## comment out of submitted
a <- scan("C:/Users/xinyi Luo/Desktop/Extend program/4300-0.txt",what="character",skip=73,nlines=32858-73)
a <- gsub("_(","",a,fixed=TRUE) ## remove "_("
a <- gsub("—","",a,fixed=TRUE) ## remove "—"
a <- gsub("_","",a,fixed=TRUE) ## remove "_"
a <- gsub(")","",a,fixed=TRUE) ## remove ")"
a <- gsub("(","",a,fixed=TRUE) ## remove "("
a <- gsub("-","",a,fixed=TRUE) ## remove "-"
a <- gsub("/","",a,fixed=TRUE) ## remove "/"
a <- gsub("*","",a,fixed=TRUE) ## remove "*"


#Step 4
split_punct <- function(words) {
  punct_indices <- grep("[[:punct:]]", words, perl = TRUE)  # Find an index of words that contain punctuation
  
  updated_words <- lapply(seq_along(words), function(i) {
    if (i %in% punct_indices) {  # If the word contains punctuation
      word <- words[i]
      punctuations <- regmatches(word, gregexpr("[[:punct:]]", word, perl = TRUE))[[1]]  # Extract punctuation characters
      split_word <- unlist(strsplit(word, punctuations, fixed = TRUE))  # Split words using punctuation characters
      
      c(split_word, punctuations)  # Merge the split word and punctuation characters
    } else {
      words[i]  # If the word does not contain punctuation, it remains the same
    }
  })
  
  return(unlist(updated_words))  # Returns the updated word as a single vector
}

#Step 5
words <- a
updated_words <- split_punct(words)

print(updated_words)

#Step 6
#(a)
# Convert the text to lower case and find unique words
unique_words <- unique(tolower(updated_words))

print(unique_words)

#(b)
# Use the match function to find the corresponding index
indices <- match(tolower(updated_words), unique_words)

print(indices)

#(c)
# The tabulate function is used to calculate the frequency
word_counts <- tabulate(indices)

print(word_counts)

#(d)
# Sort word frequencies and get the sorted index
sorted_indices <- order(word_counts, decreasing = TRUE)

# Extract the top 1000 words with the highest frequency
m <- 1000
common_words <- unique_words[sorted_indices[1:m]]
print(common_words)

# Find the position of the 1000th word in the sorted index
threshold_index <- sorted_indices[m]

# Get the number of occurrences of the 1000th word
threshold_word_count <- word_counts[threshold_index]

# Set an threshold_number to the number of occurrences of the 1000th word
threshold_number <- threshold_word_count

# Output the value of threshold_number
print(threshold_number)

#(e)
# Store it in a new variable b
b <- common_words
print(b)

#Step 7
# (a)
# Use match to create a vector indicating the position of each word in the common word vector b
match_vector <- match(tolower(updated_words), b)

# (b)
n <- length(match_vector)
# Create a three-column matrix that stores indexes for common word triples
triplets_matrix <- cbind(match_vector[1:(n-2)], match_vector[2:(n-1)], match_vector[3:n])
T<-triplets_matrix
print(T)

# (c)
# Identify common word triples and delete triples containing NA
common_triplets <- triplets_matrix[rowSums(is.na(triplets_matrix)) == 0, ]

# (d)
# Use the same idea to generate a two-column matrix P of common word pairs
pairs_matrix <- cbind(match_vector[1:(n-1)], match_vector[2:n])
common_pairs <- pairs_matrix[rowSums(is.na(pairs_matrix)) == 0, ]
P<-common_pairs
# Print result
print(common_triplets)
print(common_pairs)

#Step 8
# Create a blank vector to store the generated words
generated_text <- character(0)

# Initial two-word index
k <- c(1, 2)  # Select any initial word index

# Defines the length of the generated paragraph
paragraph_length <- 50
print(common_triplets)
Sub_matrix <- common_triplets[common_triplets[, 1] == 1 & common_triplets[, 2] == 2, ]
print(Sub_matrix)

# Fill in probabilities by using frequency
# Counts the number of words shown in triplets line3
matrix_counts <- table((Sub_matrix[,3]))
totalMatrix_counts <- sum(matrix_counts)
freq_matrix <- matrix_counts/totalMatrix_counts

result_matrix <- matrix(freq_matrix[as.character(Sub_matrix[,3])], nrow = nrow(Sub_matrix))

# Change the type of freq_matrix for the next step
DF_result_matrix <- data.frame(result_matrix)
frequency_DF <- DF_result_matrix$Freq

# Sampling based on probability
generated_text <- c()

for (i in 1:paragraph_length) {
  index <- sample(c(1,2,3),size = 1)
  next_word_index <- sample(common_triplets[, index], size = 1, prob = frequency_DF)
  # Adds the generated word to the generated text
  generated_text <- c(generated_text, na.omit(b)[next_word_index])
}
# Print the generated text
cat(generated_text, sep = " ")


#Step 9
# Initializes a vector for storing probabilistically drawn words
common_words_from_b <- character(0)

# Fill in probabilities by using frequency
# Counts the number of words
total_counts <- sum(word_counts)
freq <- word_counts/total_counts

# calculate probability of words in b
indices_b <- match(tolower(updated_words),b)
word_counts_b <- tabulate(indices_b)
total_counts_b <- sum(word_counts_b)
freq_b <- word_counts_b/total_counts_b

word_probabilities <- setNames(freq_b, word_counts_b)

# Generate a 50-word paragraph
for (i in 1:50) {
  # The next word is drawn from b according to its probability
  next_word_index <- sample(length(b), size = 1, prob = word_probabilities)
  
  # Get the next word (from b)
  next_word <- b[next_word_index]
  
  # Add words to the generated text
  common_words_from_b <- c(common_words_from_b, next_word)
}

# 50 words drawn from b according to probability
cat(common_words_from_b, sep = " ")


#Step 10
# Find unique words
unique_words_10 <- unique(updated_words)

# Use the match function to find the corresponding index
indices_10 <- match(updated_words, unique_words_10)

# The tabulate function is used to calculate the frequency
word_counts_10 <- tabulate(indices_10)
print(word_counts_10)

# Sort word frequencies and get the sorted index
sorted_indices_10 <- order(word_counts_10, decreasing = TRUE)

# Extract the top 1000 words with the highest frequency
m <- 1000
common_words_10 <- unique_words_10[sorted_indices_10[1:m]]
b_modified <- common_words_10
print(b_modified)


# Generate a 50-word paragraph
for (i in 1:50) {
  # The next word is randomly selected based on its probability of occurrence
  next_word_index <- sample(length(b_modified), size = 1, prob = frequency_DF)
  
  #Get the next word
  next_word <- b_modified[next_word_index]
  
  # Print the next word
  cat(next_word, " ")
}

