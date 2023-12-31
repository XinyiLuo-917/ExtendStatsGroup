setwd("D:/Github/ExtendStatsGroup") ## comment out of submitted
a <- scan("C:/Users/HP/Desktop/gutenberg.org_files_4300_4300-0.txt",what="character",skip=73,nlines=32858-73)
a <- gsub("_(","",a,fixed=TRUE) ## remove "_("

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
indices <- match(updated_words, unique_words)

print(indices)

#(c)
# The tabulate function is used to calculate the frequency
word_counts <- tabulate(indices)

print(word_counts)

#(d)
# Count the frequency of each word
word_counts <- tabulate(match(tolower(updated_words), unique_words))

# Set target quantity
target_count <- 1000

# Search threshold
threshold <- 1
while (length(unique_words[word_counts >= threshold]) > target_count) {
  threshold <- threshold + 1
}

# Filter common words by threshold
common_words <- unique_words[word_counts >= threshold]

print(common_words)

#(e)
# Extract m most common words
m <- 1000
b <- head(common_words, m)

print(b)

#Step 7
#(a)
# Create a matching vector
match_vector <- match(tolower(updated_words), common_words)

print(match_vector)

#(b)
# Matrix column binding
matrix <- cbind(match_vector[-length(match_vector)], match_vector[-1], match_vector[-2])

print(matrix)

#(c)
# Identify common word triples and remove triples that contain NA
common_triplets <- matrix[rowSums(is.na(matrix)) == 0, ]

print(common_triplets)

#(d)
# Matrix column binding
matrix_d <- cbind(match_vector[-length(match_vector)], match_vector[-1])

# Identify common word triples and remove triples that contain NA
common_pairs <- matrix[rowSums(is.na(matrix)) == 0, ]

print(common_pairs)

# Step 8
#(a)
# Set the initial word index pair
k <- c(1, 2)

# The simulation generates a 50-word paragraph of text
for (i in 1:50) {
  # Generates an index for the next word based on the current word index pair
  next_word_index <- sample(length(b), size = 1, prob = model$probabilities[k[1], k[2], , drop = TRUE])
  
  # Print the corresponding word
  cat(b[next_word_index], " ")
  
  # Update the word index pair for the next iteration
  k <- c(k[2], next_word_index)
}

#(b)
# Extraction the submatrix
sub_matrix <- T[T[, 1] == k[i] & T[, 2] == k[j], ]

# Print the submatrix
print(sub_matrix)

#(c)
# Select an element at random from the third column of the extracted submatrix
random_element <- sample(sub_matrix[, 3], size = 1)

# Prints randomly selected elements
print(random_element)

#Step 9
