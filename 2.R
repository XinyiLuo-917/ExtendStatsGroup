#Step 3
setwd("D:/Github/ExtendStatsGroup") ## comment out of submitted
a <- scan("C:/Users/HP/Desktop/gutenberg.org_files_4300_4300-0.txt",what="character",skip=73,nlines=32858-73)
a <- gsub("_(","",a,fixed=TRUE) ## remove "_("
a <- gsub("—","",a,fixed=TRUE) ## remove "—"
a <- gsub("_","",a,fixed=TRUE) ## remove "_"
a <- gsub(")","",a,fixed=TRUE) ## remove ")"
a <- gsub("(","",a,fixed=TRUE) ## remove "("
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

#step7
# (a)
# Use match to create a vector indicating the position of each word in the common word vector b
match_vector <- match(tolower(updated_words), b)


# (b)
n <- length(match_vector)
# Create a three-column matrix that stores indexes for common word triples
triplets_matrix <- cbind(match_vector[1:(n-2)], match_vector[2:(n-1)], match_vector[3:n])
# triplets_matrix <- cbind(match_vector[-3], match_vector[-2], match_vector[-3])
T<-triplets_matrix

# (c)
# Identify common word triples and delete triples containing NA
common_triplets <- triplets_matrix[rowSums(is.na(triplets_matrix)) == 0, ]

# (d)
# Use the same idea to generate a two-column matrix P of common word pairs
pairs_matrix <- cbind(match_vector[-2], match_vector[-1])
P<-pairs_matrix 
# Print result
print(common_triplets)
print(pairs_matrix)

#Step 8
# Create a blank vector to store the generated words
generated_text <- character(0)

# Initial two-word index
k <- c(1, 2)  # Select any initial word index

# Defines the length of the generated paragraph
paragraph_length <- 50
common_triplets[common_triplets[, 1] == 1 & common_triplets[, 2] == 2, ]

generated_text <- c()
for (i in 1:paragraph_length) {
  index <- sample(c(1,2,3),size = 1)
  next_word_index <- sample(common_triplets[, index], size = 1)
  # Adds the generated word to the generated text
  generated_text <- c(generated_text, na.omit(b)[next_word_index])
}
# Print the generated text
cat(generated_text, sep = " ")

#Step 9
common_words <- c()
word_probabilities <- runif(50,0,1)  # Fill in the corresponding word probabilities
# The simulation generates a 50-word paragraph of text
for (i in 1:50) {
  # Draw the next word according to the word probability
  next_word_index <- sample(1:50, size = 1, prob = word_probabilities)
  common_words <- c(common_words,generated_text[next_word_index])
  # Print the next word
  cat(common_words[next_word_index], " ")
}
common_words == generated_text

#Step 10
# Store common words with uppercase letters in another vector
capitalized_common_words <- common_words
capitalized_common_words[grep("^[A-Z]", common_words)] <- toupper(capitalized_common_words[grep("^[A-Z]", common_words)])

# The simulation generates a 50-word paragraph of text
for (i in 1:50) {
  # Draw the next word according to the word probability
  next_word_index <- sample(length(common_words), size = 1, prob = word_probabilities)
  
  # Get the next word (capitalized)
  next_word <- capitalized_common_words[next_word_index]
  
  # Print the next word
  cat(next_word, " ")
}