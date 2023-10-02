setwd("D:/Github/ExtendStatsGroup") ## comment out of submitted
a <- scan("C:/Users/HP/Desktop/gutenberg.org_files_4300_4300-0.txt",what="character",skip=73,nlines=32858-73)
a <- gsub("_(","",a,fixed=TRUE) ## remove "_("

#Step 4
split_punct <- function(words, punctuation) {
  new_words <- character()
  #Define the regular expression for punctuation
  punctuation_regex <- "[[:punct:]]+"
  #Look for punctuation in each word
  for (word in words) {
    index <- gregexpr(punctuation_regex, word)[[1]]
    #In case of punctuation
    if (length(index) > 0) {
      #Split the word and add punctuation as a new entry to the new vector
      split_word <- unlist(strsplit(word, punctuation_regex))
      punctuations <- regmatches(word, gregexpr(punctuation_regex, word))[[1]]
      new_words <- c(new_words, split_word, punctuations)
    } else {
      new_words <- c(new_words, word) #If there is no punctuation, the word is added to the new vector
    }
  }
  return(new_words)
}

#Step 5
words <- a
updated_words <- split_punct(words)
print(updated_words)

#Step 6
# Convert the text to lower case and find unique words
unique_words <- unique(tolower(updated_words))

print(unique_words)

# Use the match function to find the corresponding index
indices <- match(updated_words, unique_words)

print(indices)

# The tabulate function is used to calculate the frequency
word_counts <- tabulate(indices)

print(word_counts)

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

# Extract m most common words
m <- 1000
b <- head(common_words, m)

print(b)

#Step 7
