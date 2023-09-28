setwd("D:/Github/ExtendStatsGroup") ## comment out of submitted
a <- scan("C:/Users/HP/Desktop/gutenberg.org_files_4300_4300-0.txt",what="character",skip=73,nlines=32858-73)
a <- gsub("_(","",a,fixed=TRUE) ## remove "_("
split_punct <- function(words, punct) {
  # Creates an empty vector to store the updated word
  updated_words <- vector("character", length = 0)
  
  for (word in words) {
    # Replace the function gsub with a regular expression to remove punctuation from the word
    cleaned_word <- gsub(paste0(punct, "$"), "", word)
    
    # The cleaned up words and punctuation marks are added to the updated word vector as new entries
    updated_words <- c(updated_words, cleaned_word, punct)
  }
  
  return(updated_words)
}
