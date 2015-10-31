library(stringr)
clean_string = "This, is the ! main way? \n Test."

# could use lapply/sapply over all of them
words = str_extract_all(clean_string, '\\w+')
word_counts = length(words)
