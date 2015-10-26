word_vectorize <- function(string_path,speech_index)
{
#First we will strip the \n s that are still hanging around
        clean_string <- str_replace_all(string_path,
                '\\n','')
#This regex word count simply counts the number of non-word
#characters which should approximately equal the number of words
#as punctuation and spaces are non-word characters. It will
#double count hyphenated words but lets not be picky.
        word_number <- str_count(clean_string,'\\W+')
#Here, to make our life easier, we directly write out of this
#function to the parent environement. While this does defeat
#some of the point of functions, it prevents our simple
#character vector from becoming complicated
        word_counts[speech_index] <<- word_number
#Here we create a vector to hold our words, of approp length
        long_vec <- vector('character',word_number)
#This str_extract here will pull out all the words followed
#by a space or punctuation. The fault here is that the
#punctuation will be included with the word, but we can't
#be perfect
        word_vector <- str_extract_all(clean_string,
                '[[:alpha:]]*? |[[:alpha:]]*?[.\\?!]')
#Here we return our word_vector
        return(word_vector)
}


