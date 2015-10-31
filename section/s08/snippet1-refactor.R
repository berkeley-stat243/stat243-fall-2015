library (stringr)

extract_speech <- function(html) {
    # returns plain text string by removing html markup, applause, and laughter
    speech <- gsub("<p>", "\n", html)
    speech <- str_replace_all(speech, "<.*?>", "")
    speech <- gsub("\\[Applause\\]", "", speech)
    speech <- gsub("\\[Laughter\\]", "", speech)
    return(speech)
}

count_occurence <- function(text, pattern) {
    return(str_count(text, pattern))
}

speeches <- lapply(speechTxt, extract_speech)
applause <- lapply(speechTxt, count_occurence, "\\[Applause\\]"))
laughter <- lapply(speechTxt, count_occurence, "\\[Laughter\\]"))
