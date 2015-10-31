library (stringr)

extract_speech <- function(html) {
    # returns plain text string by removing html markup, applause, and laughter
    speech <- str_replace_all(html, "<p>", "\n")
    speech <- str_replace_all(speech, "<.*?>", "")
    speech <- str_replace_all(speech, "\\[.*?\\]", "")
    return(speech)
}

speeches <- lapply(speechTxt, extract_speech)
applause <- lapply(speechTxt, str_count, "\\[Applause\\]"))
laughter <- lapply(speechTxt, str_count, "\\[Laughter\\]"))
