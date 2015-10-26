links <- na.omit(speech_links)
count <- length(links)

if (debug==1) {
    cep()
    write('The speech links are                                     ')
    lapply(links, write)
}
