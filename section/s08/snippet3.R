#Here we use a for loop to count up the number of speech
#links. This will be handy. We use a dummy counter variable
#, count

count = 0

for(i in 1:length(speech_links))
{
        if(!is.na(speech_links[i]))
        {
                count = count + 1
                if(debug==1)
                {
                        if(count==1)
                        {
                                cep()
                                write('The speech links are                                     ')
                        }
                        write(speech_links[i])
                }
        }
}

