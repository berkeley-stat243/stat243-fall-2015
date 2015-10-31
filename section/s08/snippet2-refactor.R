library(stringr)

speeches <- list("We begin as a republic, America.", "God bless us!")

patterns <- c("I", "we|We", "America", "democra", "republic", "Democrat",
             "Republican", "free", "war", "God[^ bless]",
             "God bless|God bless|god bless",
             "Jesus|Christ|Christian", "job/Job/jobs")


# will this?
counts <- t(sapply(speeches, function(speech) {
                                 sapply(patterns, str_count, speech)}))
colnames(counts) <- patterns

# will this?
counts <- t(sapply(speeches, function(speech) {
                                 sapply(patterns, str_count, string=speech)}))
colnames(counts) <- patterns

# will this work?
counts <- t(sapply(speeches, str_count, patterns))
colnames(counts) <- patterns

