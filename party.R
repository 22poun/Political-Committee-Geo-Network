year = readline(prompt = 'please enter a year')
load(file = paste('sen', year, '.rda', sep = "", collapse = NULL))
load(file = paste('senTrans', year, '.rda', sep = "", collapse = NULL))

recParty <- data.frame("recParty", stringsAsFactors = FALSE)

for (i in 1:nrow(senTrans)) {
    if (senTrans[i, ]$recID %in% sen$pccID) {
        recParty[i,] <- c(as.character(sen[which(as.character(sen$pccID) == as.character(senTrans[i,]$recID)),]$party))
        
    }
    else
    recParty[i,] <- c(as.character(sen[which(as.character(sen$canID) == as.character(senTrans[i,]$recID)),]$party))
    
}

senTrans$recParty = recParty
temp <- data.frame(senTrans$sendID, senTrans$amount, recParty)
names(temp) <- c("sendID", "amount", "recParty")
temp <- aggregate(amount ~ sendID + recParty, temp, sum)

com <- as.data.frame(unique(temp$sendID))
com$recParty = NA
names(com) = c("com", "recParty")

for (i in 1:nrow(com)){ 
    t <- subset(temp, sendID == com[i, 1])
    com[i,2] = t[which.max(t$amount),]$recParty
}

save(senTrans, file = paste('senTrans', year, '.rda', sep = "", collapse = NULL))
save(com, file = paste('party', year, '.rda', sep = "", collapse = NULL))
write.table(com, file = paste('party', year, '.txt', sep = "", collapse = NULL), row.names = FALSE, col.names = TRUE, quote = FALSE)