fileName <- readline(prompt = "Please enter the name of the trans .txt file")
year <- readline(prompt = "Please enter the year")

trans <- read.csv(fileName, header = FALSE)
trans <- trans[, c(1, 4, 6, 7, 8, 9, 10, 11, 14, 15, 16)]
names(trans) <- c("sendID", "elType", "transCode", "recType", "recName", "recCity", "recState", "recZIP", "date", "amount", "recID")

trans <- subset(trans, transCode == "24K" | transCode == "24Z" | transCode == "24R" | transCode == "24E" | transCode == "24C" | transCode == "24H" | transCode == "24F")
trans <- subset(trans, recID != "")
trans <- subset(trans, amount > 0)
trans <- subset(trans, elType == "G")

sen <- read.csv(paste('cn', year, '.txt', sep = "", collapse = NULL), header = FALSE)
names(sen) = c("canID", "canName", "party", "year", "state", "canType", "district", "unknown1", "unknown2", "pccID", "street", "street2", "city", "stateAdd", "zip")
sen <- subset(sen, canType == "S")

senTrans <- subset(trans, recID %in% sen$canID | recID %in% sen$pccID)

#removing hilary clinton prez -> senate
if (year == '2008') {
    senTrans <- subset(senTrans, sendID != 'COO431569' | recID != 'C00368895')
}


actualState <- data.frame("stateAct", stringsAsFactors = FALSE)

for (i in 1:nrow(senTrans)) {
    if (senTrans[i, ]$recID %in% sen$pccID) {
        actualState[i,] <- c(as.character(sen[which(as.character(sen$pccID) == as.character(senTrans[i,]$recID)),]$state))
    }
    else
        actualState[i,] <- c(as.character(sen[which(as.character(sen$canID) == as.character(senTrans[i,]$recID)),]$state))
    }

senTrans$stateAct = actualState
temp <- data.frame(senTrans$sendID, senTrans$amount, senTrans$stateAct)
names(temp) = c("sendID", "amount", "stateAct")

elGeoGen <- aggregate(amount ~ sendID + stateAct, temp, sum)

write.table(elGeoGen, file = paste('elGeoGen', year, '.txt', sep = "", collapse = NULL), row.names = FALSE, col.names = TRUE, quote = FALSE)

states = c("AK", "AL", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI",
"ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS",
"MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR",
"PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

save(states, file = 'states.rda')
save(sen, file = paste('sen', year, '.rda', sep = "", collapse = NULL))
save(actualState, file = paste('actualState', year, '.rda', sep = "", collapse = NULL))
save(elGeoGen, file = paste('elGeoGen', year, '.rda', sep = "", collapse = NULL))
save(senTrans, file = paste('senTrans', year, '.rda', sep = "", collapse = NULL))
save(trans, file = paste('trans', year, '.rda', sep = "", collapse = NULL))
