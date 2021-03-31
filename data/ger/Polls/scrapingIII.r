##################
### Datenerhebung
##################

### Working Directory
setwd("C:/Users/Admin/Desktop")

### Pakete
library(RCurl)
library(XML)
library(stringr)

### Datenerhebung
url <- getURL("http://www.wahlrecht.de/umfragen/index.htm")
siteHtml <- htmlTreeParse(url,useInternalNodes=TRUE)
reg <- xpathSApply(siteHtml,"//th/a/@href")

list.institutes <- list()
for(i in 1:length(reg)){
	inst.url <- getURL(paste("http://www.wahlrecht.de/umfragen/",reg[i],sep=""))
	institute <- str_replace(reg[i],".htm","")
	
	if(i != 7){
	siteHtml2 <- htmlTreeParse(inst.url,useInternalNodes=TRUE)
	reg.year <- xpathSApply(siteHtml2,"//a/@href")
	reg.year <- reg.year[str_detect(reg.year,institute[1])]
	reg.year <- reg.year[str_detect(reg.year,"[0-9]+")]
	year.links <- paste("http://www.wahlrecht.de/umfragen/", reg.year,sep="")
	year.links[length(year.links)+1] <- paste("http://www.wahlrecht.de/umfragen/",reg[i],sep="")
	}
	
	if(i == 7){
	year.links <- paste("http://www.wahlrecht.de/umfragen/",reg[i],sep="")
	}
	
	list.year <- list()
	for(j in 1:length(year.links)){
		url.year <- getURL(year.links[j])
		siteHtml3 <- htmlTreeParse(url.year,useInternalNodes=TRUE)
		names1 <- xpathSApply(siteHtml3,"//thead/tr/th[@class='part']",xmlValue)
		names2 <- xpathSApply(siteHtml3,"//thead/tr/th/a",xmlValue)
		names1 <- as.character(c(names1,names2[length(names2)]))
		
		tab <- readHTMLTable(url.year)
		tab <- data.frame(tab[2])
		party.votes <- tab[3:(2+length(names1))]
		colnames(party.votes) <- names1
		colnames(party.votes) <- str_replace(colnames(party.votes),"Ãœ","Ü")
	
		party <- list()
		forecast <- list()
		for(k in 1:ncol(party.votes)){
			party[[k]] <- rep(colnames(party.votes)[k],nrow(party.votes))
			forecast[[k]] <- party.votes[[k]]
		}
		party <- unlist(party)
		forecast <- unlist(forecast)
		poll.date <- rep(tab[[1]], ncol(party.votes))
		if(any(str_detect(colnames(tab),"[Bb]efrag") == TRUE)){
			sample.size <- rep(tab[[which(str_detect(colnames(tab),"[Bb]efrag"))]],ncol(party.votes))}
		else{
			sample.size <- rep(NA,ncol(party.votes))}
		institute <- str_replace(rep(reg[i], length(poll.date)),".htm","")
		list.year[[j]] <- data.frame(poll.date,sample.size,institute,party,forecast)
	}
	list.institutes[[i]] <- do.call("rbind", list.year)
}
	
df.final <- do.call("rbind", list.institutes)	
# save(file="df.final.RData",list="df.final")


### Datensatzbearbeitung

load("df.final.RData")

### Stichprobengröße
sample.size <- str_replace(df.final[[2]], "[.]","")
sample.size <- as.numeric(str_extract(sample.size,"[0-9]+"))

### Datum
poll.date <- str_replace_all(df.final[[1]],"[*]","")
test <- str_split(poll.date,"[.]")
year <- sapply(test,function(x)if(length(x)==3){x[[3]]}else{NA})
year2 <- sapply(test,function(x)if(length(x)==1){str_extract(x[[1]],"[0-9]+")}else{NA})
year3 <- sapply(test,function(x)if(length(x)==2){str_extract(x[[2]],"[0-9]+")}else{NA})
year[is.na(year)] <- year2[is.na(year)]
year[is.na(year)] <- year3[is.na(year)]
year <- as.numeric(year)

month <- sapply(test,function(x)if(length(x)==3){x[[2]]}else{NA})
month2 <- sapply(test,function(x)if(length(x)==1){str_extract(x[[1]],"Mai")}else{NA})
month3 <- sapply(test,function(x)if(length(x)==2){x[[1]]}else{NA})
month[is.na(month)] <- month2[is.na(month)]
month[is.na(month)] <- month3[is.na(month)]

month <- str_replace(month, "Jan","1")
month <- str_replace(month, "Feb","2")
month <- str_replace(month, "Mrz","3")
month <- str_replace(month, "Apr","4")
month <- str_replace(month, "Mai","5")
month <- str_replace(month, "Jun","6")
month <- str_replace(month, "Jul","7")
month <- str_replace(month, "Aug","8")
month <- str_replace(month, "Sep","9")
month <- str_replace(month, "Okt","10")
month <- str_replace(month, "Nov","11")
month <- str_replace(month, "Dez","12")
month <- as.numeric(month)

day <- sapply(test,function(x)if(length(x)==3){x[[1]]}else{NA})
day <- as.numeric(day)

### Partei
party <- str_replace(df.final$party,"Ã¼","ü")
party[party == "Grüne"] <- "GRÜNE"
party[party == "Linke.PDS"] <- "LINKE"
party[party == "PDS"] <- "LINKE"
party <- as.factor(party)

### Institut 
institute <- as.factor(df.final$institute)

### Stimmen
forecast <- str_replace_all(df.final$forecast, ",",".")
forecast <- str_trim(str_replace(forecast, "%",""))
l <- str_detect(forecast,"[Ss]onst")
#fc <- forecast[l] evtl. später um AFD zu identfizieren
forecast[l] <- str_extract(forecast[l],"Sonst.+")
forecast[l] <- str_extract(forecast[l],"[0-9]+.+")
forecast[l] <- str_trim(str_replace(forecast[l],"%",""))
forecast <- as.numeric(forecast)

### Wahlergebnisse
l <- df.final[2] == "Bundestagswahl"

### 1998
bw1998 <- data.frame(x=1:13)
bw1998$party <- party[l & !is.na(df.final[[2]]) & year == 1998]
bw1998$result <- forecast[l & !is.na(df.final[[2]]) & year == 1998]
bw1998$institute <- institute[l & !is.na(df.final[[2]]) & year == 1998]
bw1998_2 <- data.frame(x=1:21)
bw1998_2$party <- party[df.final[[1]]=="Wahl 1998"]
bw1998_2$result <- forecast[df.final[[1]]=="Wahl 1998"]
bw1998_2$institute <- institute[df.final[[1]]=="Wahl 1998"]
bw1998 <- list(bw1998,bw1998_2)
bw1998 <- do.call("rbind", bw1998)	

### 2002
bw2002 <- data.frame(x=1:49)
bw2002$party <- party[l & !is.na(df.final[[2]]) & year == 2002]
bw2002$result <- forecast[l & !is.na(df.final[[2]]) & year == 2002]
bw2002$institute <- institute[l & !is.na(df.final[[2]]) & year == 2002]

### 2005
bw2005 <- data.frame(x=1:55)
bw2005$party <- party[l & !is.na(df.final[[2]]) & year == 2005]
bw2005$result <- forecast[l & !is.na(df.final[[2]]) & year == 2005]
bw2005$institute <- institute[l & !is.na(df.final[[2]]) & year == 2005]

### 2009
bw2009 <- data.frame(x=1:63)
bw2009$party <- party[l & !is.na(df.final[[2]]) & year == 2009]
bw2009$result <- forecast[l & !is.na(df.final[[2]]) & year == 2009]
bw2009$institute <- institute[l & !is.na(df.final[[2]]) & year == 2009]

### Genaue Datumsangaben
d <- rep(NA,length(forecast))
date <- as.Date(rep(NA,length(forecast)))
d[!is.na(day)] <- paste(day[!is.na(day)],month[!is.na(day)],year[!is.na(day)],sep="/")
date[!is.na(day)] <- as.Date(d[!is.na(day)],"%d/%m/%Y")

### Zuordnen der Wahlergebnisse für eindeutige Datumsangaben
election <- rep(NA,length(forecast))
actual.votes <- rep(NA,length(forecast))
bws <- list(bw1998,bw2002,bw2005,bw2009)
bw.date <- as.Date(c("27/09/1998","22/09/2002", "18/09/2005", "27/09/2009"),"%d/%m/%Y")
for(z in 1:4){
	for(i in 1:length(levels(institute))){
		for(j in 1:length(levels(party))){
			if(z == 1){
			l <- date < bw.date[z] & institute == levels(institute)[i] & party == levels(party)[j]
			}
			else{
			l <- (date < bw.date[z] & date > bw.date[z-1]) & institute == levels(institute)[i] & party == levels(party)[j]
			lfut <- date > bw.date[z]
			lfut[is.na(lfut)] <- FALSE
			}
			l[is.na(l)] <- FALSE
			l2 <- bws[[z]]$institute == levels(institute)[i] & bws[[z]]$party == levels(party)[j]
			if(length(bws[[z]]$result[l2]) > 0){
				actual.votes[l] <- rep(bws[[z]]$result[l2][1], length(l[l]))
				election[l] <- rep(as.numeric(format(bw.date[z], "%Y")),length(l[l])) 
				if(z == 4){
				actual.votes[lfut] <- rep("AUSSTEHEND", length(lfut[lfut]))
				election[lfut] <- rep(2013,length(lfut[lfut])) 
				}
			}
			else{"WARNUNG"}
		}
	}
}	

### Zuordnen der Wahlergebnisse für nicht-eindeutige Datumsangaben
library(zoo)

### Monat - Jahr - Datumsangaben
d2 <- rep(NA,length(forecast))
date2 <- as.Date(rep(NA,length(forecast)))
d2[is.na(day) & !is.na(month)] <- paste(month[is.na(day) & !is.na(month)],year[is.na(day) & !is.na(month)],sep="/")
date2 <- as.yearmon(d2,"%m/%Y")

bw.date <- as.yearmon(c("09/1998","09/2002", "09/2005", "09/2009"),"%m/%Y")
for(z in 1:4){
	for(i in 1:length(levels(institute))){
		for(j in 1:length(levels(party))){
			if(z == 1){
			l <- date2 <= bw.date[z] & institute == levels(institute)[i] & party == levels(party)[j]
			}
			else{
			l <- (date2 <= bw.date[z] & date2 > bw.date[z-1]) & institute == levels(institute)[i] & party == levels(party)[j]
			lfut <- date2 >= bw.date[z]
			lfut[is.na(lfut)] <- FALSE
			}
			l[is.na(l)] <- FALSE
			l2 <- bws[[z]]$institute == levels(institute)[i] & bws[[z]]$party == levels(party)[j]
			#if(length(bws[[z]]$result[l2]) == 1 | bws[[z]]$result[l2][1] == bws[[z]]$result[l2][2] & length(bws[[z]]$result[l2]) > 0){
			if(length(bws[[z]]$result[l2]) > 0){
				actual.votes[l] <- rep(bws[[z]]$result[l2][1], length(l[l]))
				election[l] <- rep(as.numeric(format(bw.date[z], "%Y")),length(l[l])) 
				if(z == 4){
				actual.votes[lfut] <- rep("AUSSTEHEND", length(lfut[lfut]))
				election[lfut] <- rep(2013,length(lfut[lfut])) 
				}
			}
			else{"WARNUNG"}
		}
	}
}

actual.votes[actual.votes == "AUSSTEHEND"] <- 999
actual.votes <- as.numeric(actual.votes)

df.forecast <- data.frame(actual.votes, election, party, forecast, sample.size, institute, day, year, month,date)
bw.date <- as.Date(c("27/09/1998","22/09/2002", "18/09/2005", "27/09/2009"),"%d/%m/%Y")
l <- list()
for(i in 1:length(bw.date)){
		l[[i]] <- which(df.forecast$date == bw.date[i])
}
l <- unlist(l)
df.forecast <- df.forecast[-l,]

### AFD 
forecast <- str_replace_all(df.final$forecast, ",",".")
forecast <- str_trim(str_replace(forecast, "%",""))
l <- str_detect(forecast,"[Ss]onst")
fc.afd <- forecast[l] 
fc.afd <- as.numeric(str_extract(str_extract(fc.afd,"AfD [0-9.]+"),"[0-9.]+"))
sample.size.afd <- sample.size[l]
institute.afd <- institute[l]
party.afd <- rep("AFD",length(fc.afd))
actual.votes.afd <- rep(999, length(fc.afd))
election.afd <- rep(2013,length(fc.afd))
day.afd <- day[l]
month.afd <- month[l]
year.afd <- year[l]
date.afd <- as.Date(paste(day.afd,month.afd,year.afd,sep="/"),"%d/%m/%Y")
df.afd <- data.frame(actual.votes.afd, election.afd, party.afd, fc.afd, sample.size.afd, institute.afd, day.afd,year.afd,month.afd,date.afd)
df.afd <- df.afd[!is.na(df.afd$fc.afd),]

### AFD an restlichen Datensatz anhängen
df.forecast$party <- as.character(df.forecast$party)
colnames(df.afd) <- colnames(df.forecast)

data.forecast <- list(df.forecast,df.afd)
data.forecast <- do.call("rbind", data.forecast)	
data.forecast$party <- as.factor(data.forecast$party)

# save(file="data.forecast.RData",list="data.forecast")
