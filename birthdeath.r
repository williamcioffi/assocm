# home <- '/Users/cioffi/Desktop/'
# # home <- 'X:/'
# # home <- 'Z:/'
# # home <- '/Volumes/wrc14/'

# source("~/Documents/cioffi.r")
# cd(paste(home, "SOCPROG_COMPARISON/calculateassrateinr", sep = ""))

# ww <- read.table("TESTOFBIRTHDEATH.csv", header = TRUE, sep = ',')


birthdeath <- function(ww) {

years <- ww$year
u_years <- sort(unique(ww$year))
nyears <- length(u_years)

ids <- ww$ID
u_ids <- sort(unique(ww$ID))
nids <- length(unique(ww$ID))

nax <- matrix(0, nids, nyears)
rownames(nax) <- u_ids
colnames(nax) <- u_years

agesex <- substring(u_ids, 5, 6)


# this is the raw nax which will be modified throughout to apply birth death rules as follows:

# AM = alive each year between first year seen and either date of known death or 'presumed 
# dead' date(details below)

# NF = alive each year between first year and either date of known death or 'presumed dead' date 
# (details below) minus years they were LF

# LF = alive only in the years they were lactating and seen

# JF = alive each year between first year and last year seen. If the JF was seen as a LF or NF later 
# then each year between last year seen as JF and first year seen as LF or NF are also marked as 
# alive for JF.

# JM = alive each year between first year and last year seen. If the JM was seen as an AM later on 
# then each year between last year seen as JM and first year seen as AM are also marked as alive 
# for JM.

# A whale becomes presumed dead in its sixth year without sightings, on January 1st (a whale last 
# seen any time during the year 1992, is presumed dead on January 1, 1998 and any date after 
# that).


for(i in 1:nids) {
	samp <- which(ids == u_ids[i])
	cur_year <- unique(years[samp])
	year_index <- match(cur_year, u_years)
	nax[i, year_index] <- 1
}

originalnax <- nax

image(originalnax)

# ids <- c(as.character(ids), add.id)
# years <- c(years, add.year)
starts <- Sys.time()
pb <- txtProgressBar(style = 3)
for(i in 1:nids) {
setTxtProgressBar(pb, i/nids)

	samp <- which(ids == u_ids[i])
	cur_year <- unique(years[samp])
	lfpartner_years <- vector()
	jpartner_years <- vector()
	lfpartner_years2 <- vector()
	nfpartner_years <- vector()
	apartner_years <- vector()
	lfpartner <- vector()
	lfpartner2 <- vector()
	nfpartner <- vector()
	rangeofyears <- vector()
	
	if(agesex[i] == "LF") {  #if you're an LF then you're only an LF when you're Lactating.
		rangeofyears <- cur_year
	} else if(agesex[i] == "NF") { #if you're an NF you're a NF every year from first sighting to last sighting minus those years you were an LF if that ever happened
		rangeofyears <- min(cur_year):max(cur_year)
		
		lfpartner <- which(paste(substring(u_ids[i], 1,4), "LF", sep = "") == u_ids)
		if(length(lfpartner) == 1) {
			lfpartner_years <- unique(years[which(ids == u_ids[lfpartner])])
			rangeofyears <- min(c(cur_year, lfpartner_years)):max(c(cur_year, lfpartner_years))
		}
		
	} else if(agesex[i] == "JF") { #if you're a JF that turns into an LF or an NF then you are a JF until you become an NF or a LF
		lfpartner2 <- which(paste(substring(u_ids[i], 1, 4), "LF", sep = "") == u_ids)
		nfpartner <- which(paste(substring(u_ids[i], 1, 4), "NF", sep = "") == u_ids)
					   
		if(length(lfpartner2) == 1) {
			lfpartner_years2 <- unique(years[which(ids == u_ids[lfpartner2])])
		}
		if(length(nfpartner) == 1) {
			nfpartner_years <- unique(years[which(ids == u_ids[nfpartner])])
		}
		if(length(nfpartner)  == 1 | length(lfpartner) == 1) {
			partnerpartner_years <- c(lfpartner_years2, nfpartner_years)
			rangeofyears <- min(cur_year):(min(partnerpartner_years, na.rm = TRUE) - 1)
		} else if(length(nfpartner) == 0 & length(lfpartner) == 0) {
			rangeofyears <- min(cur_year):max(cur_year)
		}
	} else if(agesex[i] == "JM") { #if you're a JM or a JU that turns into an AM or AU at some point then you are a J until you become an A.
		apartner <- which(paste(substring(u_ids[i], 1, 4), "A", substring(u_ids[i], 6, 6), sep = "") == u_ids)
		if(length(apartner) == 1) {
			apartner_years <- unique(years[which(ids == u_ids[apartner])])
			rangeofyears <- min(cur_year):(min(apartner_years) - 1)
		} else {
			rangeofyears <- min(cur_year):max(cur_year)
		}
	} else if(agesex[i] == "AM") {
		rangeofyears <- min(cur_year):max(cur_year)
	}
	
	nax[i,match(rangeofyears, u_years)] <- 1
	if(length(lfpartner_years) > 0) {
		nax[i, match(lfpartner_years, u_years)] <- 0
	}
}
close(pb)
Sys.time() - starts

nax.backup <- nax
image(nax)


###############################################
##############nax with birth###################
###############################################

birthdeath <- read.table("birthdeath.csv", header = TRUE, sep = ',')
kill <- -1*which(birthdeath$DeathYear > 2009) 
kill <- c(kill, -1*which(birthdeath$BirthYear > 2009))
birthdeath <- birthdeath[kill,]

birth_index <- which(!is.na(birthdeath$BirthYear))
death_index <- which(!is.na(birthdeath$DeathYear))


birth <- birthdeath[birth_index, ]
death <- birthdeath[death_index, ]


short_ids <- substring(u_ids, 1, 4)
nbirths <- length(birth[,1])

pb <- txtProgressBar(style = 3)
for(i in 1:nbirths) {
setTxtProgressBar(pb, i/nbirths)

	sampid <- which(short_ids == birth$EGNo[i])
	sampyear <- which(u_years == birth$BirthYear[i])
	nax[sampid, sampyear] <- nax[sampid, sampyear] + 2
}
close(pb)

#check to see that i'm not crazy
image(nax, col = c("black", "blue", "green", "red"))

#the ones with a two are those that have a birth date but weren't seen as Js that year so they should be seen as Js the following year until they were resighted as something else.
#NOTE: what i actually do is call them Js in the year of their birth and then go back and kill the years they were born. that way I don't have to do any checking about the next year. if they were seen as a J the next year then everything just works itself out and the resultant nax is the same as the initial nax for that individual.
earlyborns <- which(nax == 2, arr.ind = TRUE)

keepers <- vector()
startyear <- vector()
newjuvs <- vector()
newjuvstartyear <- vector()

#this loop finds the early borns and determines a year in which they should be considered a juv (born year + 1).
#NOTE: I DON'T DO ANYTHING WITH ANIMALS THAT WERE SEEN AS A CALF NEVER AS A JUV AND THEN NEXT AS AN ADULT. THESE GET SAVED IN newjuvs AND THE START YEAR THEY WERE SEEN IN newjuvstartyear BUT I NEVER ACTUALLY DO ANYTHING WITH THEM BECAUSE IT DOESN'T MAKE SENSE TO ADD IDS TO THE DATASET OF ANIMALS THAT WERE NEVER OBSERVED. THE CODE STUBS HERE WOULD ALLOW TO ADD THIS BEHAVIOR IF DESIRED.
nearlyborns <- nrow(earlyborns)
pb <- txtProgressBar(style = 3)
for(i in 1:nearlyborns) {
setTxtProgressBar(pb, i/nearlyborns)
	earlyids <- which(substring(u_ids, 1, 4) == substring(u_ids[earlyborns[i,1]], 1, 4))
	earlysamp <- which(substring(u_ids[earlyids], 5, 5) == "J")
	if(length(earlysamp > 0)) {
		keepers <- c(keepers, earlyids[earlysamp])
		tempstartyear <- earlyborns[i,2]
		if(tempstartyear > ncol(nax)) tempstartyear <- ncol(nax)
		startyear <- c(startyear, tempstartyear)
	} else { #if the animal wasn't later seen as anything
		newjuvs <- c(newjuvs, paste(substring(u_ids[earlyids], 1, 4)[1], "J", substring(u_ids[earlyids], 6, 6)[1], sep = ''))
		tempjuvstartyear <- earlyborns[i,2]
		if(tempjuvstartyear > ncol(nax)) tempjuvstartyear <- ncol(nax)
		newjuvstartyear <- c(newjuvstartyear, tempjuvstartyear)
	}
}
close(pb)

#NOTE: THESE ARE FOR THE NEW JUVs BUT ARE NOT ACTUALLY USED JUST CODE STUB FOR IF THIS BEHAVIOR WANTED TO BE ADDED SEE ABOVE.
u_juv <- unique(newjuvs)
match_juv <- match(u_juv, newjuvs)
u_juvyear <- newjuvstartyear[match_juv]

#this is just the stupid way all these vectors are related to each other.
u_keepers <- unique(keepers)
match_keepers <- match(u_keepers, keepers)
u_startyear <- startyear[match_keepers]

#I changed nax directly earlier (probably a bad idea FIX THIS LATER) so just recover it to basic nax with nax.backup
nax <- nax.backup

#these two vectors are the ids and the years of the early births
add.id = as.character(paste(substring(u_ids[u_keepers], 1, 4), "J", substring(u_ids[u_keepers], 6, 6), sep = ""))
add.year = as.numeric(colnames(nax)[u_startyear])


#this part of the code actually includes those earlybirths. I got lazy because there are so many rules to apply and so just reused the code from above but doctored the sighting list to include the years and ids of the earlybirths.

ids <- c(as.character(ids), add.id)
years <- c(years, add.year)

pb <- txtProgressBar(style = 3)

for(i in 1:nids) {
setTxtProgressBar(pb, i/nids)

	samp <- which(ids == u_ids[i])
	cur_year <- unique(years[samp])
	lfpartner_years <- vector()
	jpartner_years <- vector()
	lfpartner_years2 <- vector()
	nfpartner_years <- vector()
	apartner_years <- vector()
	lfpartner <- vector()
	lfpartner2 <- vector()
	nfpartner <- vector()
	rangeofyears <- vector()
	
	if(agesex[i] == "LF") {  #if you're an LF then you're only an LF when you're Lactating.
		rangeofyears <- cur_year
	} else if(agesex[i] == "NF") { #if you're an NF you're a NF every year from first sighting to last sighting minus those years you were an LF if that ever happened
		rangeofyears <- min(cur_year):max(cur_year)
		
		lfpartner <- which(paste(substring(u_ids[i], 1,4), "LF", sep = "") == u_ids)
		if(length(lfpartner) == 1) {
			lfpartner_years <- unique(years[which(ids == u_ids[lfpartner])])
			rangeofyears <- min(c(cur_year, lfpartner_years)):max(c(cur_year, lfpartner_years))
		}
		
	} else if(agesex[i] == "JF") { #if you're a JF that turns into an LF or an NF then you are a JF until you become an NF or a LF
		lfpartner2 <- which(paste(substring(u_ids[i], 1, 4), "LF", sep = "") == u_ids)
		nfpartner <- which(paste(substring(u_ids[i], 1, 4), "NF", sep = "") == u_ids)
					   
		if(length(lfpartner2) == 1) {
			lfpartner_years2 <- unique(years[which(ids == u_ids[lfpartner2])])
		}
		if(length(nfpartner) == 1) {
			nfpartner_years <- unique(years[which(ids == u_ids[nfpartner])])
		}
		if(length(nfpartner)  == 1 | length(lfpartner) == 1) {
			partnerpartner_years <- c(lfpartner_years2, nfpartner_years)
			rangeofyears <- min(cur_year):(min(partnerpartner_years, na.rm = TRUE) - 1)
		} else if(length(nfpartner) == 0 & length(lfpartner) == 0) {
			rangeofyears <- min(cur_year):max(cur_year)
		}
	} else if(agesex[i] == "JM") { #if you're a JM or a JU that turns into an AM or AU at some point then you are a J until you become an A.
		apartner <- which(paste(substring(u_ids[i], 1, 4), "A", substring(u_ids[i], 6, 6), sep = "") == u_ids)
		if(length(apartner) == 1) {
			apartner_years <- unique(years[which(ids == u_ids[apartner])])
			rangeofyears <- min(cur_year):(min(apartner_years) - 1)
		} else {
			rangeofyears <- min(cur_year):max(cur_year)
		}
	} else if(agesex[i] == "AM") {
		rangeofyears <- min(cur_year):max(cur_year)
	}
	
	nax[i,match(rangeofyears, u_years)] <- 1
	if(length(lfpartner_years) > 0) {
		nax[i, match(lfpartner_years, u_years)] <- 0
	}
}
close(pb)


#remove the birth years for the ones that weren't seen later as J that year.
#this is recycled code from above, boring and inefficient I know, but i'm running out of variable names so this seemed safer.

nax.temp <- nax.backup
nbirths <- length(birth[,1])

pb <- txtProgressBar(style = 3)
for(i in 1:nbirths) {
setTxtProgressBar(pb, i/nbirths)
	sampid <- which(short_ids == birth$EGNo[i])
	sampyear <- which(u_years == birth$BirthYear[i])
	nax.temp[sampid, sampyear] <- nax.temp[sampid, sampyear] + 2
}
close(pb)

#check to see that i'm not crazy
image(nax.temp, col = c("black", "blue", "red", "green"))

tokill <- which(nax.temp == 2, arr.ind = TRUE)
nax.old <- nax
for(i in 1:nrow(tokill)) {
	nax[tokill[i,1], tokill[i,2]] <- 0
}

#check to see that i'm not crazy
image(nax.old + nax, col = c("black", "green", "blue"))

#i'm going to call the one that includes animals alive for all years inbetween first and last sighting and animals alive from birth to next sighting as nax.birth
nax.births <- nax


##############################################
##########nax with births and deaths##########
##############################################

#this takes in to account both known deaths and the catalogues six year death rule

#is there a built in way to do this? anyway it is convenient so i don't have to keep writing it.
con <- function(x , y) length(which(y == x)) > 0


#anything with .si is just using the simple ids without the age sex information
years.si <- ww$year
u_years.si <- sort(unique(ww$year))
nyears.si <- length(u_years)

ids.si <- substring(ww$ID, 1, 4)
u_ids.si <- sort(unique(substring(ww$ID, 1, 4)))
nids.si <- length(unique(substring(ww$ID, 1, 4)))

nax.si <- matrix(0, nids.si, nyears.si)
rownames(nax.si) <- u_ids.si
colnames(nax.si) <- u_years.si

#just getting a nax for the simple ids
pb <- txtProgressBar(style = 3)
for(i in 1:nids.si) {
setTxtProgressBar(pb, i/nids.si)
	samp <- which(ids.si == u_ids.si[i])
	cur_year <- unique(years.si[samp])
	
	nax.si[i, match(cur_year, u_years.si)] <- 1
}
close(pb)

latest <- vector()

#for each simple id this determines the last year they were seen... we'll need this later
pb <- txtProgressBar(style = 3)
for(i in 1:nids.si) {
setTxtProgressBar(pb, i/nids.si)
	latest[i] <- max(which(nax.si[i,] > 0))
}
close(pb)

#nax.deaths is going to apply all three rules: (1) first to last sighting is alive between (see above for more detailed rules) (2) alive between birth and second sighting and (3) dead at deathdate or after 6 years (per catalogue convention).

#first i'm going to use it to id death dates
nax.deaths <- nax.births
ndeaths <- length(death[,1])
#this ids all the known death dates
# pb <- txtProgressBar(style = 3)
# for(i in 1:ndeaths) {
# setTxtProgressBar(pb, i/ndeaths)
	# sampid <- which(short_ids == death$EGNo[i])
	# sampyear <- which(u_years == death$DeathYear[i])
	# nax.deaths[sampid, sampyear] <- nax[sampid, sampyear] + 2
# }
# close(pb)

# look at them to make sure i'm not crazy
# image(nax.deaths, col = c("black", "blue", "green", "red"))

#deathborns are the indices where someone has a death date
#2 because that was a year the animal wasn't seen other than being dead. 3s are already correctly coded by rule (1) -- that is they already were alive that year and then were also seen dead and presumably never seen again (because they were actually dead). 0 and 1s should be ignored because there isn't anything with death happening there.
# deathborns <- which(nax.deaths == 2, arr.ind = TRUE)[,1]
# u_deathborns <- unique(substring(deathborns, 1,4))

#reset nax.deaths to the binary matrix with just rules (1) and (2) applied
nax.deaths <- nax.births


#here is the megaloop. applying either the known death years or the 6 year rule of the catalogue.
#note the 6 year rule really means last year seen + 5 years.
pb <- txtProgressBar(style = 3)
for(i in 1:(nids.si)) {
setTxtProgressBar(pb, i/nids.si)
	samp <- which(substring(u_ids, 1,4) == u_ids.si[i])
	sampsex <- substring(u_ids[samp], 5, 6)
	ages <- substring(sampsex, 1, 1)
	sexes <- substring(sampsex, 2,2)
	baseid <- substring(u_ids[samp], 1, 4)[1]
	naxdex <- vector()
	yearstoadd <- vector()
	starting <- vector()
	
	#want to consider the starting year the year after the last sighting. if this is beyond the study period then the starting year should be the last year of the study period.
	if(latest[i] != 29) {
		starting <- latest[i] + 1
		
		#determine the death year either by real death year or by 6 years later.
		if(length(which(death$EGNo == baseid)) > 0) { #have a deathyear? update alive up to death
			deathyear  <- death$DeathYear[which(death$EGNo == u_ids.si[i])]
			deathyear <- which(u_years == deathyear)
			
			if(sum(nax[samp, deathyear]) == 0) deathyear <- deathyear - 1	#if it wasn't seen alive also in the year of its death then kill it
			
			if(deathyear < starting) starting <- deathyear
			yearstoadd <- starting:deathyear
		} else if(starting <= ncol(nax)){
			#kill on sixth year after latest
			deathyear <- latest[i] + 5
			#make sure +5 years isn't beyond the study period
			if(deathyear > ncol(nax)) deathyear <- ncol(nax)
			yearstoadd <- starting:deathyear		
		}
		
		#determine where the alives should be allocated. for instance for females it should be under NF because can't know about LF. really maybe this should be another agesex category "unknown adult female" because the animal was not observed so it could be LF or NF. for males this is easier.
		
		#NOTE UUs ARE IGNORED HERE. ADD CODE TO DEAL WITH UUs IF THEY ARE NEEDED FOR ADJUSTED ASSOCIATION INDEX ANALYSES.
		
		if(con(sexes, "F")) { #if it is a female and it has already been a NF then make it an NF. if it has already been a U and never been a J or an NF make it a UF. if it has been a J and never been U or an N make it a JF.
		#NOTE THIS IGNORES LF, CHECK TO MAKE SURE THAT NO ONE HAS ONLY BEEN SEEN AS AN ADULT AS AN LF, OTHERWISE THIS MIGHT PRODUCE UNDESIRABLE RESULTS.
			if(con(ages, "N")) {
				naxdex <- which(u_ids == paste(baseid, "NF", sep = ""))
			} else if(con(ages, "U") & !con(ages, "J"))	{
				naxdex <- which(u_ids == paste(baseid, "UF", sep = ""))
			} else if(con(ages, "J") & !con(ages, "U")) {
				naxdex <- which(u_ids == paste(baseid, "JF", sep = ""))
			}
		} else if(con(sexes, "M")) { #if an M has been seen as an AM then make it an AM, if it has been seen as a UM but never a JM or AM make it a UM. if it has been seen as a JM but never a UM or AM then make it a JM.
			if(con(ages, "A")) {
				naxdex <- which(u_ids == paste(baseid, "AM", sep = ""))
			}  else if(con(ages, "U") & !con(ages, "J"))	{
				naxdex <- which(u_ids == paste(baseid, "UM", sep = ""))
			} else if(con(ages, "J") & !con(ages, "U")) {
				naxdex <- which(u_ids == paste(baseid,"JM", sep = ""))
			}
		}
		
		#if years to add isn't zero then add 2 to each index that should now be alive.
		if(length(yearstoadd) > 0 & length(naxdex) > 0) {
			nax.deaths[naxdex, yearstoadd] <- 2
		}
	}
}
close(pb)

#check to make sure i'm not crazy
image(nax.deaths, col = c("black", "green", "blue"))

#look at it for just a second
# Sys.sleep(2)

image(nax.deaths, col = c("black", "green", "blue"))

#convert back to a binary matrix
#this doesn't actually matter for the 0s, but if you ever want to use this matrix for anything else it matters.
nax.deaths[which(nax.deaths > 1)] <- 1

nax.juvs <- nax.deaths

js <- which(substring(agesex, 1, 1) == "J")

numberofjyears <- apply(nax.deaths[js,], 1, sum)
longjs <- which(numberofjyears >= 8)
longjs <- js[longjs]
adultyear <- longjs*NA

for(i in 1:length(longjs)) {
	#find the first year it was seen
	#check to see if it has a birthday in that year
	#adjust number of years by birthday. known birthyear + 9 years
	#or if known birthday. first year seen + 8 years.

	#if max is more than adult year then from adultyear to max it should be
	#reallocated to the corresponding adult id (if it exists)
	curid <- longjs[i]
	curstrip <- nax.deaths[curid,]
	curstrip <- which(curstrip == 1)
	firstyearalive <- min(curstrip)

	birthmatch <- which(birth$EGNo == substring(u_ids[curid], 1, 4))

	if(length(birthmatch) > 0) {
		birthyear <- birth$BirthYear[birthmatch] - min(u_years) + 1
		adultyear <- birthyear + 9 
	} else {
		adultyear <- firstyearalive + 8
	}
	
	if(adultyear <= max(curstrip)) {
		deseyearsbecomeadult <- adultyear:max(curstrip)
	
		#find matching adult
		if(agesex[curid] == "JM") {
			adultid <- which(u_ids == paste(substring(u_ids[curid], 1, 4), "AM", sep = ''))
		} else if(agesex[curid] == "JF") {
			adultid <- which(u_ids == paste(substring(u_ids[curid], 1, 4), "NF", sep = ''))
		}
	
		if(length(adultid > 0)) {
			nax.juvs[adultid, deseyearsbecomeadult] <- 2
			nax.juvs[curid, deseyearsbecomeadult] <- 0
		}
	}
}

nax.deaths <- nax.juvs

image(nax.deaths, yaxt = 'n', col = c("black", "blue", "green"))
y.seq <- seq(0,1, length = length(u_years))
abline(h = y.seq, col = "grey", lty = 2)
axis(2, las = 1, at = y.seq, lab = u_years)

nax.deaths[which(nax.deaths > 1)] <- 1


# if it was seen as both an LF and NF before it was just coded as an LF but this goes back and adds the NF if it was seen.

nax.nf.lf <- nax.deaths

agesex <- substring(u_ids, 5, 6)
lfs <- which(agesex == "LF")

for(i in 1:length(lfs)) {
	nfpartner_ind <- vector()
	yearstoaddtonf <- vector()
	
	curid <- u_ids[lfs[i]]
	curid_ind <- lfs[i]
	nfpartner <- paste(substring(curid, 1, 4), "NF", sep = "")
	nfpartner_ind <- which(u_ids == nfpartner)
	
	
	if(length(nfpartner_ind) > 0) {
		tmpnax <- originalnax[c(curid_ind, nfpartner_ind),]
		yearstoaddtonf <- which(apply(tmpnax, 2, sum) == 2)
	
		if(length(yearstoaddtonf) > 0) {
			nax.nf.lf[nfpartner_ind, yearstoaddtonf] <- 2 
			print(i)
		}
	}

}

image(nax.nf.lf)

nax.deaths[which(nax.deaths > 1)] <- 1


#find out which ids were never seen together.
togethers <- tcrossprod(nax.deaths)
# neverseentogether <- which(togethers == 0)
# neverseentogether
togethers
}