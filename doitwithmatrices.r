#####################
#debugging functions#
#####################

# findapair <- function(a,b) {
	# gotcha <- vector()

	# for(i in 1:length(u_dates)) {
		# dids <- dailyids[[i]]
		
		# if(length(which(dids == u_ids[a])) > 0 & length(which(dids == u_ids[b])) > 0) {
			# gotcha <- c(gotcha, i)
		# }
	# }

	# return(gotcha)
# }



###############################
#use these functions for outer#
###############################

# ones <- function(n, p) {
	# return(rep(1, length(n)))
# }

	pairdistances <- function(n, p) {
		lat1 <- ww[n, 'yy']
		lon1 <- ww[n, 'xx']
		lat2 <- ww[p, 'yy']
		lon2 <- ww[p, 'xx']
					
		dists <- latlond(lat1, lon1, lat2, lon2)
		
		return(dists)
	}
	
	timedifferences <- function(n, p) {
		timedif <- dateswithtimes[p] - dateswithtimes[n]
		timedif.hours <- abs(as.numeric(timedif, units = "hours"))
		return(timedif.hours)
	}

source("birthdeath.r")
source("x:/cioffi.r")
source("distance.R")

diwm <- function(max_assoc_distance, numsampcutoff = 10) {
max_assoc_distance <- 20 #km
numsampcutoff <- 35;
relative_speed_km_per_hour <- 3.1 * 2

ww <- read.table("Master_DATA_primary.csv", header = TRUE, sep = ',')
ww[, 'xx'] <- ww[, 'xx']*-1 # correct the lons

dates <- cond2(ww$year, ww$month, ww$day, UTM = FALSE)
dateswithtimes <- convertdates(ww$year, ww$month, ww$day, ww$hour, ww$min, UTM = FALSE)
u_dates <- unique(dates)


u_ids <- sort(unique(ww$ID))
nids <- length(u_ids)
nsamp <- length(u_dates)

assoc <- matrix(0, nids, nids)
rownames(assoc) <- u_ids
colnames(assoc) <- u_ids

nax <- matrix(0, nids, nsamp)
rownames(nax) <- u_ids

starts <- Sys.time()
pb <- txtProgressBar(style = 3)
for(i in 1:nsamp) {
setTxtProgressBar(pb, i/nsamp)
	samp <- which(dates == u_dates[i])
	
	cur_ids <- ww[samp,]$ID
	nax[match(cur_ids, u_ids), i] <- 1

	dists <- outer(samp,samp, FUN = pairdistances)
	diag(dists) <- NA

	timediffs <- outer(samp, samp, FUN = timedifferences)
	diag(timediffs) <- NA

	new_dists <- dists + (relative_speed_km_per_hour * timediffs)
	close_enough <- which(new_dists <= max_assoc_distance, arr.ind = TRUE)

	if(nrow(close_enough) > 0) {
		a <- match(ww$ID[samp[close_enough[,1]]], u_ids)
		b <- match(ww$ID[samp[close_enough[,2]]], u_ids)
		kill <- which(a == b)
		if(length(kill) > 0) {
			a <- a[-1*kill]
			b <- b[-1*kill]
		}
		goods <- unique(c((b-1)*nids + a))
		assoc[goods] <- assoc[goods] + 1
		}
}
close(pb)
Sys.time() - starts

idcount <- data.frame(rowSums(nax))
nmatrix <- outer(idcount[,1], idcount[,1], FUN = "+")

diag(nmatrix) <- 0
diag(assoc) <- 0

yab_prime <- tcrossprod(nax) #equivalent to nax %*% t(nax)
yab <- yab_prime - assoc

assrate <- assoc / (nmatrix - assoc - yab)
diag(assrate) <- 1

togethers <- birthdeath(ww)
neverseentogether <- which(togethers == 0)
nmatrix[neverseentogether] <- NA

###################################################filter by number of days
numsamp <- apply(nax, 1, sum)
numrec <- table(ww$ID)

keep <- which(numsamp >= numsampcutoff)
# keep <- which(numsamp >= 50)
# kill <- which(substring(names(keep), 6, 6) == "U")
# keep <- keep[-kill]


#prep for social dif
assoc_socialdif <- assoc[keep, keep]
nmatrix_socialdif <- nmatrix[keep, keep]
# m <- assoc_socialdif
# x <- assoc_socialdif[upper.tri(m)]
# n <- nmatrix_socialdif[upper.tri(nmatrix_socialdif)]

# N <- length(n)
# Sest <- sd(x/n)/mean(x/n)
# muest <- mean(x/n)
# sdest <- sd(x/n)


###################################################make a comparable output to socprog

yab_socialdif <- yab[keep, keep]

assrate_socialdif <- assoc_socialdif / (nmatrix_socialdif - assoc_socialdif - yab_socialdif)
diag(assrate_socialdif) <- 1


# age <- substring(rownames(assrate_socialdif), 5, 5)
# sex <- substring(rownames(assrate_socialdif), 6, 6)
# kill <- unique(c(which(age == "U"), which(sex == "U")))
# assrate_socialdif <- assrate_socialdif[-kill, -kill]
# yab_socialdif <- yab_socialdif[-kill, -kill]
# assoc_socialdif <- assoc_socialdif[-kill, -kill]
# nmatrix_socialdif <- nmatrix_socialdif[-kill, -kill]


write.table(rownames(assrate_socialdif), "names_20km.ge35numsamp_nodead.csv", row.names = FALSE, sep = ',', col.names = FALSE)
# write.table(assrate_socialdif, "assocrate_5km.ge35numsamp_nodead.csv", sep = ',', col.names = NA)


# library(R.matlab)
# writeMat("assocrate_05km.ge35numsamp_nodead.mat", assocrate = assrate_socialdif)
# writeMat("moremats_10km_10numsamp_noU_noDead.mat", yab = yab_socialdif, assoc = assoc_socialdif, nmatrix = nmatrix_socialdif, assocrate = assrate_socialdif)


###################################################

assrate2 <- assrate_socialdif
diag(assrate2) <- NA
keepassoc <- assrate2

meanassocrates <- as.data.frame(apply(keepassoc,1,mean, na.rm = TRUE))
# write.table(meanassocrates, "mean-adj-assoc-rates.csv", col.names = TRUE, row.names = TRUE, sep = ',')

agesex <- substring(rownames(keepassoc), 5,6)
# sex <- substring(rownames(keepassoc), 6, 6)

# m <- which(sex == "M")
# f <- which(sex == "F")
# mean(keepassoc[m,m], na.rm = TRUE)
# mean(keepassoc[m,f], na.rm = TRUE)
# mean(keepassoc[f,f], na.rm = TRUE)


jf <- which(agesex == "JF")
nf <- which(agesex == "NF")
uf <- which(agesex == "UF")
lf <- which(agesex == "LF")

am <- which(agesex == "AM")
um <- which(agesex == "UM")
jm <- which(agesex == "JM")

au <- which(agesex == "AU")
ju <- which(agesex == "JU")
uu <- which(agesex == "UU")


focal <- jf
jfpaired <- c(
mean(keepassoc[focal,jf], na.rm = TRUE),
mean(keepassoc[focal,jm], na.rm = TRUE),
mean(keepassoc[focal,lf], na.rm = TRUE),
mean(keepassoc[focal,nf], na.rm = TRUE),
mean(keepassoc[focal,am], na.rm = TRUE)
)

focal <- jm
jmpaired <- c(
mean(keepassoc[focal,jf], na.rm = TRUE),
mean(keepassoc[focal,jm], na.rm = TRUE),
mean(keepassoc[focal,lf], na.rm = TRUE),
mean(keepassoc[focal,nf], na.rm = TRUE),
mean(keepassoc[focal,am], na.rm = TRUE)
)

focal <- lf
lfpaired <- c(
mean(keepassoc[focal,jf], na.rm = TRUE),
mean(keepassoc[focal,jm], na.rm = TRUE),
mean(keepassoc[focal,lf], na.rm = TRUE),
mean(keepassoc[focal,nf], na.rm = TRUE),
mean(keepassoc[focal,am], na.rm = TRUE)
)

focal <- nf
nfpaired <- c(
mean(keepassoc[focal,jf], na.rm = TRUE),
mean(keepassoc[focal,jm], na.rm = TRUE),
mean(keepassoc[focal,lf], na.rm = TRUE),
mean(keepassoc[focal,nf], na.rm = TRUE),
mean(keepassoc[focal,am], na.rm = TRUE)
)

focal <- am
ampaired <- c(
mean(keepassoc[focal,jf], na.rm = TRUE),
mean(keepassoc[focal,jm], na.rm = TRUE),
mean(keepassoc[focal,lf], na.rm = TRUE),
mean(keepassoc[focal,nf], na.rm = TRUE),
mean(keepassoc[focal,am], na.rm = TRUE)
)

# vevery <- c(
# mean(keepassoc[jf,], na.rm = TRUE),
# mean(keepassoc[jm,], na.rm = TRUE),
# mean(keepassoc[lf,], na.rm = TRUE),
# mean(keepassoc[nf,], na.rm = TRUE),
# mean(keepassoc[am,], na.rm = TRUE)
# )


focal <- jf
jfpaired_max <- c(
max(keepassoc[focal,jf], na.rm = TRUE),
max(keepassoc[focal,jm], na.rm = TRUE),
max(keepassoc[focal,lf], na.rm = TRUE),
max(keepassoc[focal,nf], na.rm = TRUE),
max(keepassoc[focal,am], na.rm = TRUE)
)

focal <- jm
jmpaired_max <- c(
max(keepassoc[focal,jf], na.rm = TRUE),
max(keepassoc[focal,jm], na.rm = TRUE),
max(keepassoc[focal,lf], na.rm = TRUE),
max(keepassoc[focal,nf], na.rm = TRUE),
max(keepassoc[focal,am], na.rm = TRUE)
)

focal <- lf
lfpaired_max <- c(
max(keepassoc[focal,jf], na.rm = TRUE),
max(keepassoc[focal,jm], na.rm = TRUE),
max(keepassoc[focal,lf], na.rm = TRUE),
max(keepassoc[focal,nf], na.rm = TRUE),
max(keepassoc[focal,am], na.rm = TRUE)
)

focal <- nf
nfpaired_max <- c(
max(keepassoc[focal,jf], na.rm = TRUE),
max(keepassoc[focal,jm], na.rm = TRUE),
max(keepassoc[focal,lf], na.rm = TRUE),
max(keepassoc[focal,nf], na.rm = TRUE),
max(keepassoc[focal,am], na.rm = TRUE)
)

focal <- am
ampaired_max <- c(
max(keepassoc[focal,jf], na.rm = TRUE),
max(keepassoc[focal,jm], na.rm = TRUE),
max(keepassoc[focal,lf], na.rm = TRUE),
max(keepassoc[focal,nf], na.rm = TRUE),
max(keepassoc[focal,am], na.rm = TRUE)
)

# vevery_max <- c(
# max(keepassoc[jf,], na.rm = TRUE),
# max(keepassoc[jm,], na.rm = TRUE),
# max(keepassoc[lf,], na.rm = TRUE),
# max(keepassoc[nf,], na.rm = TRUE),
# max(keepassoc[am,], na.rm = TRUE)
# )


focal <- jf
jfpaired_sd <- c(
sd(keepassoc[focal,jf], na.rm = TRUE),
sd(keepassoc[focal,jm], na.rm = TRUE),
sd(keepassoc[focal,lf], na.rm = TRUE),
sd(keepassoc[focal,nf], na.rm = TRUE),
sd(keepassoc[focal,am], na.rm = TRUE)
)

focal <- jm
jmpaired_sd <- c(
sd(keepassoc[focal,jf], na.rm = TRUE),
sd(keepassoc[focal,jm], na.rm = TRUE),
sd(keepassoc[focal,lf], na.rm = TRUE),
sd(keepassoc[focal,nf], na.rm = TRUE),
sd(keepassoc[focal,am], na.rm = TRUE)
)

focal <- lf
lfpaired_sd <- c(
sd(keepassoc[focal,jf], na.rm = TRUE),
sd(keepassoc[focal,jm], na.rm = TRUE),
sd(keepassoc[focal,lf], na.rm = TRUE),
sd(keepassoc[focal,nf], na.rm = TRUE),
sd(keepassoc[focal,am], na.rm = TRUE)
)

focal <- nf
nfpaired_sd <- c(
sd(keepassoc[focal,jf], na.rm = TRUE),
sd(keepassoc[focal,jm], na.rm = TRUE),
sd(keepassoc[focal,lf], na.rm = TRUE),
sd(keepassoc[focal,nf], na.rm = TRUE),
sd(keepassoc[focal,am], na.rm = TRUE)
)

focal <- am
ampaired_sd <- c(
sd(keepassoc[focal,jf], na.rm = TRUE),
sd(keepassoc[focal,jm], na.rm = TRUE),
sd(keepassoc[focal,lf], na.rm = TRUE),
sd(keepassoc[focal,nf], na.rm = TRUE),
sd(keepassoc[focal,am], na.rm = TRUE)
)

# # vevery_sd <- c(
# sd(keepassoc[jf,], na.rm = TRUE),
# sd(keepassoc[jm,], na.rm = TRUE),
# sd(keepassoc[lf,], na.rm = TRUE),
# sd(keepassoc[nf,], na.rm = TRUE),
# sd(keepassoc[am,], na.rm = TRUE)
# )


# vevery_lower <- c(
# quantile(keepassoc[jf,], 0.05, na.rm = TRUE),
# quantile(keepassoc[jm,], 0.05, na.rm = TRUE),
# quantile(keepassoc[lf,], 0.05, na.rm = TRUE),
# quantile(keepassoc[nf,], 0.05, na.rm = TRUE),
# quantile(keepassoc[am,], 0.05, na.rm = TRUE)
# )

# vevery_upper <- c(
# quantile(keepassoc[jf,], 0.95, na.rm = TRUE),
# quantile(keepassoc[jm,], 0.95, na.rm = TRUE),
# quantile(keepassoc[lf,], 0.95, na.rm = TRUE),
# quantile(keepassoc[nf,], 0.95, na.rm = TRUE),
# quantile(keepassoc[am,], 0.95, na.rm = TRUE)
# )

# vevery_sd <- c(
# sd(keepassoc[jf,], na.rm = TRUE),
# sd(keepassoc[jm,], na.rm = TRUE),
# sd(keepassoc[lf,], na.rm = TRUE),
# sd(keepassoc[nf,], na.rm = TRUE),
# sd(keepassoc[am,], na.rm = TRUE)
# )


comparemat <- matrix(c(jfpaired, jmpaired, lfpaired, nfpaired, ampaired), byrow = TRUE, 5,5)
comparemat_max <- matrix(c(jfpaired_max, jmpaired_max, lfpaired_max, nfpaired_max, ampaired_max), byrow = TRUE, 5,5)
comparemat_sd <- matrix(c(jfpaired_sd, jmpaired_sd, lfpaired_sd, nfpaired_sd, ampaired_sd), byrow = TRUE, 5,5)

rownames(comparemat) <- c("jf", "jm", "lf", "nf", "am")
colnames(comparemat) <- c("jf", "jm", "lf", "nf", "am")

rownames(comparemat_max) <- c("jf", "jm", "lf", "nf", "am")
colnames(comparemat_max) <- c("jf", "jm", "lf", "nf", "am")

rownames(comparemat_sd) <- c("jf", "jm", "lf", "nf", "am")
colnames(comparemat_sd) <- c("jf", "jm", "lf", "nf", "am")

round(comparemat, 4)
round(comparemat_max, 4)
round(comparemat_sd, 4)

comparemat_paste <- paste(round(comparemat, 4), " (",round(comparemat_sd,4), ")", sep = "")
comparemat_paste_max <- paste(round(comparemat_max, 4), " (",round(comparemat_sd,4), ")", sep = "")

matkey <- matrix(c(1:25), 5, 5)
compare.df <- data.frame(rep("a", 5), rep("b", 5), rep("c", 5), rep("d", 5), rep("e", 5))
colnames(compare.df) <- c("jf", "jm", "lf", "nf", "am")
rownames(compare.df) <- c("jf", "jm", "lf", "nf", "am") 


for(i in 1:ncol(compare.df)) {
	levels(compare.df[,i]) <- c(comparemat_paste, "")
}

for(i in 1:25) {
	cc <- which(matkey == i, arr.ind = TRUE)
	
	compare.df[cc[1], cc[2]] <- comparemat_paste[i]
}


compare.max.df <- data.frame(rep("a", 5), rep("b", 5), rep("c", 5), rep("d", 5), rep("e", 5))
colnames(compare.max.df) <- c("jf", "jm", "lf", "nf", "am")
rownames(compare.max.df) <- c("jf", "jm", "lf", "nf", "am")

for(i in 1:ncol(compare.max.df)) {
	levels(compare.max.df[,i]) <- c(comparemat_paste_max, "")
}

for(i in 1:25) {
	cc <- which(matkey == i, arr.ind = TRUE)
	
	compare.max.df[cc[1], cc[2]] <- comparemat_paste_max[i]
}

compare.df[upper.tri(compare.df)] <- ""
compare.max.df[upper.tri(compare.max.df)] <- ""

compare.max.df.filename <- paste(max_assoc_distance, "km.ge", numsampcutoff, "numsamp.max.csv", sep = '')
compare.df.filename <- paste(max_assoc_distance, "km.ge", numsampcutoff, "numsamp.csv", sep = '')

write.table(compare.max.df, compare.max.df.filename, sep = ',', col.names = NA)
write.table(compare.df, compare.df.filename, sep = ',', col.names = NA)

# plot(vevery, pch = 16, ylim = c(min(vevery - vevery_sd), max(vevery + vevery_sd)), axes = FALSE)
# segments(1:5, vevery - vevery_sd, 1:5, vevery + vevery_sd)
# axis(2)
# axis(1, at = c(1:5), labels = c("jf", "jm", "lf", "nf", "am"))

# ducksinarow <- as.vector(comparemat)
# labs <- c("jf", "jm", "lf", "nf", "am")

# pastetogether <- function(a, b) {
	# paste(a,b, sep = "-")
# }

# labs2 <- as.vector(outer(labs,labs, FUN = pastetogether))
# compare2 <- as.vector(comparemat)

# # par(mfrow = c(3,1))
# plot(compare2, axes = FALSE, pch = 16)
# axis(2)
# axis(1, at = 1:length(compare2), labels = labs2)
# box()

# # plot(sort(compare2), axes = FALSE, pch = 16)
# # axis(2)
# # axis(1, at = 1:length(compare2), labels = labs2[order(compare2)])
# # box()


# ###################DIST DENS#############

# range(c(
# keepassoc[jf,],
# keepassoc[jm,],
# keepassoc[lf,],
# keepassoc[nf,],
# keepassoc[am,]
# ), na.rm = TRUE)

# allall <- c(jf, jm, lf, nf, am)

# pdf("numsampGT10_alive.pdf", 6, 6)
# par(mfrow = c(1,2), oma = c(0,3.1,0,1.1), mar = c(4.1,3.1,2.1,0), las = 1)
# plot(1:10, 1:10, type = 'n', xlim = c(-.01,.03), ylim = c(0, 750), xlab = "", ylab = "")
# title("first peak")
# lines(density(keepassoc[jf,allall], na.rm = TRUE), col = cioffi_twelve_colors()[1])
# lines(density(keepassoc[jm,allall], na.rm = TRUE), col = cioffi_twelve_colors()[2])
# lines(density(keepassoc[lf,allall], na.rm = TRUE), col = cioffi_twelve_colors()[3])
# lines(density(keepassoc[nf,allall], na.rm = TRUE), col = cioffi_twelve_colors()[4])
# lines(density(keepassoc[am,allall], na.rm = TRUE), col = cioffi_twelve_colors()[5])
# # legend(0.015, 500, c("JF-", "JM-", "LF-", "NF-", "AM-"), lty = 1, col = cioffi_twelve_colors()[1:5])
# mtext("density", side = 2, line = 3)

# plot(1:10, 1:10, type = 'n', xlim = c(0.002,.03), ylim = c(0, 30), xlab = "", ylab = "")
# title("second peak")
# lines(density(keepassoc[jf,allall], na.rm = TRUE), col = cioffi_twelve_colors()[1])
# lines(density(keepassoc[jm,allall], na.rm = TRUE), col = cioffi_twelve_colors()[2])
# lines(density(keepassoc[lf,allall], na.rm = TRUE), col = cioffi_twelve_colors()[3])
# lines(density(keepassoc[nf,allall], na.rm = TRUE), col = cioffi_twelve_colors()[4])
# lines(density(keepassoc[am,allall], na.rm = TRUE), col = cioffi_twelve_colors()[5])
# legend(0.015, 30, c("JF-", "JM-", "LF-", "NF-", "AM-"), lty = 1, col = cioffi_twelve_colors()[1:5])
# mtext("simple ratio association rate", line = 3, side = 1, at = -0.005)
# dev.off()

# hist(keepassoc[am,], nclass = 1000,prob = TRUE)

# keepassoc[jm,]
# keepassoc[lf,]
# keepassoc[nf,]
# keepassoc[am,]

# #########################################
# #########################################


# m <- assoc
# x <- assoc[upper.tri(m)]
# n <- nmatrix[upper.tri(nmatrix)]

# keep <- which(!is.na(n))
# x <- x[keep]
# n <- n[keep]


# N <- length(n)
# Sest <- sqrt(var(x) - mean(x)) / mean(x)
# muest <- mean(x/n)
# sdest <- muest * Sest
}
