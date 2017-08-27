#get rid of diagonals since they are all one
diag(assocm) <- NA
individualmeans <- apply(assocm, 1, mean, na.rm = TRUE)
individualsds   <- apply(assocm, 1, sd,   na.rm = TRUE)
individualmaxes <- apply(assocm, 1, max,  na.rm = TRUE)

#one has no max
individualmaxes[which(individualmaxes == -Inf)] <- NA

csvout <- data.frame(individualmeans, individualsds, individualmaxes, numsamp, numrec)
write.table(csvout, "individualstats_10km_ge10nsamp_adj.csv", sep = ',')