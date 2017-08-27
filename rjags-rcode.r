

#simulated data

# S <- 2.699
# mu <- .01
# sds <- S * mu
 # u1 <- mu*((1-mu)/(mu*S^2) - 1)
 # u2 <- (1-mu)*((1-mu)/(mu*S^2) -1)
# N <- 100
# n <- sample(1:400, N, replace = TRUE)
# p <- rbeta(N, u1, u2)
# x <- rbinom(N, n, p)

# # test data from socprog simdyaa.xls
# mm <- read.table("testdata.csv", header = FALSE, sep = ',')
# m <- as.matrix(table(mm))
# write.table(assoc, "assoc.matrix")
# nmatrix <- m*NA

# size <- dim(nmatrix)[1]
# pb <- txtProgressBar(style = 3)
	
# for(b in 1:size) {
	# setTxtProgressBar(pb, b/size)
	
	# for(a in 1:size) {
		# firstone <- which(nsamp$ids == rownames(m)[a])
		# secondone <- which( nsamp$ids == rownames(m)[b])
		
		# nmatrix[a,b] <- nsamp[firstone,2] + nsamp[secondone,2]
	# }
# }
# close(pb)

# x <- m[upper.tri(m)]
# n <- nmatrix[upper.tri(nmatrix)]

# N <- length(n)


# Sest <- sqrt(var(x) - mean(x))/mean(x)
# muest <- mean(x/n)

# sdest <- muest * Sest
 
 
library(rjags)
jags <- jags.model('jagsmod',
                   data = list('x' = x,
                               'N' = N,
							   'n' = n,
							   'cenmu' = muest,
							   'cenSD' = sdest),
				    inits = list('mu' = muest,
							     'sd'  = sdest),
                   n.chains = 3,
                   n.adapt = 100)
ng <- 10000

out <- coda.samples(jags,
             c('mu', 'sd'),
             ng)	 
			 
plot(out)
summary(out)

burnin <- .9 * ng
out.burn <- window(out, burnin, ng)
plot(out.burn)
summary(out.burn)


#how good is the fit?
ss <- as.matrix(out.burn)
mu <- apply(ss, 2, quantile, 0.50)[1]
sd <- apply(ss, 2, quantile, 0.50)[2]

a <- mu*((1-mu)/(mu*(sd/mu)^2) - 1)
b <- (1-mu)*((1-mu)/(mu*(sd/mu)^2) -1)

hist(x/n, nclass = 1000, prob = TRUE, xlim = c(0,0.1), ylim = c(0,2000))
lines(density(rbeta(seq(0,1, length.out = 1000), a, b)), col = "blue", lty = 2)