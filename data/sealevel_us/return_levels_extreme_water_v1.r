# Reference:
#	https://ulyssepasquier.wordpress.com/2017/02/06/r-extracting-data-needed-for-plotting-fevd-in-extremes-package-extreme-value-analysis/

library(extRemes)
library(evd)
library(ggplot2)

# pot.mle must be an extRemes fevd object
getrlpoints <- function(pot.mle){
 
xp2 <- ppoints(pot.mle$n, a = 0)
ytmp <- datagrabber(pot.mle)
y <- c(ytmp[, 1])
sdat <- sort(y)
npy <- pot.mle$npy
u <- pot.mle$threshold
rlpoints.x <- -1/log(xp2)[sdat > u]/npy
rlpoints.y <- sdat[sdat > u]
rlpoints <- data.frame(rlpoints.x, rlpoints.y)
 
return(rlpoints)
}

getcidf <- function(pot.mle){
 
rperiods = c(2, 5, 10, 20, 50, 80, 100, 120, 200, 250, 300, 500, 800)
bds <- ci(pot.mle, return.period = rperiods)
c1 <- as.numeric(bds[,1])
c2 <- as.numeric(bds[,2])
c3 <- as.numeric(bds[,3])
ci_df <- data.frame(c1, c2, c3, rperiods) 
 
return(ci_df)
}

# Set up the points; pot.mle must be an extRemes fevd object
z = revd(100, loc=0.848, shape=0.236, scale=0.224)
pot.mle = fevd(z)
rlpoints <- getrlpoints(pot.mle)
ci_df <- getcidf(pot.mle)

ggplot() +
 geom_line(data = ci_df, aes(x = rperiods, y = c2), color = "red") +
 geom_line(data = ci_df, aes(x = rperiods, y = c1), color = "grey") +
 geom_line(data = ci_df, aes(x = rperiods, y = c3), color = "grey") +
 geom_point(data = rlpoints, aes(x = rlpoints.x, y = rlpoints.y), size = 2) +
 ylab("Return Level (m)") +
 xlab("Return Period (Years)") +
 scale_x_log10(expand = c(0, 0), breaks = c(2,5,10,20,50,100,200,500), limits = c(1,1000))+
 theme_bw() +
 theme(axis.text = element_text(size=12), axis.title = element_text(size = 14, face = "bold"))


