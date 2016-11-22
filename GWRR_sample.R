library(gwrr)
data(columbus)
locs <- cbind(columbus$x, columbus$y)


# GWR
#col.gwr <- gwr.est(crime ~ income + houseval, locs, columbus, "exp")
col.bw <- gwr.bw.est(crime ~ income + houseval, locs, columbus, "exp")
col.gwr <- gwr.est(crime ~ income + houseval, locs, columbus, "exp", bw=col.bw$phi)
plot(col.gwr$beta[2,], col.gwr$beta[3,])
plot(columbus$x, columbus$y, cex=col.gwr$beta[1,]/10)


# diagnostic
col.bw <- gwr.bw.est(crime ~ income + houseval, locs, columbus, "exp")
col.vdp <- gwr.vdp(crime ~ income + houseval, locs, columbus, col.bw$phi, "exp")
hist(col.vdp$condition)

# GW ridge regression
col.gwrr <- gwrr.est(crime ~ income + houseval, locs, columbus, "exp", bw=2.00, rd=0.03) 
plot(col.gwrr$beta[2,], col.gwrr$beta[3,])
plot(columbus$x, columbus$y, cex=col.gwrr$beta[1,]/10)
col.gwr <- gwrr.est(crime ~ income + houseval, locs, columbus, "exp", bw=col.gwrr$phi, rd=0)


# GW lasso
col.gwl <- gwl.est(crime ~ income + houseval, locs, columbus, "exp")
plot(col.gwl$beta[2,], col.gwl$beta[3,])
plot(columbus$x, columbus$y, cex=col.gwl$beta[1,]/10)