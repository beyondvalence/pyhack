# pca
# 20150924THU

recorders <- data.frame("X"=c(0,0,1,1), "Y"=c(0,1,1,0), row.names = c("A", "B", "C", "D"))

locs <- data.frame("X"=c(.3, .5), "Y"=c(.8,.2))

intensities <- data.frame("sine"=sin(0:99*(pi/10))+1.2,
                          "cos"=.7*cos(0:99*(pi/15))+.9)

# lights combine linearly
dists <- matrix(nrow=dim(locs)[1], ncol=dim(recorders)[1],
                dimnames=list(NULL, row.names(recorders)))

for (i in 1:dim(dists)[2]) {
  dists[,i]=sqrt((locs$X-recorders$X[i])^2 +
                 (locs$Y-recorders$Y[i])^2)
}

set.seed(500)
# results
recorded.data <- data.frame(jitter(as.matrix(intensities)%*%
                                     as.matrix(exp(-2*dists)), amount=0))

plot(recorded.data)
round(cor(recorded.data),2)
plot.ts(recorded.data)
