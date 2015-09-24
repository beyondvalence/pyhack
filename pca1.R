# pca
# 20150924THU

recorders <- data.frame("X"=c(0,0,1,1), "Y"=c(0,1,1,0), row.names = c("A", "B", "C", "D"))

locs <- data.frame("X"=c(.3, .5), "Y"=c(.8,.2))

intensities <- data.frame("sine"=sin(0:99*(pi/10))+1.2,
                          "cos"=.7*cos(0:99*(pi/15))+.9)