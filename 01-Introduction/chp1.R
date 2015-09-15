
# Machine Learning for Hackers
# Created: 20140407 MON
# Modified: 20150614 SUN
# Chapters 1, 2 ####

library(ggplot2)

setwd("~/Documents/R/hackers/data/01-Introduction/data/ufo")
help.search("directory") # search help
list.files() # files in cwd

# 1.1 read in tsv ####
ufo <- read.delim("ufo_awesome.tsv", sep="\t",
                  header=FALSE, stringsAsFactors=FALSE, na.strings="")

# 1.2 first 6 rows ####
head(ufo)
# append column names
names(ufo) <- c("DateOccurred", "DateReported", "Location", 
                "ShortDescription", "Duration", "LongDescription")
# change to " and ' in description
ufo[,6] <- gsub("&quot;", "\"", ufo[,6])
ufo[,6] <- gsub("&apos;", "\'", ufo[,6])
# format dates
ufo$DateOccurred <- as.Date(ufo$DateOccurred, format="%Y%m%d")
index <- sample(1:dim(ufo)[1], 10)
ufo$DateOccurred[index]
ufo$DateReported <- as.Date(ufo$DateReported, format="%Y%m%d")
table(nchar(ufo$DateOccurred))
ufo$DateOccurred[which(nchar(ufo$DateOccurred)==7)]

# 1.3 organize location data ####
# function to split location into (city, state) list
split.location <- function(l) {
  # function will throw error
  split.location <- tryCatch(strsplit(l,",")[[1]], 
                             error=function(e) return(c(NA, NA)))
  # remove space in front
  clean.location <- gsub("^ ", "", split.location)
  # city and state only, no international cities
  if (length(clean.location)>2) {
    return(c(NA, NA))
  }
  else {
    return(clean.location) 
  }
}

# apply splitting function to ufo locations
city.state <- lapply(ufo$Location, split.location)
head(city.state)
# create city state matrix
# use do.call to execute a function call over a list
location.matrix <- do.call(rbind, city.state)
head(location.matrix)
# create new columns in ufo
# using transform function
ufo <- transform(ufo, 
                 USCity=location.matrix[,1], 
                 USState=tolower(location.matrix[,2]), 
                 stringsAsFactors=FALSE)

# 1.4 data outside our scope- USA ####
# some states abbreviations are not from the US
# ex, no Canadian province abbreviations match US state abbreviations
us.states <- c("ak", "al", "ar", "az", "ca", "co", "ct",
               "de", "fl", "ga", "hi", "ia", "id", "il",
               "in", "ks", "ky", "la", "ma", "md", "me",
               "mi", "mn", "mo", "ms", "mt", "mc", "nd", 
               "ne", "nh", "nj", "nm", "nv", "ny", "oh", 
               "ok", "or", "pa", "ri", "sc", "sd", "tn",
               "tx", "ut", "va", "vt", "wa", "wi", "wv",
               "wy")
# matches first arg against second arg and returns
# vector of first arg length which subsets us.states
ufo$USState <- us.states[match(ufo$USState, us.states)]
# non matched states are NA, so change those cities to NA
ufo$USCity[is.na(ufo$USState)] <- NA
ufo.us <- subset(ufo, !is.na(ufo$USState))
head(ufo.us)
ufo.us <- ufo.us[!is.na(ufo.us$DateOccurred),]
# ufo.us dim = (50435,8)
tail(ufo$USState)
tail(ufo.us$USState)

# 1.5 aggregating and organizing data ddply ####
library(scales)
library(png)
library(plyr)
library(ggplot2)
summary(ufo.us$DateOccurred) # earliest occurred in 1400!
# 1.5.1 use ggplot ####
quick.hist <- ggplot(ufo.us, aes(x=DateOccurred)) + geom_histogram() + 
  scale_x_date(breaks="50 years", labels=date_format("%Y"))
print(quick.hist)
setwd("~/Documents/R/hackers/data/01-Introduction/images")
ggsave(plot=quick.hist, filename="quick_hist.png",
       height=6, width=8)
# subset ufo.us to recent 2 decades
ufo.us <- subset(ufo.us, DateOccurred>=as.Date("1990-01-01"))
nrow(ufo.us) # 45,271
quick.hist <- ggplot(ufo.us, aes(x=DateOccurred)) + geom_histogram() + 
  scale_x_date(breaks="5 years", labels=date_format("%Y"))
print(quick.hist)
#
# 1.5.2 organize ufo by year-month state aggregates ####
ufo.us$YearMonth <- strftime(ufo.us$DateOccurred, format="%Y-%m")
# use plyr for nrow group counts by state and year-month
sightings.counts <- ddply(ufo.us, .(USState, YearMonth), nrow)
head(sightings.counts)
sightings.counts[sample(1:nrow(sightings.counts),10),]
#
# 1.5.3 fill in missing months with zeros ####
date.range <- seq.Date(from=as.Date(min(ufo.us$DateOccurred)),
                       to=as.Date(max(ufo.us$DateOccurred)), by="month")
date.strings <- strftime(date.range, format="%Y-%m")
state.dates <- lapply(us.states, function(s) cbind(s,date.strings))
state.dates <- data.frame(do.call(rbind, state.dates), stringsAsFactors=FALSE)
# match the extended state date to sightings
all.sightings <- merge(state.dates, sightings.counts, by.x=c("s", "date.strings"),
                       by.y=c("USState", "YearMonth"), all=TRUE)
head(all.sightings)
#
# 1.5.4 housekeeping the data.frame ####
names(all.sightings) <- c("State", "YearMonth", "Sightings")
all.sightings$Sightings[is.na(all.sightings$Sightings)] <- 0
all.sightings$YearMonth <- as.Date(rep(date.range, length(us.states)))
all.sightings$State <- as.factor(toupper(all.sightings$State))
setwd("~/Documents/R/hackers/data/01-Introduction")
save(all.sightings, file="allsightings.RData")
#
# 1.6 analyzing data visually ####
# use ggplot and build layers
setwd("~/Documents/R/hackers/data/01-Introduction")
load("allsightings.RData")
library(ggplot2)
library(png)
library(scales)
state.plot <- ggplot(all.sightings, aes(x=YearMonth, y=Sightings))+
  geom_line(aes(color="db"))+ # darkblue = db
  facet_wrap(~State, nrow=10, ncol=5)+
  theme_bw()+
  scale_color_manual(values=c("db"="darkblue"))+
  scale_x_date(breaks="5 years", labels=date_format("%Y"), legend=NULL)+
  # legend or guide? book is old ggplot 0.8.9
  xlab("Time")+
  ylab("Number of Sightings")+
  labs(title="Number of UFO Sightings by Month-Year and U.S. State (1990-2010)")
setwd("~/Documents/R/hackers/data/01-Introduction/images")
ggsave(plot=state.plot, file="stateplot.png", width=14, height=9)
setwd("~/Documents/R/hackers/data/01-Introduction")
#
# chapter 2: Data Exploration ####
# Exploration versus Confirmation
#
# 2.1 Numeric Summaries ####
setwd("~/Documents/R/hackers/data/02-Exploration/data")
heights.weights <- read.csv("01_heights_weights_genders.csv",
                            header=TRUE, stringsAsFactors=FALSE,
                            sep=",")
heights <- with(heights.weights, Height)
summary(heights)
# Mean, Median, Mode, Quantiles
c(mean(heights),median(heights), range(heights))
quantile(heights)
#
# 2.2 standard deviations and variances ####
# middle 50%
quantile(heights, probs=c(0.25,0.75))
# variance, biased downwards from true value, so length-1
my.var <- function(x) {
  m <- mean(x)
  return(sum((x-m)^2)/(length(x)-1))
}
my.var(heights)-var(heights) # should be 0
my.sd <- function(x) { # standard deviation
  return(sqrt(my.var(x)))
}
my.sd(heights); my.var(heights)
# one standard deviation below and above the mean:
c(mean(heights)-sd(heights), mean(heights)+sd(heights))
range(heights)

# 2.3 Exploratory Data Visualization ####
library(ggplot2)
ggplot(heights.weights, aes(x=Height))+
  geom_histogram(binwidth=1) # bin width 1, default
ggplot(heights.weights, aes(x=Height))+
  geom_histogram(binwidth=5) # bin width 5, too blocky
ggplot(heights.weights, aes(x=Height))+
  geom_histogram(binwidth=0.01) # bin width 0.01, fine definition

# to prevent over-under smoothing, use kernel density est.
ggplot(heights.weights, aes(x=Height))+
  geom_density()
# peak is flat, use qualitative variable
ggplot(heights.weights, aes(x=Height, fill=Gender))+
  geom_density()
ggplot(heights.weights, aes(x=Weight, fill=Gender))+
  geom_density() # mixture of two standard dist mixed into nonstandard dist
# faceted plot- grid
ggplot(heights.weights, aes(x=Weight, fill=Gender))+
  geom_density()+
  facet_grid(Gender ~ .) # (row ~ column)
#
# 20150618
# changing the mean and variance will change the center and width
# of the bell curve
#
# Examining Cauchy curves with heavier tails
set.seed(1)
normal.val <- rnorm(250, 0, 1)
cauchy.val <- rcauchy(250, 0, 1)
range(normal.val)
range(cauchy.val)
ggplot(data.frame(x = normal.val), aes(x = x)) + geom_density()
ggplot(data.frame(x = cauchy.val), aes(x = x)) + geom_density()

