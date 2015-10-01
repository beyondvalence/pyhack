
# Machine learning Hackers
# Monday April 7th, 2014
# Chapters 1,2

library(ggplot2)

# directory <- "~/R/hackers/data/01-Introduction/data/ufo"
# help.search("directory") # search help
list.files() # files in cwd
getwd() # returns working directory
loc <- paste0(getwd(), "/01-Introduction/data/ufo")
loc
# 1.1 read in tsv ####
filename <- paste0(loc, "/ufo_awesome.tsv")
ufo <- read.delim(filename, sep="\t",
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
location.matrix <- do.call(rbind, city.state)
head(location.matrix)
# create new columns in ufo
ufo <- transform(ufo, USCity=location.matrix[,1], 
                 USState=tolower(location.matrix[,2]), 
                 stringsAsFactors=FALSE)

# 1.4 data in our scope- USA ####
us.states <- c("ak", "al", "ar", "az", "ca", "co", "ct",
               "de", "fl", "ga", "hi", "ia", "id", "il",
               "in", "ks", "ky", "la", "ma", "md", "me",
               "mi", "mn", "mo", "ms", "mt", "mc", "nd", 
               "ne", "nh", "nj", "nm", "nv", "ny", "oh", 
               "ok", "or", "pa", "ri", "sc", "sd", "tn",
               "tx", "ut", "va", "vt", "wa", "wi", "wv",
               "wy")
ufo$USState <- us.states[match(ufo$USState, us.states)]
ufo$USCity[is.na(ufo$USState)] <- NA
ufo.us <- subset(ufo, !is.na(ufo$USState))
ufo.us <- ufo.us[!is.na(ufo.us$DateOccurred),]
tail(ufo$USState)
tail(ufo.us$USState)

# 1.5 aggregating and organizing data ddply ####
library(scales)
library(png)
library(plyr)
library(ggplot2)
summary(ufo.us$DateOccurred) # earliest occurred in 1400!
# use ggplot
quick.hist <- ggplot(ufo.us, aes(x=DateOccurred)) + geom_histogram() + 
  scale_x_date(breaks="5 years", labels=date_format("%Y"))
print(quick.hist)
ggsave(plot=quick.hist, file=paste(loc,"quick_hist.png", sep="/"), height=6, width=8)
# subset ufo.us to recent 2 decades
ufo.us <- subset(ufo.us, DateOccurred>=as.Date("1990-01-01"))
# organize ufo by year-month state aggregates
ufo.us$YearMonth <- strftime(ufo.us$DateOccurred, format="%Y-%m")
# use plyr for group counts
sightings.counts <- ddply(ufo.us, .(USState, YearMonth), nrow)
head(sightings.counts)
sightings.counts[sample(1:nrow(sightings.counts),10),]
# fill in missing months with zeros
date.range <- seq.Date(from=as.Date(min(ufo.us$DateOccurred)),
                       to=as.Date(max(ufo.us$DateOccurred)), by="month")
date.strings <- strftime(date.range, format="%Y-%m")
state.dates <- lapply(us.states, function(s) cbind(s,date.strings))
state.dates <- data.frame(do.call(rbind, state.dates), stringsAsFactors=FALSE)
# match the extended state date to sightings
all.sightings <- merge(state.dates, sightings.counts, by.x=c("s", "date.strings"),
                       by.y=c("USState", "YearMonth"), all=TRUE)
head(all.sightings)
# housekeeping data.frame
names(all.sightings) <- c("State", "YearMonth", "Sightings")
all.sightings$Sightings[is.na(all.sightings$Sightings)] <- 0
all.sightings$YearMonth <- as.Date(rep(date.range, length(us.states)))
all.sightings$State <- as.factor(toupper(all.sightings$State))
save(all.sightings, file=paste(loc,"allsightings.RData", sep=))

# 1.6 analyzing data visually ####
# use ggplot and build layers
load(paste(loc,"/allsightings.RData",sep="/"))
library(ggplot2)
library(png)
library(scales)
state.plot <- ggplot(all.sightings, aes(x=YearMonth, y=Sightings))+
  geom_line(aes(color="db"))+
  facet_wrap(~State, nrow=10, ncol=5)+
  theme_bw()+
  scale_color_manual(values=c("db"="darkblue"))+
  scale_x_date(breaks="5 years", labels=date_format("%Y"), legend=NULL)+
  # legend or guide? book is old ggplot 0.8.9
  xlab("Time")+
  ylab("Number of Sightings")+
  labs(title="Number of UFO Sightings by Month-Year and U.S. State (1990-2010)")
print(state.plot)
ggsave(plot=state.plot, file=paste(loc,"stateplot.pdf",sep="/"), width=14, height=9)

# 2.1 Numeric Summaries ####
setwd("~/Documents/R/hackers/data/02-Exploration/data")
loc2 <- paste0(getwd(), "/02-Exploration/data")
heights.weights <- read.csv(paste(loc2,"01_heights_weights_genders.csv",sep="/"),
                            header=TRUE, stringsAsFactors=FALSE,
                            sep=",")
heights <- with(heights.weights, Height)
summary(heights)
c(mean(heights),median(heights), range(heights))
quantile(heights)

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
c(mean(heights)-sd(heights), mean(heights)+sd(heights))
range(heights)

# 2.3 Exploratory Data Visualization ####
library(ggplot2)
ggplot(heights.weights, aes(x=Height))+
  geom_histogram(binwidth=1) # bin width 1, default
ggplot(heights.weights, aes(x=Height))+
  geom_histogram(binwidth=5) # bin width 5, too smooth
ggplot(heights.weights, aes(x=Height))+
  geom_histogram(binwidth=0.01) # bin width 0.01

# to prevent over-under smoothing, use kernel density est.
ggplot(heights.weights, aes(x=Height))+
  geom_density(color="darkgreen")
# peak is flat, use qualitative variable
ggplot(heights.weights, aes(x=Height, fill=Gender))+
  geom_density()
ggplot(heights.weights, aes(x=Weight, fill=Gender))+
  geom_density() # mixture of two standard dist mixed into nonstandard dist
# faceted plot- grid
ggplot(heights.weights, aes(x=Weight, fill=Gender))+
  geom_density()+
  facet_grid(Gender~ .)

# 2.3.1 Tails of Distributions ####
set.seed(1)
normal.values <- rnorm(250,0,1)
cauchy.values <- rcauchy(250,0,1)
# cauchy has heavier tails than the normal distribution
# cauchy has 90% values within 3 deviations away from the mean, 
# whereas a normal distribution has 99% of its values 3 deviations away
range(normal.values)
range(cauchy.values)
ggplot(data.frame(X=normal.values), aes(x=X)) + geom_density()
ggplot(data.frame(X=cauchy.values), aes(x=X)) + geom_density()
# gamma distribution, produces only positive values
gamma.values <- rgamma(100000, 2,0.001)
ggplot(data.frame(X=gamma.values), aes(x=X)) + geom_density()

# 2.4 Visualizing the Relationships Between Columns ####
loc2 <- paste0(getwd(), "/02-Exploration/data")
heights.weights <- read.csv(paste(loc2,"01_heights_weights_genders.csv",sep="/"),
                            header=TRUE, stringsAsFactors=FALSE,
                            sep=",")
# two types of problems: regression
# predicts values of a column given other columns
ggplot(heights.weights, aes(x=Height, y=Weight)) + geom_point()
ggplot(heights.weights, aes(x=Height, y=Weight)) + geom_point() +geom_smooth()
# and classification
# of gender in the heights.weights dataset
# categorizes a predictor
ggplot(heights.weights, aes(x=Height,y=Weight, color=Gender)) + geom_point()

# preview of classication:
heights.weights <- transform(heights.weights, Male=ifelse(Gender=='Male', 1, 0))
logit.model <- glm(Male ~ Height + Weight, data=heights.weights, 
                   family=binomial(link='logit'))
ggplot(heights.weights, aes(x=Weight, y=Height, color=Gender)) + 
  geom_point() +
  stat_abline(intercept= -coef(logit.model)[1]/coef(logit.model)[2],
              slope= -coef(logit.model)[3]/coef(logit.model)[2],
              geom='abline',
              color='black')
