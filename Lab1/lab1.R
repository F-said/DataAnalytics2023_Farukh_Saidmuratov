library(dplyr)
library(readxl)

# show don't tell assumes that the viewer knows how to see

# read in data
df_grump <- read.csv("Lab1/data/GPW3_GRUMP_SummaryInformation_2010.csv")
df_epi <- read_excel("Lab1/data/2010EPI_data.xls", sheet = "EPI2010_all countries")
EPI_data <- read.csv("Lab1/data/2010EPI_data.csv")

# modify EPI_data to fix header name
names(EPI_data) <- as.matrix(EPI_data[1,])
EPI_data <- EPI_data[-1,]

# set as default object in env
attach(EPI_data)

# some simple data editor
# fix(EPI_data)

# view EPI_data EPI column
EPI

# fix missing vals
tf <- is.na(EPI)
E <- EPI[!tf]

# convert to num
EPI <- as.numeric(as.character(EPI))

# get summary statistics of EPI column
summary(EPI)
fivenum(EPI, na.rm=TRUE)

# visualize EPI histogram
stem(EPI)
hist(EPI)
hist(EPI, seq(30., 95., 1.0), prob=TRUE)
lines(density(EPI, na.rm=TRUE, bw=1.))
lines(density(EPI, na.rm=TRUE, bw="SJ"))
rug(EPI)

# cumulative density function
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE)
par(pty='s')
qqnorm(EPI)
qqline(EPI)

### EXERCISE 1 : exploring air health & water health

# convert to num
AIR_H <- as.numeric(as.character(AIR_H))
WATER_H <- as.numeric(as.character(WATER_H))

boxplot(AIR_H, WATER_H, names=c("Air Health","Water Health"))

nulls_air <- is.na(AIR_H)
clean_air <- AIR_H[!nulls_air]

nulls_water <- is.na(WATER_H)
clean_water <- WATER_H[!nulls_water]

hist(clean_air)
hist(clean_air, seq(0, 100, 1.0), prob=TRUE)
lines(density(clean_air, na.rm=TRUE, bw="SJ"))
rug(clean_air)

hist(clean_water)
hist(clean_water, seq(0, 100, 1.0), prob=TRUE)
lines(density(clean_water, na.rm=TRUE, bw="SJ"))
rug(clean_water)

plot(ecdf(clean_air), do.points=FALSE, verticals=TRUE)
par(pty='s')
qqnorm(clean_air)
qqline(clean_air)

plot(ecdf(clean_water), do.points=FALSE, verticals=TRUE)
par(pty='s')
qqnorm(clean_water)
qqline(clean_water)

# notice correlation of two
plot(clean_air, clean_water)
abline(lm(clean_water ~ clean_air))

### EXERCISE 2 : filtering
# look at difference of other features, even when they fall into the same class

EPILand <- EPI[!Landlock]
ELand <- EPILand[!is.na(EPILand)]
hist(ELand)
hist(ELand, seq(30., 95., 1.0), prob=TRUE)
lines(density(ELand, na.rm=TRUE, bw="SJ"))
rug(ELand)

# repeat exercise 1 
plot(ecdf(EPILand), do.points=FALSE, verticals=TRUE)
par(pty='s')
qqnorm(EPILand)
qqline(EPILand)

# repeat filter on group, east-europe
EPI_East_Eur <- EPI[EPI_regions=="Eastern Europe and Central Asia"]
EPI_East_Eur <- EPI_East_Eur[!is.na(EPI_East_Eur)]

hist(EPI_East_Eur)
hist(EPI_East_Eur, seq(30., 95., 1.0), prob=TRUE)
lines(density(EPI_East_Eur, na.rm=TRUE, bw="SJ"))
rug(EPI_East_Eur)

plot(ecdf(EPI_East_Eur), do.points=FALSE, verticals=TRUE)
par(pty='s')
qqnorm(EPI_East_Eur)
qqline(EPI_East_Eur)

### GPW3_GRUMP : read in df & explore via exercise / exercise 2
df_grump


### WATER_TREATMENT : repeat exercise 1 & 2 for water


