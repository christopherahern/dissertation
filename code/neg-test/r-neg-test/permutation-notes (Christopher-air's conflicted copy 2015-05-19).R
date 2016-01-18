##########################################################################
# Load libraries and scripts
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(xtable)
source("neg-data.R")
source("binning.R")
##########################################################################
# Read in data file and filter out texts and contexts
neg.data.full = cleanNegData("coding.cod.ooo")
neg.data.full = tbl_df(neg.data.full)
# Filter out tokens without do-support label, year, or type
excluded.texts = c("CMBOETH", "CMORM", "CMNTEST","CMOTEST")
# CMBOETH : translation of Boethius' "Consolation of Philosophy", which is notably stilted
# CMORM   : Ormulum is very specific poetic format
# CMOTEST, CMNTEST : Old and new testaments
# Exclude tokens where 'ne' is contracted, appears in negative concord, or looks like predicate negation
neg.data.full = neg.data.full %>% filter(! author %in% excluded.texts & exclude != "X") 
# Plot individual documents
neg.plot.auth = neg.data.full %>% group_by(year) %>% summarize(total=n(), ne=sum(neg.type=="ne", na.rm=TRUE)/total,not=sum(neg.type=="not", na.rm=TRUE)/total,ne.not=sum(neg.type=="both", na.rm=TRUE)/total)
neg.plot.auth = melt(test, id=c("year", "total"))
# Plot points and smooth fits
ggplot(aes(x = year, y = value, color = variable), data = neg.plot.auth) +
  geom_point(aes(size = total), alpha = 0.5, position = position_jitter()) +
  geom_smooth(aes(weight = total),method="loess", span=.5, se = FALSE, size=4) + # span=.5,
  xlab("Year") +   ylab("Proportion forms") +   scale_size_area("N", max_size = 20) +
  theme(text = element_text(size=30)) +   theme(legend.position="none") +
  coord_cartesian(xlim = c(1090,1540)) +  coord_cartesian(ylim = c(-.1,1.1))


# Get start and end from data frame
start = min(neg.data.full$year)
end = max(neg.data.full$year)
# Get years for bins
df.bins = seq(start, end,(end - start)/5)
df.quants = as.vector(quantile(neg.data.full$year, probs=seq(0,1,1/5)))
# Summarize data
sum.df = neg.data.full %>% 
  group_by(cut(year,df.bins, include.lowest=TRUE)) %>% 
  summarize(count=n())
#sum.df = neg.data.full %>% group_by(cut(year,df.bins, include.lowest=TRUE)) %>% summarize(count=n())
# Rename columns
colnames(sum.df) = c('year', 'count')
# Replace ranges with midpoints
years = as.integer((head(df.bins,-1) + tail(df.bins,-1))/2)
sum.df$year = years


do.by.year = function(df,n) {
  # Split data into fixed-width time bins #
  # df : data frame with two columns 
  # df$year : the year a given token occurs
  # df$value : the binary value of the variants, by default 1 is the incoming variant
  # n : number of fixed-width bins to split data into
  ###
  # Get start and end from data frame
  start = min(df$year)
  end = max(df$year)
  # Get years for bins
  df.bins = seq(start, end,(end - start)/n)
  # Summarize data
  sum.df = df %>% group_by(cut(year,df.bins, include.lowest=TRUE)) %>% summarize(value=sum(value), count=n())
  # Rename columns
  colnames(sum.df) = c('year','value','count')
  # Replace ranges with midpoints
  years = as.integer((head(df.bins,-1) + tail(df.bins,-1))/2)
  sum.df$year = years
  return(sum.df)
}

do.by.quantile = function(df,q) {
  # Split data into varaible-width time bins #
  # df : data frame with two columns 
  # df$year : the year a given token occurs
  # df$value : the binary value of the variants, by default 1 is the incoming variant
  # q : number of variable-width bins (qanttiles) to split data into
  ###
  # Get quantiles
  df.quant = as.vector(quantile(df$year, probs=seq(0,1,1/q)))
  # Summarize data
  sum.df = df %>% dplyr::group_by(cut(year,df.quant, include.lowest=TRUE)) %>% dplyr::summarize(value=sum(value), count=n())
  # Rename columns
  colnames(sum.df) = c('year','value','count')
  # Replace ranges with midpoints and filter data
  years = as.integer((head(df.quant,-1) + tail(df.quant,-1))/2)
  sum.df$year = years
  return(sum.df)
}