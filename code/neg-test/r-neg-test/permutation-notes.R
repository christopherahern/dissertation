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

neg.data.full %>% filter(author == "CMBOETH")
neg.data.full %>% filter(author == "CMORM")
neg.data.full %>% filter(author == "CMOTEST")
neg.data.full %>% filter(author == "CMNTEST")
# CMBOETH : translation of Boethius' "Consolation of Philosophy", which is considered stilted
# CMORM   : Ormulum is very specific poetic format
# CMOTEST, CMNTEST : Old and new testaments
# Lag the previous negative token
# Exclude tokens where 'ne' is contracted, appears in negative concord, or looks like predicate negation
# Group data into a number of fixed- and variable-width bins
number = 5
neg.data = neg.data.full %>% 
  mutate(prev.neg = lag(neg.type)) %>%
  filter(! author %in% excluded.texts & exclude != "X") %>%
  filter(!is.na(neg.type) & !is.na(prev.neg)) %>%
  mutate(n.bin = cut(year, seq(min(.$year), max(year), (max(year)-min(year))/number), include.lowest=TRUE),
         n.bin.label = cut(year, seq(min(.$year), max(year), (max(year)-min(year))/number), include.lowest=TRUE, label=FALSE),
         q.bin = cut(year, as.vector(quantile(.$year, probs=seq(0,1,1/number))), include.lowest=TRUE))

neg.test = neg.data %>% filter(n.bin.label == 1)
neg.data %>% filter(n.bin.label == 1) %>% 
  group_by(neg.type) %>% summarize(count = n())

neg.data %>% filter(n.bin.label == 1) %>% 
  group_by(prev.neg, neg.type) %>% 
  summarize(count=n())

summarize(total=n(), ne=sum(neg.type=="ne", na.rm=TRUE)/total,not=sum(neg.type=="not", na.rm=TRUE)/total,ne.not=sum(neg.type=="both", na.rm=TRUE)/total)

for (i in c(2:8)){
  print(i)
  print(seq(min(neg.data$year), max(neg.data$year), (max(neg.data$year)-min(neg.data$year))/i))
  print(as.vector(quantile(neg.data$year, probs=seq(0,1,1/i))))
}

seq(min(neg.data$year), max(neg.data$year), (max(neg.data$year)-min(neg.data$year))/number)
as.vector(quantile(neg.data$year, probs=seq(0,1,1/number)))

##########################################################################
neg.plot.auth = neg.data %>% 
  group_by(year, prev.neg) %>% 
  summarize(total=n(), ne=sum(neg.type=="ne", na.rm=TRUE)/total,not=sum(neg.type=="not", na.rm=TRUE)/total,ne.not=sum(neg.type=="both", na.rm=TRUE)/total)
neg.plot.auth =  melt(neg.plot.auth, id=c("year", "prev.neg", "total"))
# Plot points and smooth fits
ggplot(aes(x = year, y = value, color = variable), data = neg.plot.auth) +
  geom_point(aes(size = total), alpha = 0.5, position = position_jitter()) +
  geom_smooth(aes(weight = total), method="loess",se = FALSE) + # span=.5,
  xlab("Year") +   ylab("Proportion forms") +   scale_size_area("N", max_size = 20) +
  theme(text = element_text(size=30)) +   theme(legend.position="none") +
  coord_cartesian(xlim = c(1090,1540)) +  coord_cartesian(ylim = c(-.1,1.1)) +
  facet_wrap(~ prev.neg, nrow=1)
##########################################################################
neg.years =   neg.data %>% group_by(n.bin.label, prev.neg) %>%
  summarize(total=n(),
            ne.p=sum(neg.type=="ne")/total,
            ne.se = sqrt(ne.p*(1-ne.p)/total),
            ne.not.p=sum(neg.type=="both")/total,
            ne.not.se = sqrt(ne.not.p*(1-ne.not.p)/total),
            not.p=sum(neg.type=="not")/total,
            not.se = sqrt(not.p*(1-not.p)/total))

neg.years %>% filter(n.bin.label == 1)

neg.data %>% group_by(n.bin.label) %>%
  summarize(total=n(),
            ne.p=sum(neg.type=="ne")/total,
            ne.se = sqrt(ne.p*(1-ne.p)/total),
            ne.not.p=sum(neg.type=="both")/total,
            ne.not.se = sqrt(ne.not.p*(1-ne.not.p)/total),
            not.p=sum(neg.type=="not")/total,
            not.se = sqrt(not.p*(1-not.p)/total)) %>%
  filter(n.bin.label == 1)

neg.years.melt = melt(neg.years %>% select(n.bin, prev.neg, ne.p, not.p, ne.not.p), id=c('n.bin', 'prev.neg'))
#test = melt(neg.years %>% select(n.bin, ne.p, not.p, ne.not.p), id=c('n.bin'))
#ggplot(test, aes(x=n.bin, y=value, fill=variable)) + 
#  geom_bar(stat="identity", position="dodge")
neg.years.melt$variable = factor(neg.years.melt$variable, levels = c("ne.p", "ne.not.p", "not.p"))
neg.years.melt$prev.neg = factor(neg.years.melt$prev.neg, levels = c("ne", "both", "not"))
#test = c('n.bin', 'prev.neg', 'total')
#neg.years = melt(neg.years, id=test)
neg.years.melt = melt(neg.years %>% select(n.bin, prev.neg, ne.p, not.p, ne.not.p), id=c('n.bin', 'prev.neg'))
# Bar plots faceted  by previous form and time bin
ggplot(neg.years.melt, aes(x=n.bin, y=value, fill=variable)) + 
  geom_bar(stat="identity", position="dodge") + facet_wrap(~prev.neg)
ggplot(neg.years.melt, aes(x=prev.neg, y=value, fill=variable)) + 
  geom_bar(stat="identity", position="dodge") + facet_wrap(~ n.bin, nrow=1)
# Lines faceted by previous form and time bin
ggplot(neg.years.melt, aes(x=n.bin, y=value, color=variable)) + 
  #geom_bar(stat="identity", position="dodge", aes(fill=variable,alpha=.1)) +
  geom_line(position="identity",aes(group=variable), size=3) + 
  facet_wrap(~prev.neg)
ggplot(neg.years.melt, aes(x=prev.neg, y=value, color=variable)) + 
  #geom_bar(stat="identity", position="dodge", aes(fill=variable,alpha=.2)) +
  geom_line(position="identity",aes(group=variable), size=4) + 
  facet_wrap(~ n.bin, nrow=1)

# Let's do this for quantiles
neg.quants = neg.data %>% group_by(q.bin, prev.neg) %>%
  summarize(total=n(), 
            ne.p=sum(neg.type=="ne")/total,
            ne.se = sqrt(ne.p*(1-ne.p)/total),
            ne.not.p=sum(neg.type=="both")/total,
            ne.not.se = sqrt(ne.not.p*(1-ne.not.p)/total),
            not.p=sum(neg.type=="not")/total,
            not.se = sqrt(not.p*(1-not.p)/total))
#test = c('n.bin', 'prev.neg', 'total')
#neg.quants = melt(neg.quants, id=test)
neg.quants = melt(neg.quants %>% select(q.bin, prev.neg, ne.p, not.p, ne.not.p), id=c('q.bin', 'prev.neg'))
neg.quants$variable = factor(neg.quants$variable, levels = c("ne.p", "ne.not.p", "not.p"))
neg.quants$prev.neg = factor(neg.quants$prev.neg, levels = c("ne", "both", "not"))
# Bar plots faceted  by previous form and time bin
ggplot(neg.quants, aes(x=q.bin, y=value, fill=variable)) + 
  geom_bar(stat="identity", position="dodge") + facet_wrap(~prev.neg)
ggplot(neg.quants, aes(x=prev.neg, y=value, fill=variable)) + 
  geom_bar(stat="identity", position="dodge") + facet_wrap(~ q.bin, nrow=1)
# Lines faceted by previous form and time bin
ggplot(neg.quants, aes(x=q.bin, y=value, color=variable)) + 
  #geom_bar(stat="identity", position="dodge", aes(fill=variable,alpha=.1)) +
  geom_line(position="identity",aes(group=variable), size=3) + 
  facet_wrap(~prev.neg)
ggplot(neg.quants, aes(x=prev.neg, y=value, color=variable)) + 
  #geom_bar(stat="identity", position="dodge", aes(fill=variable,alpha=.2)) +
  geom_line(position="identity",aes(group=variable), size=4) + 
  facet_wrap(~ q.bin, nrow=1)

#geom_errorbar(aes(ymin=perf-se, ymax=perf+se)
