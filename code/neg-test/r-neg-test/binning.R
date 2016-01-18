#########################################################
# This file contains utility functions for binning data
#########################################################
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
output.binned = function(df, bin.method, out){
  # Output binned data to file #
  # df : data frame with three columns
  # df$year : the midpoint year of a bin
  # df$value : the number of tokens in a bin of variant 1
  # df$count : the total number of tokens in a bin
  # bin.method : use 'n' for fixed-width bins, 'q' for variable-width bins
  # out : label for output file
  ###
  # Get the number of rows
  n = nrow(df)
  # Create name for file
  name = paste(out, bin.method,sprintf("%02d", n),".data",sep='')
  # Print name to keep track of any errors
  print(name)
  # Write data frame to tab-separated file without row or column names
  write.table(df,file=name,sep="\t",row.names=FALSE,col.names=FALSE) 
}
do.by.years = function(df, n, out) {
  # Bin up to maximum number of bins and output files #
  # df : data frame with same input as do.by.year
  # n : maximum number of fixed-width bins
  # out : label for output files
  ###
  for (i in c(2:n)) {
    data = do.by.year(df, i)
    output.binned(data, "n",out)
  }  
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
do.by.quantiles = function(df,q, out) {
  # Bin up to maximum number of bins and output files #
  # df : data frame with same input as do.by.year
  # q : maximum number of bins
  # out : label for output files
  for (i in c(2:q)) {
    data = do.by.quantile(df, i)
    output.binned(data, "q", out)
  }
}
compare.q.n = function(df, n, max.height) {
  # Plot comparison of fixed-width bins and quantiles
  # df : data frame with year and value columns
  # n : number of bins to compare
  # max.height : the maximum height of the plot, set to multiple of nrows(df)
  ###
  # Get start and end from data frame
  start = min(df$year)
  end = max(df$year)
  # Create fixed- and variable-width bins
  bins = seq(start, end, (end - start)/n)
  quants = as.vector(quantile(df$year, probs=seq(0,1,1/n)))
  # Transform data given fixed- and variable-width bins
  bin.df = df %>% group_by(cut(year,bins, include.lowest=TRUE)) %>% summarize(value=sum(value), count=n())
  quant.df = df %>% group_by(cut(year,quants, include.lowest=TRUE)) %>% summarize(value=sum(value), count=n())
  # Get the mean number of tokens per bin
  data.mean = paste("mu == ", floor(mean(bin.df$count)))
  # Get the standard deviations of tokens per bin for both kinds of binning methods
  bin.sd = paste("sigma[n] ==", floor(sd(bin.df$count)))
  quant.sd = paste("sigma[q] ==", floor(sd(quant.df$count)))
  # Create histogram from both data kinds of bins
  p = ggplot(df) +  geom_histogram(aes(x=year, y=..count.., alpha=.5), color="white", fill="grey30",  breaks = bins, right=TRUE) +  
    geom_histogram(aes(x=year, y=..count.., alpha=.5),colour = "black", fill="white", breaks = quants, right=TRUE) + 
    theme(legend.position="none") +
    ylab("Count") + xlab("Year") +
    annotate("text", label = paste("n = ", n,sep=""), x = end - 100, y = max.height - 200, size = 8) + 
    annotate("text", label = data.mean, x = end - 100, y = max.height - 600, size = 8, parse=T) + 
    annotate("text", label = bin.sd, x = end - 100, y = max.height - 1000, size = 8, parse=T) + 
    annotate("text", label = quant.sd, x = end - 100, y = max.height - 1400, size = 8, parse=T) + 
    theme(text = element_text(size=20)) +
    coord_cartesian(ylim = c(0, max.height))
  return(p)  
}


get.increments  = function(df, n) {
  # Get the increments for the FIT test
  # df : unbinned data
  # n  : maximum number of increments
  ###
  # Create data frame to hold increments
  out.df = NULL
  # For each number of increments
  for (i in 4:n){
    this.data = do.by.quantile(df, i)
    # Get number of bins from data
    r = nrow(this.data)
    # Create vector to hold fitness increments
    Y = rep(0,(r-1))
    # Get variant frequencies from df
    v = this.data$value/this.data$count
    # Get years from df
    t = this.data$year
    # Rescale increments according to definition
    for (i in c(2:r)) { # R indexes from 1 rather than 0
      Y[i-1] = (v[i] - v[i - 1])/sqrt(2*v[i-1]*(1 - v[i-1])*(t[i] - t[i-1]))
    }
    these.increments = cbind(rep(i, r-1), Y)
    out.df = rbind(out.df, these.increments)
  }  
  colnames(out.df) = c("increments", "Yi")
  return(as.data.frame(out.df))
}


do.FIT  = function(df) {
  # Perform the FIT defind in Feder et al. 2013 #
  # df : binned data frame with three columns
  # df$year : the midpoint year of a bin
  # df$value : the number of tokens in a bin of variant 1
  # df$count : the total number of tokens in a bin
  ###
  # Get number of bins from data
  n = nrow(df)
  # Create vector to hold fitness increments
  Y = rep(0,(n-1))
  # Get variant frequencies from df
  v = df$value/df$count
  # Get years from df
  t = df$year
  # Rescale increments according to definition
  for (i in c(2:n)) { # R indexes from 1 rather than 0
    Y[i-1] = (v[i] - v[i - 1])/sqrt(2*v[i-1]*(1 - v[i-1])*(t[i] - t[i-1]))
  }
#   print(Y)
  # Mean fitness increment
  Y.bar = mean(Y)
  # Get t statistic from rescaled fitness increments
  stat = as.numeric(t.test(Y, alternative="greater")$statistic)
  # p value for t-test : H_0 is that the mean of rescaled increments are not greater than zero
  p = t.test(Y, alternative="greater")$p.value
  # Get mean number of tokens per bin
  mu = floor(mean(df$count))
  # Get standard deviation of tokens across bins
  sigma = floor(sd(df$count))
  # p value of shapiro-wilk test : H_0 is that rescaled increments are normally distributed
  w = shapiro.test(Y)$p.value
  # p value of wilcoxon test : H_0 is that the rescaled increments are 
  x = wilcox.test(Y, alternative="greater")$p.value
  # Create vector that leaves room for including data from tsinfer
  values = c(n, 0, 0, 0, Y.bar, stat, p, mu, sigma, w, x)
  return(values)
}
n.table = function(df, n) {
  # Generate table for FIT information up to n fixed-with bins#
  return.df = NULL
  for (i in c(4:n)){
#     print(i)
    this.data = do.by.year(df, i)
    return.df = rbind(return.df, do.FIT(this.data))
  }
  return.df = as.data.frame(return.df)
  colnames(return.df) = c("Bins", "MLs", "MLalpha","LRT.P","Y.bar", "t.FIT","FIT.P", "mu", "sigma", "SW.P")
  return(return.df)
}
q.table = function(df, n) {
  # Generate table for FIT information up to n fixed-with bins#
  return.df = NULL
  for (i in c(4:n)){
    this.data = do.by.quantile(df, i)
    do.FIT(this.data)
    return.df = rbind(return.df, do.FIT(this.data))
  }
  return.df = as.data.frame(return.df)
  colnames(return.df) = c("Bins", "MLs", "MLalpha","LRT.P","Y.bar", "t.FIT","FIT.P", "mu", "sigma", "SW.P", "WX.p")
  return(return.df)
}

# Generate tables for the number of documents per bin
n.doc = function(df, n){
  start = min(df$year)
  end = max(df$year)
  df.bins = seq(start, end, (end - start)/n)
  n.df = df %>% group_by(cut(year,df.bins, include.lowest=TRUE)) %>% summarize(doc=n_distinct(TextName))
  years = as.integer((head(df.bins,-1) + tail(df.bins,-1))/2)
  n.df$year = years
  # reutrn min, max, mean, and sd
  return(c(n, min(n.df$doc), max(n.df$doc), mean(n.df$doc), sd(n.df$doc)))
}
n.docs = function(df, n){
  return.df = NULL
  for (i in c(4:n)){
    return.df = rbind(return.df, n.doc(df, i))
  }
  return.df = as.data.frame(return.df)
  colnames(return.df) = c("Bins", "min", "max", "mean","sd")
  return(return.df)
}
q.doc = function(df, q){
  df.quant = as.vector(quantile(df$year, probs=seq(0,1,1/q)))
  q.df = df %.% dplyr::group_by(cut(year,df.quant, include.lowest=TRUE)) %.% dplyr::summarize(auth=n_distinct(TextName))
  years = as.integer((head(df.quant,-1) + tail(df.quant,-1))/2)
  q.df$year = years
  # reutrn min, max, mean, and sd
  return(c(q, min(q.df$auth), max(q.df$auth), mean(q.df$auth), sd(q.df$auth)))
}
q.docs = function(df, q){
  return.df = NULL
  for (i in c(4:q)){
    return.df = rbind(return.df, q.doc(df, i))
  }
  return.df = as.data.frame(return.df)
  colnames(return.df) = c("Bins", "min", "max", "mean","sd")
  return(return.df)
}
n.auth = function(df, n){
  start = min(df$year)
  end = max(df$year)
  df.bins = seq(start, end, (end - start)/n)
  n.df = df %>% group_by(cut(year,df.bins, include.lowest=TRUE)) %>% summarize(doc=n_distinct(AuthName))
  years = as.integer((head(df.bins,-1) + tail(df.bins,-1))/2)
  n.df$year = years
  # reutrn min, max, mean, and sd
  return(c(n, min(n.df$doc), max(n.df$doc), mean(n.df$doc), sd(n.df$doc)))
}
n.auths = function(df, n){
  return.df = NULL
  for (i in c(4:n)){
    return.df = rbind(return.df, n.auth(df, i))
  }
  return.df = as.data.frame(return.df)
  colnames(return.df) = c("Bins", "min", "max", "mean","sd")
  return(return.df)
}
q.auth = function(df, q){
  df.quant = as.vector(quantile(df$year, probs=seq(0,1,1/q)))
  q.df = df %.% dplyr::group_by(cut(year,df.quant, include.lowest=TRUE)) %.% dplyr::summarize(auth=n_distinct(AuthName))
  years = as.integer((head(df.quant,-1) + tail(df.quant,-1))/2)
  q.df$year = years
  # reutrn min, max, mean, and sd
  return(c(q, min(q.df$auth), max(q.df$auth), mean(q.df$auth), sd(q.df$auth)))
}
q.auths = function(df, q){
  return.df = NULL
  for (i in c(4:q)){
    return.df = rbind(return.df, q.auth(df, i))
  }
  return.df = as.data.frame(return.df)
  colnames(return.df) = c("Bins", "min", "max", "mean","sd")
  return(return.df)
}

