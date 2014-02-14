# ****************************************
# STA 250 HW 2 - Method 2
# Using R & C code with AirlineDelays package

# ********* CODE TWEAKS *********
# For the most part, the code utilized is the same as that
# present in the git repository, with the following
# adjustments made inside the files themselves:

# In readrecords.h, expand the range of the tables
# to account for values just beyond the default ranges:

#define MAX_NUM_CHARS 5000
#define MAX_NUM_VALUES 10000

# In getDelayFreqTable.R, adjust the function defintions
# to include a PACKAGE argument to properly search for the
# required .dll files

getDelayTable =
function(filename, fieldNum = getFieldNum(filename))
{
    # Must add PACKAGE="AirlineDelays" for this to function
    
    tt = .Call("R_getFileDelayTable",
               path.expand(filename),
               TRUE, as.integer(fieldNum),
               PACKAGE="AirlineDelays")
    tt[tt > 0]
}
getDelayTable_thread =
    function(files, fieldNum = sapply(files, getFieldNum), numThreads = 4L)
    {
        # Must add PACKAGE="AirlineDelays" for this to function
        tt = .Call("R_threaded_multiReadDelays",
                   files, as.integer(numThreads),
                   TRUE, as.integer(fieldNum),
                   PACKAGE="AirlineDelays")
        tt[tt > 0]
    }


# ************
# Basic setup

# Set working directory to a folder with all CSV files
setwd("~/Desktop/STA_250_HW1")

# First identify the files to read in
# This is divided into old (pre-2008) and new (2008 and on)
# This only gives the initial filenames--need full paths too
files = list.files(pattern="csv$")
files.old = files[grep('1[0-9]{3}.csv|200[0-7]{1}.csv', files)]
files.new = files[grep('[a-z].csv', files)]

# Properly gives files their entire pathnames
files.old = paste(getwd(),"/",files.old, sep = "")
files.new = paste(getwd(),"/",files.new, sep = "")

# Organizes files into a list that the C code can read
filelist = as.list(c(files.old,files.new))

library(AirlineDelays) # First load the package

# This is a vector indicating which column number to read
# for each file. Old & new files use columns 15 and 45
# respectively, but in C we start counting at 0, so we use
# 14 and 44 instead.
fieldNums = c(rep(14L, length(files.old)),
              rep(44L, length(files.new)))


Rprof("/tmp/readSelectedLines.prof") # R profiling

start = proc.time()
              
# This R function calls C code to read entries line by line.
# filelist is a list of all files to read
# fieldNum is a vector of column numbers to read, as above
# numThreads MUST equal the number of files being entered,
# or else the R session will abort.

delays2 = getDelayTable_thread(filelist, fieldNum = fieldNums,
                               numThreads = length(filelist))

names(delays2) # These are the factor levels
as.numeric(delays2) # These are the actual values

# This puts the data frame in the same form as Method 1
# for ease of comparison
delays2.df = data.frame(names(delays2), as.numeric(delays2))

# GETTING RELEVANT VALUES

# All possible delay times
d.time = as.numeric(as.matrix(delays2.df[1]))
# Frequency of each time
d.count = as.numeric(as.matrix(delays2.df[2]))

n = sum(d.count) # Number of entries
sum.prod = sum(d.time*d.count) # sum of products
sum.prod2 = sum((d.time^2)*d.count) # sum of counts by time squared

# Mean of the values
# Takes sum of all products and divides by total # entries
mu = mean((sum.prod)/n)

# Median of the values
# Orders all values and takes middle one
med = sort(rep(d.time,d.count))[n/2]

# Std. dev. of the values
# Uses formula for variance (with n-1 correction),
# then takes square root
sd = sqrt((sum.prod2 - (sum.prod^2)/n)/(n-1))

time = proc.time() - start
Rprof(NULL)
delay.cr.prof = summaryRprof("/tmp/readSelectedLines.prof")$by.self

# Results for C and R code
results.cr = list(time = time, results = c(mean = mu, median = med, sd = sd),
                    system = Sys.info(),  session = sessionInfo(),
                    computer = c(RAM = "16 GB 1600 MHz DDR3",
                                 CPU = "2.6 GHz Intel Core i7",
                                 Software = "OS X 10.8.5 (12F45)"))

# ************************************************************
# *** Plots ***

# With results files from each of the three methods, we can
# plot the data with these commands:
methods = c("Non-Par R","R Clust.", "C Thread")


par(mfrow=c(1,1))
# Time Elapsed
plot(c(results.nonpar$time[3],results.calb$time[3],results.cr$time[3]),
     col = c('red','black','blue'), xlab = "Method Used", ylim = c(0,500),
     ylab = "Time (sec)", main = "Time Elapsed by Method", pch = 15,
     xaxt="n")
axis(1,at=1:3,labels=methods)

legend("topright", c("Non-Parallel R", "R Clusters", "C Threads"),
       col = c('red','black','blue'), pch = 15)


par(mfrow=c(1,3))
# Mean Results
plot(c(results.nonpar$results[1],results.calb$results[1],results.cr$results[1]),
     col = c('red','black','blue'), xlab = "Method Used", ylab = "Mean",
     main = "Mean by Methods", ylim = c(6.4,6.6), pch = 15,
     xaxt="n")
axis(1,at=1:3,labels=methods)

# Median Results
plot(c(results.nonpar$results[2],results.calb$results[2],results.cr$results[2]),
     col = c('red','black','blue'), xlab = "Method Used", ylab = "Median",
     main = "Median by Methods", pch = 15,
     xaxt="n")
axis(1,at=1:3,labels=methods)

# Standard Deviation Results
plot(c(results.nonpar$results[3],results.calb$results[3],results.cr$results[3]),
     col = c('red','black','blue'), xlab = "Method Used", ylab = "Std Dev",
     main = "Standard Deviation by Methods", ylim = c(31,32),
     pch = 15, xaxt="n")
axis(1,at=1:3,labels=methods)

legend("topright", c("Non-Parallel R", "R Clusters", "C Threads"),
       col = c('red','black','blue'), pch = 15)