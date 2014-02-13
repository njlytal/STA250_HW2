# STA 250 HW 2 - Method 2: Using R & C in AirlineDelays


# Current issues:
#   * Values are appearing in the wrong places
#   * How are NA values handled? They don't appear in tables
#   * Single file works, but parallel threads crashes R
#       possibly due to use of exit(1) in readRecords.c?
#       What is the PROBLEM-ERROR suggested replacement?


setwd("~/Desktop/STA_250_HW1")

# First identify the files to subject to clustering
files = list.files(pattern="csv$")
files.old = files[grep('1[0-9]{3}.csv|200[0-7]{1}.csv', files)]
files.new = files[grep('[a-z].csv', files)]

# Properly gives files their entire pathnames
files.old = paste(getwd(),"/",files.old, sep = "")
files.new = paste(getwd(),"/",files.new, sep = "")

filelist = as.list(c(files.old,files.new))

library(AirlineDelays) # First load the package

fieldNums = c(rep(14L,21), rep(44L, 60))

start.t = proc.time()
delays.t = getDelayTable_thread(filelist, fieldNum = fieldNums, numThreads = 81L)
time.t = proc.time() - start.t

# THIS ALL WORKS in shell. And in 51 seconds to boot!

# *** Code Edits ***

# Must edit code to specify PACKAGE="AirlineDelays"
# Otherwise, dll file search will be limited
getDelayTable = edit(getDelayTable)

# EDIT WINDOW
function (filename, fieldNum = getFieldNum(filename)) 
{
    tt = .Call("R_getFileDelayTable", path.expand(filename), 
               TRUE, as.integer(fieldNum), PACKAGE="AirlineDelays")
    tt[tt > 0]
}

getDelayTable_thread = edit(getDelayTable_thread)

# EDIT WINDOW
function (files, fieldNum = sapply(files, getFieldNum), numThreads = 4L) 
{
    tt = .Call("R_threaded_multiReadDelays", files, as.integer(numThreads), 
               TRUE, as.integer(fieldNum), PACKAGE="AirlineDelays")
    tt[tt > 0]
}

