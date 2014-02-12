# STA 250 HW 2 - Method 2: Using R & C in AirlineDelays


# Current issues:
#   * Values are appearing in the wrong places
#   * How are NA values handled? They don't appear in tables
#   * Single file works, but parallel threads crashes R
#       possibly due to use of exit(1) in readRecords.c?
#       What is the PROBLEM-ERROR suggested replacement?
#   * Use GDB for debugging:  R -d GDB

# ssh into hilbert and do everything in there

readrecord: change how NA vlaues

R -d gdb
run
library(airlineDelays)
ls(2)


library(AirlineDelays) # First load the package

test = getDelayTable(path.expand("~/Desktop/STA_250_HW1/1987.csv"), 15L)

path.expand("~/Desktop/STA_250_HW1/testfile.csv")


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

# Should also edit readRecords.h to be 5000 and 10000, not 2000 and 4000
