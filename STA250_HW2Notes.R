STA250 - HW 2 Notes

# Game Plan

# Take HW1 code for freq table and modify it to
# work as a function rather than free-form.
# Make a list with all relevant files, then group
# it into proper chunks.
# Run the cluster methods described in class, making
# modifications as necessary

# First test parallelization on a handful of the files

setwd("~/Desktop/STA_250_HW1")
stopCluster(cl) # Stop the cluster & free up memory

# *** TEST RUN with 1987-1994

ff = listfiles(pattern="csv$", full.names=TRUE)

# Here, let "files" be ff

# Assignment 1 in a nutshell (retool your code!)
# files
f = function(files){
    # Call shell/C/Java code to do things. Examples:
    getFreqTable(fs) # where getFreq works
    for(f in fs)
        getFreqTable(f)
}

f(ff) # This should produce our results

# ******
# EXECUTED BELOW

# NOTE: Alter delays to just COLLECT the data, NOT
# to merge them into frequency tables yet! 
delays = function(files){
    con = pipe(paste("cat", paste(files, collapse = " "),
                     "| cut -f 15 -d, | egrep -v '^$' | egrep -v 'ArrDelay'"))
    
    open(con, open="r") # Opens the defined connection to read
    delays = readLines(con) # contains all arrival delays
    close(con) # Closes defined connection
    #removes NA counts, contained in last row of table
    delays = data.frame(table(delays))
    delays = delays[1:nrow(delays)-1,] # All possible delay times
    delays # returns delay table
}

end2 = lapply(files, delays)

cl = makeCluster(4, "FORK")
filesplit = clusterSplit(cl, files)
end = clusterApply(cl, filesplit, delays)
# This produces a list of four elements, where
# each element in a delay table.
stopCluster(cl) # Stop the cluster & free up memory

# ******

# TECHNIQUE REMINDER

test = c("1987.csv","1988.csv","1989.csv","1990.csv")
filesplit[1] # file names for first cluster

# This takes a vector of filenames and collapses it into
# a single argument of filenames with spaces, to be inserted
# into a pipe expression
paste(test, collapse = " ")

# By using paste, we can enter file names into a pipe
# expression
con = pipe(paste("cat", paste(test, collapse = " "), "| cut -f 15 -d, | egrep -v '^$' |
           egrep -v 'ArrDelay'"))

open(con, open="r") # Opens the defined connection to read
delays = readLines(con) # contains all arrival delays
close(con) # Closes defined connection

delays = data.frame(table(delays))
#removes NA counts, contained in last row of table
delays = delays[1:nrow(delays)-1,] 
# All possible delay times

test = mapply(end[1], end[2], FUN=list, SIMPLIFY=FALSE)